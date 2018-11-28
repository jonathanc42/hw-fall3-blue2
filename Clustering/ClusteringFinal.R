load('final_data.Rdata')

library(splines)
library(readr)
library(factoextra)
library(dplyr)


times <- seq(1,295)/100 # Observations in 1/100th of a second
X <- bs(times,intercept=TRUE,df=60) #create a spline to 
#model the data
betas <- matrix(0,ncol=60,nrow = 6792)
###########################################################
# run a linear regression on each data set
# here I am manipulating my data you I can cluster
###########################################################
for (ii in 1:6792){
    temp <- lm(as.numeric(final_data[ii,6:300])~X-1) #-1 removes the natural intercept
    betas[ii,]  <- coefficients(temp)
}
cdata <- cbind(final_data[,1:5],betas)

#CONVERT EVERTYING TO 'numbers'
cdata$AGE <- as.numeric(cdata$AGE)
cdata$EVER_SMOKE <- as.numeric(cdata$EVER_SMOKE)
cdata$ASTHMA <- as.numeric(cdata$ASTHMA)
cdata$POVERTY_RATIO <- as.numeric(cdata$POVERTY_RATIO)

#### running PCA ####

spiropca <- princomp(cdata[,2:65])
spiropca$sdev

# Comp.1     Comp.2     Comp.3     Comp.4     Comp.5     Comp.6 
# 45.2117498 31.2900771 22.3753781 17.3299544 13.0471238 10.2160214 

#### optimized clustering numbers ####
fviz_nbclust(scale(spiropca$scores), kmeans, method = "wss",k.max=10)
fviz_nbclust(scale(spiropca$scores), kmeans, method = "gap",k.max=10)
fviz_nbclust(scale(spiropca$scores), kmeans, method = "silhouette",k.max=10)

set.seed(12345)
kmean_4 <- kmeans(scale(spiropca$scores),4,nstart=10)
cdata$clust <- kmean_4$cluster

#### Principle Component Analysis and clustering ####

Absenteeism_at_work <- read_delim("Absenteeism_at_work.csv", 
                                  ",", escape_double = FALSE, trim_ws = TRUE)
View(Absenteeism_at_work)

############################################
# First use principal component analysis to 
# find the components of variation
absent <- princomp(Absenteeism_at_work[,6:21])
absent$loadings
absent$center
absent$scores[1,]
##################################################################
# just a reminder how to build the scores on your own
temp <- Absenteeism_at_work[,6:21] - matrix(absent$center,ncol=16,nrow=740,byrow =TRUE)
temp <- as.matrix(temp)%*%absent$loadings
temp[1,]
absent$scores[1,]
##################################################################
#
# Plot this 
fviz_nbclust(scale(absent$scores), kmeans, method = "wss",k.max=20)
fviz_nbclust(scale(absent$scores), kmeans, method = "gap",k.max=10)
fviz_nbclust(scale(absent$scores), kmeans, method = "silhouette",k.max=10)

##################################################################
means_absent <- colMeans(absent$scores)
absent$center
# so we are scaling the scores but not modifying their meen ->
# important to know when we use it to plot
###################################################################
#compare the SD between the princ component analysis
sd_absent    <- apply(absent$scores,2,sd)
sd_absent 
absent$sdev
###################################################################
#There are about 17 clusters!?!
###################################################################
kmean_17 <- kmeans(scale(absent$scores),17,nstart=25)
Absenteeism_at_work$clust <- kmean_17$cluster
###################################################################
#### Try to understand the clusters ####
###################################################################
#
clusters <- list()
for( ii in 1:4){
    clusters[[ii]] <-  cdata %>% filter(clust == ii)
}


#### Find the means of each cluster to "Name them" ####
x <- cbind(colMeans(cdata))
y <- x
for (ii in 1:4) {
    x <- cbind(x,colMeans(clusters[[ii]])-y)
}

######################################################################
# Plot some of the "centers"
######################################################################

line_clust1<-spline(apply(clusters[[1]][,6:65],2,mean))
line_clust2<-spline(apply(clusters[[2]][,6:65],2,mean))
line_clust3<-spline(apply(clusters[[3]][,6:65],2,mean))
line_clust4<-spline(apply(clusters[[4]][,6:65],2,mean))
plot(line_clust1, type = 'l', ylim=c(0,90), col=1, lwd=2)
lines(line_clust2, col=2, lwd=2)
lines(line_clust3, col=3, lwd=2)
lines(line_clust4, col=4, lwd=2)
legend("topright", legend=c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4"), col=c(1, 2, 3, 4), lwd=2, cex=0.8)

sfun <- spline(line_clust1)   #this creates an interpolant of the curve from min(times) to max(times)
plot(sfun, type='l')
integrate(sfun,min(times),max(times)) #this will find the area under the curve



plot(kmean_17$centers[,1],-1*kmean_17$centers[,2],ylab="Distance from Work",xlab = "Work Load",
     axes=F,xlim=c(-2.5,2.5),ylim=c(-1,1),pch = 16,col="Light Blue",cex=2)

abline(v=0,lty=2)
abline(h=0,lty=2)
# Why did I multiply the second one by -1? 

######################################################################
# Plot some of the "centers"
######################################################################
plot(kmean_17$centers[,3],kmean_17$centers[,2],ylab="Tall but close absentee",xlab = "Heavy",
     axes=F,xlim=c(-2.5,2.5),ylim=c(-1,1),pch = 16,col="Light Blue",cex=2)

abline(v=0,lty=2)
abline(h=0,lty=2)
# Why did I multiply the second one by -1? 