#FINAL CLUSTERING PROJECT

library(splines)
library(factoextra)
library(dplyr)

load('Clustering/final_data.Rdata')

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


#running PCA#
spiropca <- princomp(cdata[,2:65])
spiropca$sdev
#SD for first 5 components:
# Comp.1     Comp.2     Comp.3 
# 45.2117509 31.2901007 22.3753978 
# Comp.4     Comp.5     Comp.6 
# 17.3299734 13.0471355 

# Plot this 
fviz_nbclust(scale(spiropca$scores), kmeans, method = "wss",k.max=20)
fviz_nbclust(scale(spiropca$scores), kmeans, method = "silhouette",k.max=20)

#Decided to use 4 clusters#

set.seed(12345)
#Creating 4 clusters and adding to PCA
kmean_4 <- kmeans((spiropca$scores),4,nstart=25)
cdata$clust <- kmean_4$cluster

#Creating means and stdev matrices
cmeans <- matrix(colMeans(bet),64,1)
stdev  <- matrix(apply(bet,2,sd),64,1)

cl1 <- matrix(kmean_4$centers[1,],64,1)
bl1 <- cl1 * stdev + cmeans
plot(times,X%*%bl1,ylab="ML",xlab = "Time",type = 'l',lwd=2,col=1,ylim=c(0,100))
cl2 <- matrix(k_means4$centers[2,],40,1)
bl2 <- cl2 * stdev + cmeans
lines(times,X%*%bl2,lwd=2,col=2)
cl3 <- matrix(k_means4$centers[3,],40,1)
bl3 <- cl3 * stdev + cmeans
lines(times,X%*%bl3,lwd=2,col=3)
cl4 <- matrix(k_means4$centers[4,],40,1)
bl4 <- cl4 * stdev + cmeans
lines(times,X%*%bl4,lwd=2,col=4)


#
clusters <- list()
for( ii in 1:4){
    clusters[[ii]] <-  cdata %>% filter(clust == ii)
}

mean(clusters[[1]]$ASTHMA)
mean(clusters[[2]]$ASTHMA)
mean(clusters[[3]]$ASTHMA)
mean(clusters[[4]]$ASTHMA)

mean(clusters[[1]]$AGE)
mean(clusters[[2]]$AGE)
mean(clusters[[3]]$AGE)
mean(clusters[[4]]$AGE)

mean(clusters[[1]]$EVER_SMOKE)
mean(clusters[[2]]$EVER_SMOKE)
mean(clusters[[3]]$EVER_SMOKE)
mean(clusters[[4]]$EVER_SMOKE)

mean(clusters[[1]]$POVERTY_RATIO)
mean(clusters[[2]]$POVERTY_RATIO)
mean(clusters[[3]]$POVERTY_RATIO)
mean(clusters[[4]]$POVERTY_RATIO)

#
####################################################################

# Find the means of each cluster to "Name them"
x <- cbind(colMeans(cdata))
y <- x
for (ii in 1:4) {
    x <- cbind(x,colMeans(clusters[[ii]])-y)
}
kmean_4$centers


#Graphing each cluster on original scale
line_clust1<-apply(clusters[[1]][,2:65],2,mean)
line_clust2<-apply(clusters[[2]][,2:65],2,mean)
line_clust3<-apply(clusters[[3]][,2:65],2,mean)
line_clust4<-apply(clusters[[4]][,2:65],2,mean)
plot(line_clust1, type = 'l', ylim=c(0,90), col=1, lwd=2)
lines(line_clust2, col=2, lwd=2)
lines(line_clust3, col=3, lwd=2)
lines(line_clust4, col=4, lwd=2)
legend("topright", legend=c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4"), col=c(1, 2, 3, 4), lwd = 2, cex=0.8) 

#printing mean values for Cluster 3
line_clust3

#What makes this cluster different from the others?
#They are old poor smokers with asthma

#Looking at Lung Capacity for CLusters 1 2 and 4
line_clust1<-apply(clusters[[1]][,6:65],2,mean)
line_clust2<-apply(clusters[[2]][,6:65],2,mean)
line_clust4<-apply(clusters[[4]][,6:65],2,mean)
plot(line_clust1, type = 'l', ylim=c(0,90), col=1)
lines(line_clust2, col=2)
lines(line_clust4, col=4)
legend("topright", legend=c("Cluster 1", "Cluster 2", "Cluster 4"), col=c(1, 2, 3, 4), lty=1:4, cex=0.8) 

# it looks like Cluster 1 has the least lung capacity.
# Cluster 1 is slightly older, but way poorer.
# Cluster 2 is the youngest.
#Cluster 4 is the most likely to have not ever smoked or to have asthma
# & Cluster 4 is the richest of the 4.
# this makes sense since green Cluster 2 & blue CLuster 4 have the highest lung capacity.

##PART 2##

set.seed(12345)
library(mclust)
clustBIC <-mclustBIC(cdata[,10:20],G=1:20, modelNames='VVV')   # This is model selection
plot(clustBIC)


mod1 <- Mclust(cdata[,10:20], G = 6, modelNames = 'VVV')
summary(mod1,parameters = TRUE)
plot(mod1$parameters$mean[,1])
    
plot(mod1$parameters$mean[,1], type = 'l', ylim=c(0,90), col=1, xlab = 'omg', ylab='time')
lines(mod1$parameters$mean[,2], col=2)
lines(mod1$parameters$mean[,3], col=3)
lines(mod1$parameters$mean[,4], col=4)
lines(mod1$parameters$mean[,5], col=5)
lines(mod1$parameters$mean[,6], col=6)

mod1$classification
mod1$parameters$mean

ggplot(data=df,mapping=aes(x=x,y=y,color=id,shape=class)) + geom_point()
