library(readxl)
library(dplyr)
library(triangle)
library(ks)
library(car) # plot qq plot with confidence interval

df<-read_excel('Analysis_Data2.xlsx',sheet=2)

#### get return rate 1991 to 2006 ####
return <- df %>% filter(Date %in% 1991:2006) %>%
    select(Return_crude,Return_gas,Return_dry)
    
return <- as.numeric(unlist(return))

summary(return)
return.sd=sd(return)
return.mean=mean(return)

# get average drill cost of 2016
cost2016=mean(as.numeric(df %>% filter(Date==2006) %>%
                             select(Cost_crude,cost_gas,Cost_dry)))

# normality test w qq plot
par(mfrow=c(1,2))
hist(return, breaks=25, main='Return Distribution', xlab='Return')
qqPlot(return, main='Normal Q-Q Plot 1991:2006')




#### normal distribution estimate ####
N=10000000
P.normal <- rep(0,N)
P.normal = (rnorm(n = N,mean = return.mean,sd = return.sd) + 1)
for (i in 1:6){
    P.normal = P.normal * (rnorm(n = N,mean = return.mean,sd = return.sd) + 1)
}
for (i in 1:3){
    P.normal = P.normal * (rtriangle(N, -0.22, -0.07, -0.0917)+1)
}
for (i in 1 : 4){
    P.normal = P.normal * (rtriangle(N, 0.02,0.06,0.05)+1)
}

P.normal = P.normal * cost2016

hist(P.normal, breaks=100, main='Estimated 2019 Cost Distribution Normal', xlab='Return')
abline(v = cost2016, col="red", lwd=2)
mtext("Cost 2006", at=cost2016, col="red")

mean(P.normal)
sd(P.normal)
quantile(P.normal, c(.05, .95))
sd(P.normal)/mean(P.normal)
median(P.normal)


#### kernel density function ####
Density.rev <- density(return, bw="SJ-ste")
Density.rev$bw

Est.rev <- rkde(fhat=kde(return, h=Density.rev$bw), n=N)
hist(Est.rev, breaks=100, main='Kernel Density Estimated Return Distribution', xlab='Final Value')
#abline(v = 0, col="red", lwd=2)
#mtext("Initial Inv.", at=0, col="red")

#### simulate kernal from 2006:2012 (6 years)

N=10000000
P=rep(0,N)
P=rkde(fhat=kde(return, h=Density.rev$bw), n=N)+1
for (i in 1 : 6){
    P=P*(rkde(fhat=kde(return, h=Density.rev$bw), n=N)+1)
}
for (i in 1:3){
    P = P * (rtriangle(N, -0.22, -0.07, -0.0917)+1)
}
for (i in 1 : 4){
    P = P * (rtriangle(N, 0.02,0.06,0.05)+1)
}

P=P * cost2016

hist(P, breaks=100, main='Kernel Estimated 2019 Cost Distribution', xlab='Final Value')
abline(v = cost2016, col="red", lwd=2)
mtext("Cost 2006", at=cost2016, col="red")

median(P)
mean(P)
sd(P)
summary(P)
#quantile(P)
quantile(P, c(.05, .95))
sd(P)/mean(P)


#### viz ####
par(mfrow=c(1,2))
hist(Est.rev, breaks=100, main='Kernel Density Return Distribution', xlab='Return')
hist(P, breaks=100, main='Kernel Estimated 2019 Cost', xlab='Cost')
abline(v = cost2016, col="red", lwd=2)
mtext("Cost 2006", at=cost2016, col="red")
# dev.off()


#### mutation ####
simulation.size=10000000

P.n <- cbind(matrix(data=rnorm(n=(6 * simulation.size), mean=return.mean, sd=return.sd)+1,
                    nrow=simulation.size, byrow = TRUE),
             matrix(data=rtriangle(3 * simulation.size, -0.22, -0.07, -0.0917)+1,
                    nrow=simulation.size, byrow = TRUE),
             matrix(data=rtriangle(4 * simulation.size, 0.02,0.06,0.05)+1,
                    nrow=simulation.size, byrow = TRUE))

Simulation.P <- apply(P.n,1,prod)*cost2016

hist(Simulation.P, breaks=100, main='Estimated 2019 Cost Distribution Normal', xlab='Return')
abline(v = cost2016, col="red", lwd=2)
mtext("Cost 2006", at=cost2016, col="red")

mean(Simulation.P)
sd(Simulation.P)
quantile(Simulation.P, c(.05, .95))
sd(Simulation.P)/mean(Simulation.P)
median(Simulation.P)



