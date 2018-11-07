library(readxl)
library(dplyr)
library(triangle)
library(ks)
library(car) # plot qq plot with confidence interval

# read in data
df<-read_excel('Simulation/Analysis_Data2.xlsx',sheet=2)

#### get return 1991 to 2006 ####
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
par(mfrow=c(1,1))



#### normal distribution estimate ####
set.seed(42)
simulation.size=10000000

P.n <- cbind(matrix(data=rnorm(n=(6 * simulation.size), mean=return.mean, sd=return.sd)+1,
                    nrow=simulation.size, byrow = TRUE),
             matrix(data=rtriangle(3 * simulation.size, -0.22, -0.07, -0.0917)+1,
                    nrow=simulation.size, byrow = TRUE),
             matrix(data=rtriangle(4 * simulation.size, 0.02,0.06,0.05)+1,
                    nrow=simulation.size, byrow = TRUE))

Simulation.normal <- apply(P.n,1,prod)*cost2016

hist(Simulation.normal, breaks=100, main='Estimated 2019 Cost Distribution Normal', xlab='Cost')
abline(v = cost2016, col="red", lwd=2)
mtext("Cost 2006", at=cost2016, col="red")

mean(Simulation.normal)
sd(Simulation.normal)
quantile(Simulation.normal, c(.05, .95))
sd(Simulation.normal)/mean(Simulation.normal)
median(Simulation.normal)

#### kernel density ####
density.return <- density(return, bw="SJ-ste")

est.return <- rkde(fhat=kde(return, h=density.return$bw), n=simulation.size)
hist(est.return, breaks=100, main='Kernel Density Estimated Return Distribution', xlab='Final Value')
#abline(v = 0, col="red", lwd=2)
#mtext("Initial Inv.", at=0, col="red")

#### Simulation w kernal density function
set.seed(42)
simulation.size=10000000

P.kernel <- cbind(matrix(data=rkde(fhat=kde(return, h=density.return$bw), n=6 * simulation.size)+1,
                    nrow=simulation.size, byrow = TRUE),
             matrix(data=rtriangle(3 * simulation.size, -0.22, -0.07, -0.0917)+1,
                    nrow=simulation.size, byrow = TRUE),
             matrix(data=rtriangle(4 * simulation.size, 0.02,0.06,0.05)+1,
                    nrow=simulation.size, byrow = TRUE))

Simulation.kernel <- apply(P.kernel,1,prod)*cost2016

hist(Simulation.kernel, breaks=100, main='Kernel Estimated 2019 Cost Distribution', xlab='Cost')
abline(v = cost2016, col="red", lwd=2)
mtext("Cost 2006", at=cost2016, col="red")

median(Simulation.kernel)
mean(Simulation.kernel)
sd(Simulation.kernel)
summary(Simulation.kernel)
#quantile(P)
quantile(Simulation.kernel, c(.05, .95))
sd(Simulation.kernel)/mean(Simulation.kernel)


#### viz ####
par(mfrow=c(1,2))
hist(est.return, breaks=100, main='Kernel Density Return Distribution', xlab='Return')
hist(Simulation.kernel, breaks=100, main='Kernel Estimated 2019 Cost', xlab='Cost')
abline(v = cost2016, col="red", lwd=2)
mtext("Cost 2006", at=cost2016, col="red")
# dev.off()
