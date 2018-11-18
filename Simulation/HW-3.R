library(msm)
library(readxl)
library(dplyr)
library(truncnorm)
library(purrr)

#### Initial setting ####
# set up the random seed as 42 per The Hitchhiker's Guide to the Galaxy to answer everything
set.seed(42)

# set up simulation.size as 10 Million
simulation.size=10000000

p.h <- rtnorm(n = simulation.size, mean=0.99, sd=0.05, lower=0, upper=1)
p.r <- rtnorm(n = simulation.size, mean=0.8, sd=0.1, lower=0, upper=1)

hist(p.h, breaks = 100)
hist(p.r, breaks = 100)

length(p.h)
length(p.r)

p <- p.h * p.r



#simulate the probabability that hydrocarbons are present
#using truncated normal distribution
hydrocarbon <- rtruncnorm(simulation.size, a=0, b=1, mean=.99, sd=.05)

#simulate the probability that there is a reservoir 
#using trancated normal distribution
reservoir <- rtruncnorm(simulation.size, a=0, b=1, mean=.8, sd=.1)

#overall probability of producing well
probwet <- (hydrocarbon * reservoir)

#plot histograms of the distributions
h <- hist(hydrocarbon, main='Probability of Hydrocarbons',
          col = "lightgray", xlab = "Probability") 

r <- hist(reservoir, main='Probability of Reservoir',
          col = "lightgray", xlab = "Probability")

pw <- hist(probwet, main='Probability of Wet Well',
           col = "lightgray", xlab="Probability")

#Simulates the number of planned wells
nwells <- ceiling(runif(simulation.size, min=9, max=30))
hist(nwells)
summary(nwells)

#Simulates the number of wet wells and dry wells
producing <- rbinom(simulation.size, nwells, p)
hist(producing)

producing2 <- rbinom(simulation.size, nwells, 0.7616);hist(producing2, main='a fixed test')

mean(producing)
dry <- nwells - producing
mean(dry)

mean(producing + dry)
mean(nwells)

propwet <- (producing/(producing+dry))


#histogram of the number of wet wells, dry wells, and proportion of wet
p <- hist(producing, main="Number of Wet wells",
          col = "lightgray", xlab = "Number")

d <- hist(dry, main="Number of Dry Wells", breaks=10,
          col = "lightgray", xlab = "Number")

wetratio <- hist(propwet, main = "Proportion of Wet Wells",
                 col = "lightgray", xlab="Proportion")

n=10
m=5
c(rep(1,n),rep(0,n-m))


a=c(1,2,3,4,5)
a
a.apply(c(rep(1,a),rep(0,n-a)))

matrix(data=rnorm(n=(6 * 100), mean=1, sd=2)+1,
       nrow=100, byrow = TRUE) 


nwells

matrix(data=rbinom((n* simulation.size),1,p),
       nrow=simulation.size, byrow=TRUE)

a= matrix(c(1,2,3,4),nrow=2)
b= matrix(c(1,1,0,0),nrow=2)

test = matrix(rep(0,n*simulation.size),
              nrow=simulation.size, byrow=TRUE)

i = 1
a[i,] <- c(666,666)
for(i = 1:5){
    print(i)
}
