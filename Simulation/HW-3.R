library(dplyr)
library(truncnorm)
library(purrr)

#### Initial setting ####
# set up the random seed as 42 per The Hitchhiker's Guide to the Galaxy to answer everything
set.seed(42)

# set up simulation.size as 10 Million
simulation.size=10000000


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
producing <- rbinom(simulation.size, nwells, probwet)
hist(producing)

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



#### get individual number of wet well per individual rate ####
n = 30

well_mat <- matrix(rep(NA, n * simulation.size),nrow=simulation.size)

for(i in 1:simulation.size){
    well_mat[i,]=c(rep(1,nwells[i]),rep(0,n-nwells[i]))
}

Well_ind <- matrix(rbinom(n = n * simulation.size,
                          size = 1, 
                          prob = rtruncnorm(n * simulation.size, a=0, b=1, mean=.99, sd=.05) *
                              rtruncnorm(n * simulation.size, a=0, b=1, mean=.8, sd=.1)),
                   nrow = simulation.size)

well_final <- well_mat * Well_ind

well_wet <- apply(well_final,1,sum)

hist(well_wet, main="Number of Wet wells",
     col = "lightgray", xlab = "Number")

hist(nwells - well_wet, main="Number of dry wells",
     col = "lightgray", xlab = "Number")

hist(well_wet /nwells, main="Proportion of Wet Wells",
     col = "lightgray", xlab = "Proportion")
abline(v = var, col="red", lwd=2)
mtext("5% VaR: 0.59", at=var, col="red")

proportion = well_wet /nwells

quantile(proportion, c(.05))
var = quantile(proportion, c(.05))
# CVaR = ES ...
mean(proportion[proportion<quantile(proportion, c(.05))])

# and rest
median(proportion)
mean(proportion)
sd(proportion)
quantile(proportion, c(.05, .95))
sd(proportion)/mean(proportion)

median(well_wet)
median(nwells - well_wet)
