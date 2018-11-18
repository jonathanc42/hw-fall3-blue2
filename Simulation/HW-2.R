library(readxl)
library(dplyr)
library(triangle)
library(ks)
library(car) # plot qq plot with confidence interval

# read drilling cost data as df
df<-read_excel('Simulation/Analysis_Data2.xlsx',sheet=2)

#### get cost return 1991 to 2006 to build simulation of drilling cost ####
return <- df %>% filter(Date %in% 1991:2006) %>%
    select(Return_crude,Return_gas,Return_dry)
    
return <- as.numeric(unlist(return))
return.sd=sd(return)
return.mean=mean(return)

# get average drill cost of 2016 by averaging all 3 kinds of drills
# adjust by 1000 dollars
cost2016=1000 * mean(as.numeric(df %>% filter(Date==2006) %>%
                             select(Cost_crude,cost_gas,Cost_dry)))

#### Initial setting ####
# set up the random seed as 42 per The Hitchhiker's Guide to the Galaxy to answer everything
set.seed(42)
# set up simulation.size as 10 Million
simulation.size=10000000

#### Initial cost ####
# simulate the 2019 drilling cost by normal distribution per homework 1
P.n <- cbind(matrix(data=rnorm(n=(6 * simulation.size), mean=return.mean, sd=return.sd)+1,
                    nrow=simulation.size, byrow = TRUE),
             matrix(data=rtriangle(3 * simulation.size, -0.22, -0.07, -0.0917)+1,
                    nrow=simulation.size, byrow = TRUE),
             matrix(data=rtriangle(4 * simulation.size, 0.02,0.06,0.05)+1,
                    nrow=simulation.size, byrow = TRUE))

Cost <- apply(P.n,1,prod)*cost2016

# Lease Costs equals estimated area * price per area
LC <- rnorm(n = simulation.size, mean = 600, sd = 50) * 960

# Seismic Costs
SC <- rnorm(n = simulation.size, mean = 3, sd = 0.35) * 43000

# Completion Costs
CC <- rnorm(n = simulation.size, mean = 390000, sd = 50000) 

# Professional Overhead
# this is assumed constant per well throught the years!
# and will be needed in the future calculaton for the revenue
PO <- rtriangle(simulation.size, 172000, 279500, 215000)

# sum up to get the initial cost for a dry well
IC <- Cost + LC + SC + CC + PO

# abline(v = cost2016, col="red", lwd=2)
# mtext("Cost 2006", at=cost2016, col="red")

# Summary of the initial cost
# we don't include VaR or ES here, since you are plainly spending money
median(IC)
IC.mean <- mean(IC)
IC.mean
IC.sd <- sd(IC)
IC.sd
quantile(IC, c(.05, .95))
sd(IC)/mean(IC)

# normality test w qq plot because it looks normal
qqPlot(IC, main='Normal Q-Q Plot Inicial Cost')

# plot the histogray and add a normal line to check the fit to make it look even normal
h <- hist(IC/1e6, breaks = 100, main='Initial Dry Well Cost 2019',
          col = "lightgray", xlab = "Cost in Million") 
xfit <- seq(min(IC), max(IC), length = 40) 
yfit <- dnorm(xfit, mean = mean(IC), sd = sd(IC)) 
yfit <- yfit * diff(h$mids[1:2]) * length(IC) 
lines(xfit, yfit, col = "blue", lwd = 2)

#### Annual Production ####
# Production Risk
# Initial Production Rate (Rate) with lognormal distribution
IP <- rlnorm(n = simulation.size, meanlog = 6, sdlog = 0.28)

# Decline Rate 
# This is assumed constant per well throught the years!
DR <- runif(n = simulation.size, min = 0.15, max = 0.32)

# Build up correlation between IP and DR 
# Using choleski decomposition to build transform matrix for 0.64 correlation
# first build up the correlation matrix
R <- matrix(data=cbind(1,0.64,0.64,1), nrow=2)
U <- t(chol(R))

# function needed to standardize and destandardize your data
standardize <- function(x){
    x.std = (x - mean(x))/sd(x)
    return(x.std)
}

destandardize <- function(x.std, x){
    x.old = (x.std * sd(x)) + mean(x)
    return(x.old)
}

# Transform and get final IP and DR
# which ever put in the front will keep their shape,
# which ever put in the back will deshape to look like the first more or less
# Don't Panic! We will put uniform first for it will look lnormal, we will keep its good shape
Both <- cbind(standardize(DR), standardize(IP))
SB <- t(U %*% t(Both))
final.SB <- cbind(destandardize(SB[,1], DR), destandardize(SB[,2], IP))

DR.transformed <- final.SB[,1]
IP.transformed <- final.SB[,2]
IP.yearbegin <- IP.transformed

# if you want to see the change after decomposition
# you can also change the position of IP and DR
# you will see how panic the uniform distribution deshaped
# summary(DR)
# summary(IP)
# hist(IP)
# hist(DR)
# summary(DR.transformed)
# summary(IP.transformed)


BothX <- cbind(standardize(IP), standardize(DR))
SBX <- t(U %*% t(BothX))
final.SBX <- cbind(destandardize(SBX[,1], IP), destandardize(SBX[,2], DR))
DR.transformedX <- final.SBX[,1]
IP.transformedX <- final.SBX[,2]

par(mfrow=c(1,4))
hist(DR.transformed, breaks=100, main="Decline Rate (uniform) correct",
     xlab = "DR Rate", xlim = c(0.1, 0.35))
hist(IP.transformed, breaks=100, main="Initial Production (lognormal)", xlab = "Initial Production")
hist(IP.transformedX, breaks=100, main="Decline Rate (uniform) wrong",
     xlab = "DR Rate")
hist(DR.transformedX, breaks=100, main="Initial Production(lognormal) wrong",
     xlab = "Initial Production",xlim = c(0, 1500))

# These code below are commented because you have to rewrite in the for loop
# considering these will change every for a new simulated distribution

# Rate Year End
# IP.yearend <- (1 - DR.final) * IP.transformed

# Yearly Production (Barrels of Oil)
# YP <- 365 * (IP.transformed + IP.yearend ) / 2

#### Revenue Risk ####

# load Oil price dataframe
oilprice <- read_excel('Simulation/Analysis_Data2.xlsx',sheet=1)

# use a for loop to read in the predicted oil triangle discribution parameters
# n = 15 to read in the first required year, make it easy to configure to forecast further revenue
# create matrix OP to save all these years price simulation
n = 15
OP <- matrix(rep(0, n * simulation.size),ncol = n)
for(i in 1:n){
    OP[,i] = rtriangle(n = simulation.size, a = oilprice$Low_price[i],
                       b = oilprice$High_price[i],
                       c = oilprice$Reference[i])
}

# Net Revenue Interest
# constant every year
NRI <- rnorm(n = simulation.size, mean = 0.75, sd = 0.02)

# Operation Expense Rate
# also rewrite in the for loop for yearly change
# OE <- rnorm(n = simulation.size, mean = 2.25,sd = 0.3)

# Severance Taxes
# when calculate the Net revenue, multiple by 1 - ST
ST <- 0.046

#### Simulation ####

# m is number of year you want to simulate
# you can change to align with n, just give you extra freedom
m = 15
# assign IP year begin to the IP transformed, recall to save the uniform
# IP is deshaped but still in good shape
IP.yearbegin <- IP.transformed
# initial NPV with year 0 cost which means IC, the initial cost
NPV = - IC
for(j in 1:m){
    # Operation Expense changeing every year so resample in each loop
    OE <- rnorm(n = simulation.size, mean = 2.25,sd = 0.3)
    # get IP year end every year by DR rate (the rate is assume constant through the year)
    IP.yearend <- (1 - DR.transformed) * IP.yearbegin
    # calculate Year production of oil
    YP <- 365 * (IP.yearbegin + IP.yearend ) / 2
    # calculate the net value
    NPV <- NPV + (OP[,i] * YP * NRI * (1-ST) - OE * YP - PO) / ((1 + 0.1)^i)
    # this year's end is the start of the next year
    IP.yearbegin <- IP.yearend
}

# visualize the value
# in order to present less zeros, using millions, you making crazy money
hist(NPV/1e6, breaks = 100,xlab = 'Value in Million',main='Net Present Value 2033')
abline(v = quantile(NPV, c(.01))/1e6, col="red", lwd=2)
mtext("1% VaR: -4.5M", at=quantile(NPV, c(.01))/1e6, col="red")
# some summary
summary(NPV)
# VaR at 1 %
quantile(NPV, c(.01))
# ES at 1 %
mean(NPV[NPV<quantile(NPV, c(.01))])
# and rest
median(NPV)
mean(NPV)
sd(NPV)
quantile(NPV, c(.05, .95))
sd(NPV)/mean(NPV)
# you may want to save the result because running it takes looooong
write.csv(x = NPV,file = 'NPV.csv')
write.csv(x = IC,file = 'IC.csv')