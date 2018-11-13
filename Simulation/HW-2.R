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
return.sd=sd(return)
return.mean=mean(return)

# get average drill cost of 2016
cost2016=mean(as.numeric(df %>% filter(Date==2006) %>%
                             select(Cost_crude,cost_gas,Cost_dry)))

#### Initial setting ####
set.seed(42)
# simulation.size=10000000
simulation.size=10000000

#### Initial cost ####
# 2019 drilling cost

P.n <- cbind(matrix(data=rnorm(n=(6 * simulation.size), mean=return.mean, sd=return.sd)+1,
                    nrow=simulation.size, byrow = TRUE),
             matrix(data=rtriangle(3 * simulation.size, -0.22, -0.07, -0.0917)+1,
                    nrow=simulation.size, byrow = TRUE),
             matrix(data=rtriangle(4 * simulation.size, 0.02,0.06,0.05)+1,
                    nrow=simulation.size, byrow = TRUE))

Cost <- apply(P.n,1,prod)*cost2016

# Lease Costs
LC <- rnorm(n = simulation.size, mean = 600, sd = 50) * 960

# Seismic Costs
SC <- rnorm(n = simulation.size, mean = 3, sd = 0.35) * 43000

# Completion Costs
CC <- rnorm(n = simulation.size, mean = 390000, sd = 50000) 

# Professional Overhead
# this is constant
PO <- rtriangle(simulation.size, 172000, 279500, 215000)

IC <- Cost + LC + SC + CC + PO

# abline(v = cost2016, col="red", lwd=2)
# mtext("Cost 2006", at=cost2016, col="red")

# Summary
median(IC)
IC.mean <- mean(IC)
IC.sd <- sd(IC)
quantile(IC, c(.05, .95))
sd(IC)/mean(IC)

# normality test w qq plot
qqPlot(IC, main='Normal Q-Q Plot Inicial Cost')

# plot the histogray and add a normal line to check the fit
h <- hist(IC, breaks = 100, main='Initial Dry Well Cost 2019',
          col = "lightgray", xlab = "Cost") 
xfit <- seq(min(IC), max(IC), length = 40) 
yfit <- dnorm(xfit, mean = mean(IC), sd = sd(IC)) 
yfit <- yfit * diff(h$mids[1:2]) * length(IC) 
lines(xfit, yfit, col = "blue", lwd = 2)

#### Annual Production ####
# Production Risk
# Initial Production Rate (Rate)
IP <- rlnorm(n = simulation.size, meanlog = 6, sdlog = 0.28)

# Decline Rate 
# This is stable no need draw sample in future time
DR <- runif(n = simulation.size, min = 0.15, max = 0.32)

# Build up correlation between IP and DR 
# Using choleski decomposition to build transform matrix for 0.64 correlation
R <- matrix(data=cbind(1,0.64,0.64,1), nrow=2)
U <- t(chol(R))

# standardize and destandardize function
standardize <- function(x){
    x.std = (x - mean(x))/sd(x)
    return(x.std)
}

destandardize <- function(x.std, x){
    x.old = (x.std * sd(x)) + mean(x)
    return(x.old)
}

# Transform and get final IP and DR
Both <- cbind(standardize(DR), standardize(IP))
SB <- t(U %*% t(Both))
final.SB <- cbind(destandardize(SB[,1], DR), destandardize(SB[,2], IP))

# summary(DR)
# summary(IP)
# hist(IP)
# hist(DR)

# summary(DR.transformed)
# summary(IP.transformed)
# hist(DR.transformed)
# hist(IP.transformed)

DR.transformed <- final.SB[,1]
IP.transformed <- final.SB[,2]
IP.yearbegin <- IP.transformed

# Rate Year End
# IP.yearend <- (1 - DR.final) * IP.transformed

# Yearly Production (Barrels of Oil)
# YP <- 365 * (IP.transformed + IP.yearend ) / 2

#### Revenue Risk ####

# Oil price
oilprice <- read_excel('Simulation/Analysis_Data2.xlsx',sheet=1)

n = 15
OP <- matrix(rep(0, n * simulation.size),ncol = n)
for(i in 1:n){
    OP[,i] = rtriangle(n = simulation.size, a = oilprice$Low_price[i],
                       b = oilprice$High_price[i],
                       c = oilprice$Reference[i])
}

# Net Revenue Interest
NRI <- rnorm(n = simulation.size, mean = 0.75, sd = 0.02)

# Operation Expense Rate
# OE <- rnorm(n = simulation.size, mean = 2.25,sd = 0.3)

# Severance Taxes
ST <- 0.046

#### Simulation ####

m = 15
IP.yearbegin <- IP.transformed
NPV = - IC
for(j in 1:m){
    OE <- rnorm(n = simulation.size, mean = 2.25,sd = 0.3)
    IP.yearend <- (1 - DR.transformed) * IP.yearbegin
    YP <- 365 * (IP.yearbegin + IP.yearend ) / 2
    NPV <- NPV + (OP[,i] * YP * NRI * (1-ST) - OE * YP - PO) / ((1 + 0.1)^i)
    IP.yearbegin <- IP.yearend
}

hist(NPV/1e6, xlab = 'Million',main='Net Present Value 2033')

summary(NPV)

# VaR
quantile(NPV, c(.001))

# ES
mean(NPV[NPV<quantile(NPV, c(.001))])

# qqPlot(NPV, main='Normal Q-Q Plot NPV 2033')

# i
# (OP[,i] * YP * NRI * ST - OE * YP - PO) / (1 + 0.1)^i
# YP
# OP[i]
