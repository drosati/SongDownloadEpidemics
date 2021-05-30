## Title: StochasticSIRSample
## Created: April 15-20, 2016 (DR & BB)
## Modified:
##
## Purpose: This script demonstrates how to run one stochastic SIR simulation,
##          fit a spline and the SIR model to that simulation, and plot the simulation
##          and both fits on the same plot. It is very rough, simply demonstrating the
##          basic steps.


## The Kermack-McKendrick SIR model is defined as
# dS/dt = -beta*N*S
# dI/dt = beta*N*S - gamma*I
# dR/dt = gamma*I
#
# This model consists of two reactions with the following per capita rates,
# transmission: beta
# recovery:     gamma

## load packages
library(GillespieSSA)
library("bbmle")
library("fitsir")
library("rJava")
library("stats4")
library("nloptr")
library("plyr") # needed by dplyr
library("dplyr") # for data manipulation
library("lubridate") # for dealing with dates
library("timeDate") # also for dealing with dates
library("deSolve")
library("splines")

# Define parameters
parms <- c(beta=.001, gamma=.100)

# Define system
x0 <- c(S=500, I=1, R=0)                      # Initial state vector

nu <- matrix(c(-1,0,1,-1,0,1),nrow=3,byrow=T) # State-change matrix

a  <- c("beta*S*I", "gamma*I")                # Propensity vector

tf <- 100                                     # Final time

simName <- "Kermack-McKendrick SIR"

nf <- layout(matrix(c(1,2,3,4),ncol=2, byrow=T)) # this is likely unnecessary

# Direct method
set.seed(8)

out <- ssa(x0,a,nu,parms,tf,method="D",simName,verbose=TRUE,consoleInterval=Inf)

## take infection data -- we only actually care about this so we're not keeping
## any of the other data right now
stoch.sir.data <- as.data.frame(out$data,row.names=NA)
names(stoch.sir.data)[1] <- "date"
stoch.sir.data <- stoch.sir.data[c("date","I")]
last.index <- which(stoch.sir.data$I==0)[1] #determine when the infectives are gone
stoch.sir.data <- stoch.sir.data[1:(last.index-1),] #only look at data before this for fitting so that spline will be kept positive by using log of infectives

## create spline fit and plot
p1 <- lm(log(I)~ns(date,df=3),data=stoch.sir.data)
sp <- exp(fitted(p1))
with(stoch.sir.data,plot(date,I,type="l"))
with(stoch.sir.data,lines(date,exp(fitted(p1)),col="blue",lwd=3))

fitsir.data <- stoch.sir.data[c("date","I")]
names(fitsir.data)[2] <- "count"
m1 <- fitsir(data=fitsir.data)
tp <- trans.pars(coef(m1)) # extracted parameters from sir fit
ss <- SIR.detsim(seq_along(stoch.sir.data$date),tp)
with(stoch.sir.data,lines(date,ss,col="green"))

## the plot method from GillespieSSA package
#ssa.plot(out)
