## Created: July 7, 2015 (DR)
## Modified: July 10, 2015 (DR - added settings specifically related
##                          to boxplot ranges)
## Modified: 
##
## Purpose: This file stores the parameter settings for all the
## plots in the ParameterPlots file so that rather than setting
## them over and over again, this file can simply be sourced.


## specify maximum fit measure
max.rel.fit <- 11

## set a beta cap for our reasonable beta data frame
reasonable.beta <- 100

## set a gamma cap for our reasonable gamma data frame
reasonable.gamma <- 100

## set a Calculated.s0 cap for our reasonable calc.S0 data frame
reasonable.calc.S0 <- 1000000

## set a second cut for Calculated.S0
second.cut.calc.S0 <- 500000

## set an Extracted.s0 cap for a reasonable ext.S0 data frame
reasonable.ext.S0 <- 1000000

## set an N cap for a reasonable N data frame
reasonable.N <- 500000

################################################################
## BOXPLOT CAP PARMS ###########################################
################################################################

R0.boxplot.cap <- 25
r.boxplot.cap <- 2.5
infper.boxplot.cap <- 75
i0.boxplot.cap <- 0.3
beta.boxplot.cap <- 5
gamma.boxplot.cap <- 5
N.boxplot.cap <- 30000
Extracted.s0.boxplot.cap <- 30000
Calculated.s0.boxplot.cap <- 150000
count.boxplot.cap <- 20000

################################################################
## SONG-SPECIFIC PARAMETER COMPARISON CAPS #####################
################################################################

R0.cutoff <- 20
r.cutoff <- 2.5
infper.cutoff <- 50
gamma.cutoff <- 5
beta.cutoff <- 5
ExtS0.cutoff <- 400000
CalcS0.cutoff <- 400000
##note no Z cutoff because it distributes nicely between 0 and 1
I0.cutoff <- 0.5
N.cutoff <- 250000
count.cutoff <- 20000
