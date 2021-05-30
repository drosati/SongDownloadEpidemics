## Name: SplineFitsRdaOnly
## Author: Dora Rosati
## Created: March 21, 2016
## Modified:
##
## Purpose: This script is designed purely to aggregate each song at the
##          most appropriate timescale before fitting a spline to it
##
## To Run: Set appropriate working directory in R and type
## 'source("SplineFitsTimescaleOnly.R")' OR with:
##      time R CMD BATCH --vanilla SplineFitsTimescaleOnly.R &

################################################################
## READ IN SONG LIST ###########################################
################################################################

# Clear workspace
rm(list=ls())

# set number of warnings printed to be much higher so that they will all
# be printed to the .Rout file at the end
options(nwarnings=100000)

# Load a few necessary packages:
library("rJava")
library("stats4")
library("nloptr")
library("plyr") # needed by dplyr
library("dplyr") # for data manipulation
library("lubridate") # for dealing with dates
library("timeDate") # also for dealing with dates
library("fitsir") # requires bbmle and deSolve
library("bbmle")
library("deSolve")

# Load function to aggregate download data at appropriate time scale
# Note: this script loads plyr, dplyr, mgcv and lubridate
source("timeaggfuns.R")

# Load file with information on songs that you want to extract
# download data for (as a data frame)
# NOTE: THE USER SPECIFIES THIS FILENAME HERE!
songlistdf <- read.delim("top1000GBsongs.tsv",quote="")
names(songlistdf) <- c("count","title","artist","genre")

# Create a column of filenames for each song by removing all spaces
# and punctuation from the song title and artist columns and putting
# them together
songtitle <- with(songlistdf,gsub(" ","",title))
songtitle <- gsub("[[:punct:]]","",songtitle)
artistname <- with(songlistdf,gsub(" ","",artist))
artistname <- gsub("[[:punct:]]","",artistname)
datafilename <- paste(songtitle,artistname,sep="_")

stopifnot(length(datafilename)==length(songlistdf$title))

## check for duplicates in datafilename vector
## this might slow things down a lot, there is probably a better way to do it...
for (i in 2:length(datafilename)) { # start from 2 because otherwise the first song will be compared with itself
  if (is.element(tolower(datafilename[i]),tolower(datafilename[1:(i-1)]))) { # check if the file name exists with any capitalization differences because these are not enough to distinguish
    datafilename[i] <- sprintf("%s2",datafilename[i])
  }
}

## Add escape characters for apostrophes. Previously done so
## that they wouldn't mess up sql queries when they were executed
## (this replaces ' with \'), now important for R manipulation, LaTeX, etc.
songlistdf <- songlistdf %>%
  mutate(artist=gsub("'","\\\\'",songlistdf$artist),
         title=gsub("'","\\\\'",songlistdf$title))

## Also add escape characters for the $ and & characters, since these will
## mess up Latex
songlistdf <- songlistdf %>%
    mutate(artist=gsub("[&]","\\\\&",songlistdf$artist),
           title=gsub("[&]","\\\\&",songlistdf$title))
songlistdf <- songlistdf %>%
    mutate(artist=gsub("[$]","\\\\$",songlistdf$artist),
           title=gsub("[$]","\\\\$",songlistdf$title))

## and finally fix the titles of three genres that will create problems for latex
## switch R 'n' B to R&B
levels(songlistdf$genre)[levels(songlistdf$genre)=="Soul/R 'n' B/Funk"] <- "Soul/R\\&B/Funk"

## fix the & in Indipop & Remix
levels(songlistdf$genre)[levels(songlistdf$genre)=="Indipop & Remix"] <- "Indipop \\& Remix"

## fix the _ in rock_and_pop
levels(songlistdf$genre)[levels(songlistdf$genre)=="rock_and_pop"] <- "rock\\_and\\_pop"


################################################################
## SPECIFY QUERY PARAMETERS ####################################
################################################################

# specify whether you want data for a set of specific countries
# or the whole world
some.countries <- TRUE
worldwide <- FALSE
stopifnot(some.countries!=worldwide)

# if specific countries, load a list of countries that we want to look at
if (some.countries) {
  countrylist <- as.vector(t(read.table("countrylist.tsv",header=TRUE)))
}

## decide whether we want to aggregate missing beginnings
agg.missing <- TRUE

################################################################
## LOAD DATA AND FIT SIR MODEL #################################
################################################################

## create a vector to store best timescale to aggregate each song at
timescale.save <- rep(0,nrow(songlistdf))

# Code for data if we are looking at downloads in a specific
# list of countries
if (some.countries) {
    for (icountry in 1:length(countrylist)) {
        country <- countrylist[icountry]
        for (isong in 1:nrow(songlistdf)) {
            ## set all variables for each song to be used in plots
            songtitle <- songlistdf[isong,"title"]
            artistname <- songlistdf[isong,"artist"]
            genre <- songlistdf[isong,"genre"]
            totaldownloads <- songlistdf[isong,"count"]
            songdatafilename <- datafilename[isong]

            ## load dataframe for download counts of song we want to examine
            ## and set appropriate names for aggfun1() to handle
            song1 <- read.delim(sprintf("Songtsv/%s%s.tsv",songdatafilename,country)) %>%
                setNames(c("count","datetime")) # note we're calling this datetime so that aggfun1 can find it

            ## nervously adding this line...
            song1$datetime <- as.POSIXct(as.character(song1$datetime),tz="GMT")

#####################################################################################################
            ## FIGURE OUT WHAT TIMESCALE THIS DATA SET SHOULD BE AGGREGATED AT
            ## if total download count is less than 10 000, start by aggregating at monthly level
            ## create vector of times that will be used
            if (totaldownloads<10000) {
                timescale.vec <- c("month","week","3.5 day","day")
                ## initialize a value for the max.point and a counter
                max.point <- 1
                itime <- 1
                while (max.point<11 && itime<5) {
                    timescale <- timescale.vec[itime]
                    ## aggregate song download data
                    song <- aggfun1(song1,breaks=timescale,back.conv=ymd)
                    ## check where max count occurs
                    max.point <- which.max(song$count)
                    ## update counter
                    itime <- itime+1
                }

                if (agg.missing) {
                  ## next, if the max.point is still not at least 10 points in,
                  ## aggregate only the beginning of the time series at a finer timescale
                  if (max.point<11) {
                    ## check where the maximum point falls in the download time series
                    max.point <- which.max(song$count)
                    ## make note of the date at which this max.point occurs
                    peak.date <- as.Date(song$date[max.point]) # must coerce to a date, otherwise
                                        # it's a factor or something and doesn't work
                    
                    ## use this date to create a data frame containing downloads up to the
                    ## peak point and a data frame containing downloads after this point
                    song1beginning <- subset(song1,round_date(as.Date(song1$date),unit="day")<=peak.date)
                    ## for song1beginning, song1$date was coerced to be a date rather than a list of factors (I
                    ## think) and then rounded to be only days so that it could be compared to the date at
                    ## which the peak number of downloads occurred
                    song1end <- subset(song1,round_date(as.Date(song1$date),unit="day")>peak.date)
                    song2end <- aggfun1(song1end,breaks="day",back.conv=ymd)
                    
                    ## now better aggregate beginning of song only
                    
                    ## now aggregate at progressively smaller timescales the same as was
                    ## done in my PlotAndParms scripts by checking the max.point each time
                    ## create a finer timescale vector and a counter itime
                    timescale.vec <- c("12 hour","6 hour","3 hour","1 hour","30 min","15 min")
                    itime <- 1
                    ## add this in case we don't enter the while loop
                    song2beginning <- song1beginning %>%
                      setNames(c("count","date"))
                    ## from looking at some time series with missing beginnings, we will set the number of points
                    ## required before the max.point to be higher now
                    while (max.point<25 && itime<7) {
                      timescale <- timescale.vec[itime]
                      ## aggregate song download data
                      song2beginning <- aggfun1(song1beginning,breaks=timescale,back.conv=ymd_hms) #note that now back.conv has to be ymd_hms since timescale is <1 day
                      ## check where max downloads occurs
                      max.point <- which.max(song2beginning$count)
                      ## update counter
                      itime <- itime+1
                    }
                    
                    ## put the more finely aggregated beginning of the time series back
                    ## together with the rest of the time series
                    song2 <- rbind(song2beginning,song2end)
                    
                    ## save timescale to timescale.save vector
                    timescale.save[isong] <- timescale
                    
                    ## fit SIR model
                    ## fit SIR to song data, first changing units
                    song3 <- mutate(song2,
                                    tvec=as.numeric(date)/86400)  ## units are now in *days* (added Aug. 3)
                    
                  } else { #if max.point<11
                    ## save timescale to timescale.save vector
                    timescale.save[isong] <- timescale
                    
                  } #else, i.e. max.point>=11
                  
                } else { ##if agg.missing
                  ## save timescale to timescale.save vector
                  timescale.save[isong] <- timescale
                  
                } ##else, i.e. not aggregating for missing beginnings
                    
              }# <10000 downloads
            
            ## now do the same thing for songs with >10 000 downloads, except start aggregating these at 1 week
            ## (too many downloads to bother with monthly)
            if (totaldownloads>=10000) {
                timescale.vec <- c("week","3.5 day","day")
                ## initialize a value for the max.point and a counter
                max.point <- 1
                itime <- 1
                while (max.point<11 && itime<4) {
                    timescale <- timescale.vec[itime]
                    ## aggregate song download data
                    song <- aggfun1(song1,breaks=timescale,back.conv=ymd)
                    ## check where max count occurs
                    max.point <- which.max(song$count)
                    ## update counter
                    itime <- itime+1
                }

                ## if we want to aggregate missing beginnings
                if (agg.missing) {
                ##finer aggregation goes here
                  if (max.point<11) {
                    ## check where the maximum point falls in the download time series
                    max.point <- which.max(song$count)
                    ## make note of the date at which this max.point occurs
                    peak.date <- as.Date(song$date[max.point]) # must coerce to a date, otherwise
                                        # it's a factor or something and doesn't work
                    
                    ## use this date to create a data frame containing downloads up to the
                    ## peak point and a data frame containing downloads after this point
                    song1beginning <- subset(song1,round_date(as.Date(song1$date),unit="day")<=peak.date)
                    ## for song1beginning, song1$date was coerced to be a date rather than a list of factors (I
                    ## think) and then rounded to be only days so that it could be compared to the date at
                    ## which the peak number of downloads occurred
                    song1end <- subset(song1,round_date(as.Date(song1$date),unit="day")>peak.date)
                    song2end <- aggfun1(song1end,breaks="day",back.conv=ymd)
                    
                    ## now better aggregate beginning of song only
                    
                    ## now aggregate at progressively smaller timescales the same as was
                    ## done in my PlotAndParms scripts by checking the max.point each time
                    ## create a finer timescale vector and a counter itime
                    timescale.vec <- c("12 hour","6 hour","3 hour","1 hour","30 min","15 min")
                    itime <- 1
                    ## add this in case we don't enter the while loop
                    song2beginning <- song1beginning %>%
                      setNames(c("count","date"))
                    ## from looking at some time series with missing beginnings, we will set the number of points
                    ## required before the max.point to be higher now
                    while (max.point<25 && itime<7) {
                      timescale <- timescale.vec[itime]
                      ## aggregate song download data
                      song2beginning <- aggfun1(song1beginning,breaks=timescale,back.conv=ymd_hms) #note that now back.conv has to be ymd_hms since timescale is <1 day
                      ## check where max downloads occurs
                      max.point <- which.max(song2beginning$count)
                      ## update counter
                      itime <- itime+1
                    }
                    
                    ## put the more finely aggregated beginning of the time series back
                    ## together with the rest of the time series
                    song2 <- rbind(song2beginning,song2end)
                    
                    ## save timescale to timescale.save vector
                    timescale.save[isong] <- timescale
                    
                    ## fit SIR to song data, first changing units
                    song3 <- mutate(song2,
                                    tvec=as.numeric(date)/86400)  ## units are now in *days* (added Aug. 3)
                    
                  } else { #if max.point<11
                    ## save timescale to timescale.save vector
                    timescale.save[isong] <- timescale
                    
                  }
                } else { ##if agg.missing
                  ## save timescale to timescale.save vector
                  timescale.save[isong] <- timescale
                  
                } ##else, i.e., not aggregating missing beginnings
                
              } # >=10000 downloads
            
          } #for isong
        
        ## save song.splines and timescale aggregation vector to an .rda file
        filename <- paste(sprintf("TopSongs%sSplineTimescales.RData",country))
        save(timescale.save,file=filename)
    } #for icountry
} #some.countries
