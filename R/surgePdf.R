## Name: SplineFitsPdf
## Author: Dora Rosati
## Created: March 21, 2016
## Modified: July 25, 2016 (DR - changed from smooth.spline to other method
##                          recommended by Ben; this has correct number of
##                          degrees of freedom)
## Modified: July 26, 2016 (DR - changed settings so that script can use
##                          either timescales made specifically for spline
##                          fitting, or those made for fitsir; script now
##                          saves spline fits)
## Modified: 
##
## Purpose: This script is designed to fit splines to download time series
##          for songs, then plot the splines and download time series and
##          save these as pdfs. It should ideally be merged with my
##          PlotAndParmsPdf script as an option at some point in the future.
##
## Note: This replaces SplineFitsPdfSmoothSpline.R, which was using the
##       smooth.spline() method.
##
## To Run: Set appropriate working directory in R and type
## 'source("SplineFitsPdf.R")' OR with:
##      time R CMD BATCH --vanilla SplineFitsPdf.R &

################################################################
## READ IN SONG DATA ###########################################
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
library("splines")

# Load function to aggregate download data at appropriate time scale
# Note: this script loads plyr, dplyr, mgcv and lubridate
source("timeaggfuns.R")

## Load file of songs to exclude eventually
songstoexcludedf <- read.delim("../Data/ExcludedSongsGB.tsv",quote="")
names(songstoexcludedf) <- "title"

# Load file with information on songs that you want to extract
# download data for (as a data frame)
# NOTE: THE USER SPECIFIES THIS FILENAME HERE!
songlistdf <- read.delim("../Data/top1000GBsongs.tsv",quote="")
## FIXME: using temporary song list to debug
#songlistdf <- read.delim("songs.tsv",quote="")
## names(songlistdf)[1:4] <- c("count","title","artist","genre")

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
  countrylist <- as.vector(t(read.table("../Data/countrylist.tsv",header=TRUE)))
}

## some timescale options
ymd.timescale <- c("month","week","3.5 day","day")
hms.timescale <- c("12 hour","6 hour","3 hour","1 hour","30 min","15 min")

## set various parameter limits by sourcing a file with settings -- need max.rel.fit in this case
## can reference a different file for (example) users with 5-5000 downloads
source("ParameterSettings.R")

## decide whether using timescales made specifically for splines or not
spline.timescales <- FALSE

##dolphin remove
b <- 0

################################################################
## FIT SPLINES AND CREATE PLOTS ################################
################################################################

## create vectors to hold information about goodness of fit
rel.fit.goodness.vec <- rep(0,nrow(songlistdf))
abs.fit.goodness.vec <- rep(0,nrow(songlistdf))

# create empty list to store the fits for all the songs in case they're needed at some point
spline.fits <- list()

# Code to plot data if we are looking at downloads in a specific
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

            ## FIXME: debugging
            print(songtitle)
            
            ## load .RData file
            if (spline.timescales)
              {
                rdafilename <- paste(sprintf("../Data/TopSongs%sSplineTimescales.RData",country))
              } else {
                rdafilename <- paste(sprintf("../Data/TopSongs%sFitsAndTimescales_NoAccents.RData",country))
              }
            load(rdafilename)
            
            ## set timescale to aggregate at
            timescale <- timescale.save[isong]

            ## create pdfs
            pdf(sprintf("Top1000GBSongSplinePlots/%s%s.pdf",songdatafilename,country))
      
            ## load dataframe for download counts of song we want to examine
            ## and set appropriate names for aggfun1() to handle
            song1 <- read.delim(sprintf("../Data/Top1000GBSongstsv/%s%s.tsv",songdatafilename,country)) %>%
                setNames(c("count","datetime")) # note we're calling this datetime so that aggfun1 can find it

            ## hesitantly adding this here as well
            song1$datetime <- as.POSIXct(as.character(song1$datetime),tz="GMT")
            
            ## aggregate song at the best timescale
            if (is.element(timescale,ymd.timescale)) {
                song <- aggfun1(song1,timescale,ymd)

                ## plot resulting curve
                titlestring <- paste(sprintf("Downloads in %s of '%s' ", country, songtitle), "\n", sprintf("by %s (%s)", artistname, genre), "\n", sprintf("(%s intervals)",timescale))

                ## Plot aggregated downloads
                with(song,plot(date,count,type="l",main=titlestring,xlim=c(min(date),max(date)),xlab="Date",ylab="Downloads",col="grey"))
                with(song,points(date,count,pch=21,bg="black",cex=0.75))

                ## create fit of data and plot this as well
                p1 <- lm(log(count)~ns(date,df=3),data=song)
                sp <- exp(fitted(p1))
                with(song,lines(date,exp(fitted(p1)),col="forestgreen",lwd=3))

                ## save fitted spline to spline.fits
                spline.fits[isong] <- p1

                ## we will soon want to calculate proportional sum of squares as a measure of how `good' the fitted model is
                cc <- song$count

              } else {

                if (is.element(timescale,hms.timescale)) { # in this case we have to reaggregate the beginning... likely not the best way to do this
                  song <- aggfun1(song1,breaks="day",back.conv=ymd)
                  
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
                  
                  ## plot resulting curve
                  titlestring <- paste(sprintf("Downloads in %s of '%s' ", country, songtitle), "\n", sprintf("by %s (%s)", artistname, genre), "\n", sprintf("(%s intervals for beg.)",timescale))
                  
                  ## Plot aggregated downloads
                  with(song2,plot(date,count,type="l",main=titlestring,xlim=c(min(date),max(date)),xlab="Date",ylab="Downloads",col="grey"))
                  with(song2,points(date,count,pch=21,bg="black",cex=0.75))

                  ## create fit of data and plot this as well
                  p1 <- lm(log(count)~ns(date,df=3),data=song2)
                  sp <- exp(fitted(p1))
                  with(song2,lines(date,exp(fitted(p1)),col="forestgreen",lwd=3))

                  ## save fitted spline to spline.fits
                  spline.fits[isong] <- p1

                  ## we will soon want to calculate proportional sum of squares as a measure of how `good' the fitted model is
                  cc <- song2$count

                } else {

                  warning("Timescale error")

                }
              }

            ## calculate how `good' this fit is, then save it in a vector with everything else
            ## first relative
            rel.sum.sq <- mean((1-sp/cc)^2)
            ## then root of this
            root.mean.sq <- sqrt(rel.sum.sq)
            ## also absolute
            abs.sum.sq <- mean((cc-sp)^2)            
            
            rel.fit.goodness.vec[isong] <- root.mean.sq
            abs.fit.goodness.vec[isong] <- abs.sum.sq

            ## add parameters to the plot as a legend
            relsongfit <- root.mean.sq
            abssongfit <- abs.sum.sq
            parmstring <- sprintf("Relative Fit Measure = %.3f\nAbsolute Fit Measure = %3f", relsongfit,abssongfit)
            
            legend("topright",legend=parmstring,bty="n")            
            
            ## done with figures
            dev.off()

          }

        ## add parameter data to song dataframe
        songlistdf <- cbind(songlistdf[,1:4],rel.fit.goodness.vec,abs.fit.goodness.vec)
            names(songlistdf)[5:6] <- c("rel.fit.goodness","abs.fit.goodness")
        
        filename <- paste(sprintf("songlistsplineparams%s.tsv",country))

        ## store these parameters in a file
        ## in this case there is one file for each country
        write.table(songlistdf,file=filename,row.names=FALSE,quote=FALSE,sep="\t")

        ## save song parameters to an .RData file now
        filename <- paste(sprintf("TopSongs%sSplinePars.RData",country))
        save(spline.fits,file=filename)
      }
    
  }


warnings()
