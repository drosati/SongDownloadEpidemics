## Script Name: Time Aggregation Functions
## Author: Dora Rosati (with much help from Ben Bolker)
## Created: April 17, 2015 (from ModifyTimescale.R)
## Modified:
##
## Purpose: This script provides functions related to aggregating song
## download data on the appropriate timescale, which will be used by
## the script PlotAndParms.R

# load necessary packages
library("lubridate")
# Ben: generally convenient for date/time manipulation,
# we'll go ahead and use dplyr.  Not strictly
# necessary, but for large data sets it should
# be faster
library("plyr")
library("dplyr")
library("mgcv")

# function to aggregate counts at desired time scale
# NOTE: data frame that is fed to this function must have
# column names 'count' and 'datetime'
# NOTE: back.conv must be ymd if breaks are > or = 1 day and must
# be ymd_hms if breaks are < 1 day - incorporate this in
# PlotAndParms.R script
aggfun1 <- function(dd,breaks="hour",back.conv=NULL) {
    dd2 <- dd %>%
        ## convert date+time to time
        mutate(tt=ymd_hms(datetime),
               ## make time breaks
               ## FIXME: should we worry about making the
               ## starting time 'pretty', e.g. by adding a
               ## zero-count observation at the beginning?
               ttcat=cut(tt,breaks=breaks)) %>%
                   group_by(ttcat) %>%
                       summarise(count=sum(count))
    if (!is.null(back.conv)) {
        ## convert back from factor to date/time
        dd2 <- dd2 %>%
            ## FIXME: should this be ymd or ymd_hms or ??
            ## currently depends on whether break is > or < 1 day
            mutate(date=back.conv(ttcat)) %>%
                select(-ttcat)
    }
    return(dd2)
}

# Example use:
# dd2 <- aggfun1(song,"month",back.conv=ymd)
