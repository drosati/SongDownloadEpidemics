## Title: R0 Min Calculator
## Author: Dora Rosati
## Created: May 19, 2015
## Modified: 
##
## Purpose: This script will calculate the minimum possible R0 for
## a given set of songs based on the minimum download count and the
## maximum possible susceptible population.


# number of users in GB, found using the below SQL query:
# SELECT count(*) FROM User WHERE country='GB' GROUP BY country;
maxS0 <- 552784
# This is the maximum possible susceptible population for a song
# in GB

# load file with songs and parameters we're interested in
songparmsdf <- read.delim("../songlistparamsGB.tsv",quote="")

# find the minimum download count for the songs in the data frame
# (note that for the top 1000 songs it will be the last entry,
# but this may not be the case if we use some other criterion to
# examine songs in the future)
min.downloads <- with(songparmsdf,min(count))

# Then we can find the minimum possible final size Z
min.Z <- min.downloads/maxS0

# set up function for R0 as a function of Z
R <- function(Z) {
  -log(1-Z)/Z
}

# use the min.Z to calculate a minimum R0
(min.R0 <- R(min.Z))
