# SongDownloadEpidemics
Supporting data and scripts for the paper Modelling Song Popularity as a Contagious Process

###########################
## REQUIRED R PACKAGES ####
###########################

R packages that are used for the various scripts in this repo are:

rJava
stats4
nloptr
plyr
dplyr
lubridate
timeDate
fitsir
bbmle
lattice
lhs
deSolve
GillespieSSA
splines
tibble
tidyr
ggplot2
gridExtra
ggstance
tikzDevice
RColorBrewer
mgcv


#######################
## FIGURE SCRIPTS #####
#######################

(In the R folder)

Fig. 2: UserStatsPlots.Rnw
- Generates Fig. 2 in section "Number of Downloads by Genre Plots", subsection "Downloads by all Users"
- Requires included files newaxis.R, downloadsbygenreGB.tsv

Fig. 3: PlotSongFits.Rnw
- Generates figures included in Fig. 3
- Select individual songs to plot -- this is done on line 248 when setting the "song.pos" parameter
- Requires included files timeaggfuns.R, ExcludedSongsGB.tsv, countrylist.tsv, minR0calc.R, ParameterSettings.R
- Requires included files that top1000GBsongs.tsv, TopSongsGBFitsAndTimescales_NoAccents.RData, TopSongsGBTimescales_with_accents.RData, tsv file for song being plotted (e.g., BadRomance_LadyGagaGB.tsv)
- tsv files for songs are located in subfolder "Top1000GBSongstsv"

Fig. 4: RelFitScatterGB.Rnw
- Generates the Songs plot in Fig. 4
- Requires included files minR0calcGB.R, ParameterSettings.R, ExcludedSongsGB.tsv, newaxis.R, songlistparamsGB.tsv, songlistsplineparamsGB.tsv

Fig. 4: RelFitScatterStochastic.Rnw
- Generates the Epidemics plot in Fig. 4
- Requires included files ParameterSettings.R, newaxis.R, stochsim_4.rda

Fig. 5: FitCompPlots.Rnw
- Generates both violin plots, along with a few other density plots
- Requires included files minR0calcGB.R, ExcludedSongsGB.tsv, stochsim_4.rda, songlistparamsGB.tsv, songlistsplineparamsGB.tsv

Fig. 6: ParameterDensityPlotsLHS.Rnw
- Generates density plot and boxplot for R0 and mean infectious period
- Requires included files minR0calc.R, ParameterSettings.R, ExcludedSongsGB.tsv, songlistparamsGB.tsv

Fig. 7: S0vsCountLHS.Rnw
- Generates plot of S0 vs. Download Count
- Requires included files minR0calc.R, ParameterSettings.R, ExcludedSongsGB.tsv, songlistparamsGB.tsv


####################
## OTHER SCRIPTS ###
####################

(In the R folder)

SplineFitsTimescaleOnly.R
- Aggregates download data for each song at the most appropriate timescale and saves this timescale information. These timescales are then used by the script SplineFitsPdf.R to fit splines to each song.
- Requires included files top1000GBsongs.tsv, timeaggfuns.R, countrylist.tsv
- Requires all tsv files with download curve data for songs (included)
- Produces file TopSongsGBSplineTimescales.RData, which is used by SplineFitsPdf.R
- NOTE: To run this script, adjust data and script paths to line up with where you have placed the data files and supporting scripts on your machine.

SplineFitsPdf.R
- Fits splines to download time series for songs, then plots the splines and download time series and saves these as pdfs.
- Requires included files timeaggfuns.R, top1000GBsongs.tsv, ExcludedSongsGB.tsv, countrylist.tsv, ParameterSettings.R, TopSongsGBSplineTimescales.RData
- Requires all tsv files with download curve data for songs (included)
- NOTE: To run this script, adjust data and script paths to line up with where you have placed the data files and supporting scripts on your machine.

PlotAndParmsRdaOnlyGB.R
- Creates fits for the download time series for a list of songs; stores these fits (along with the appropriate data aggregation timescale for each song) as an Rdata file that can be reloaded later to make plots with.
- Requires included files timeaggfuns.R, top1000GBsongs.tsv, countrylist.tsv
- Requires all tsv files with download curve data for songs (included)
- NOTE: To run this script, adjust data and script paths to line up with where you have placed the data files and supporting scripts on your machine.

PlotAndParmsPdfGB.R
- Loads .RData files generated by PlotAndParmsRdaOnly.R and uses them to create plots of download time series with their fitted curves for the songs listed. Will also extract parameters and measures of goodness of fit and store these in an .RData file and a .tsv file.
- Requires included files timeaggfuns.R, top1000GBsongs.tsv, countrylist.tsv, ExcludedSongsGB.tsv, minR0calc.R, ParameterSettings.R, TopSongsGBFitsAndTimescales_Accents.RData, TopSongsGBFitsAndTimescales_NoAccents.RData
- Requires all tsv files with download curve data for songs (included)
- Produces the file songlistparamsGB.tsv, which contains all SIR parameters from the fits, measures of goodness of fit, etc. Also produces pdf file with plot of download time series and fitted SIR curve.
- NOTE: To run this script, adjust data and script paths to line up with where you have placed the data files and supporting scripts on your machine.
- NOTE: On line 210 of the script, the user selects which Data file the script should load (i.e., TopSongsGBFitsAndTimescales_Accents.RData or TopSongsGBFitsAndTimescales_NoAccents.RData). There are two RData files because of the way that accents in artist names or song titles were handled by R.

StochasticSIRSample.R
- Demonstrates how epidemic curves were generated, how splines were fitted to these curves and plotted, and how the SIR model was fitted to the curves
- NOTE: This is a sample file and is lacking the LHS machinery that was used (to fit 100 different SIR curves to each epidemic curves). The code for this can be found in PlotAndParmsRdaOnlyGB.R.


#######################
## OTHER DATA FILES ###
#######################

(In the Data folder)

EpidemicFitData.tsv
- contains relative fit measures for both SIR and spline models fitted to our epidemic curves

SongFitData.tsv
- contains relative fit measures for both SIR and spline models fitted to our song download curves
- also contains various parameters extracted from the SIR fits for song download curves

SongDownloadData.zip
- contains all .tsv files with (proportional) download time series for the songs in our sample set
