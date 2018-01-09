## ----setup, include = FALSE----------------------------------------------
library(knitr)
#rmarkdown::render("vignettes/useCase.Rmd")
opts_knit$set(root.dir = '..')
opts_chunk$set(
    #, fig.align = "center"
    #, fig.width = 3.27, fig.height = 2.5, dev.args = list(pointsize = 10)
    #,cache = TRUE
    #, fig.width = 4.3, fig.height = 3.2, dev.args = list(pointsize = 10)
    #, fig.width = 6.3, fig.height = 6.2, dev.args = list(pointsize = 10)
    # works with html but causes problems with latex
    #,out.extra = 'style = "display:block; margin: auto"' 
    )
knit_hooks$set(spar = function(before, options, envir) {
    if (before) {
        par(las = 1 )                   #also y axis labels horizontal
        par(mar = c(2.0,3.3,0,0) + 0.3 )  #margins
        par(tck = 0.02 )                          #axe-tick length inside plots             
        par(mgp = c(1.1,0.2,0) )  #positioning of axis title, axis labels, axis
     }
})

## ---- include = FALSE, warning = FALSE-----------------------------------
#themeTw <- theme_bw(base_size = 10) + theme(axis.title = element_text(size = 9))
bgiDir <- "~/bgi"

## ----inputData, spar = TRUE, message = FALSE-----------------------------
#+++ load libraries used in this vignette
library(REddyProc)
library(dplyr)
#+++ Load data with 1 header and 1 unit row from (tab-delimited) text file
fileName <- getExamplePath('Example_DETha98.txt', isTryDownload = TRUE)
EddyData.F <- if (length(fileName)) fLoadTXTIntoDataframe(fileName) else
  # or use example dataset in RData format provided with REddyProc
  Example_DETha98
#+++ Add time stamp in POSIX time format
EddyDataWithPosix.F <- fConvertTimeToPosix(EddyData.F, 'YDH',Year.s = 'Year'
    ,Day.s = 'DoY',Hour.s = 'Hour')
#+++ Initalize R5 reference class sEddyProc for post-processing of eddy data
#+++ with the variables needed for post-processing later
EddyProc.C <- sEddyProc$new('DE-Tha', EddyDataWithPosix.F, 
	c('NEE','Rg','Tair','VPD', 'Ustar'))

## ---- include = FALSE----------------------------------------------------
.tmp.f <- function(){
  # suspected that example dataset was already gapfilled and tried to load 
  # from LaThuille - not successful
  library(ncdf4)
  ipath <- file.path(bgiDir,"data/DataStructureMDI/DATA/site/Fluxnet"
                     , "halfhourly/level5_new_v2_newRpot_UncNew/Data")
  fname <- "DE-Tha.1996.2006.hourly.nc"
  nc <- nc_open(file.path(ipath,fname))
  get.var.ncdf <- function(...){ as.vector(ncvar_get(...))}
  ds <- subset(data.frame(
    Year  = get.var.ncdf(nc, "year")
    ,DoY  = get.var.ncdf(nc, "julday")
    ,Hour  = get.var.ncdf(nc, "hour")
    ,NEE  = get.var.ncdf(nc, "NEE")
    ,Rg  = get.var.ncdf(nc, "Rg")
    ,Tair  = get.var.ncdf(nc, "Tair")
    ,VPD  = get.var.ncdf(nc, "VPD")
    ,Ustar  = get.var.ncdf(nc, "u")
    ,rH  = get.var.ncdf(nc, "rH")
  ), (Year == 1998 & !(DoY == 366 & Hour == 0)) | 
    (Year == 1999 & (DoY == 366 & Hour == 0)))
  #), Year == 1998)
  ds$DoY[ds$DoY == 366] <- 1
  dss <- EddyDataWithPosix.F <- fConvertTimeToPosix(ds, 'YDH',Year.s = 'Year'
      ,Day.s = 'DoY',Hour.s = 'Hour')
  #EddyDataWithPosix.F$VPD <- fCalcVPDfromRHandTair(
  #EddyDataWithPosix.F$rH, EddyDataWithPosix.F$Tair)
  EddyProc.C <- sEddyProc$new('DE-Tha', EddyDataWithPosix.F, 
  	c('NEE','Rg','Tair','VPD', 'Ustar'))
}

## ------------------------------------------------------------------------
EddyProc.C$sPlotFingerprintY('NEE', Year.i = 1998)

## ---- warn = FALSE, message = FALSE--------------------------------------
uStarTh <- EddyProc.C$sEstUstarThresholdDistribution(
  nSample = 100L, probs = c(0.05, 0.5, 0.95)) 
#filter(uStarTh, aggregationMode == "year")
select(uStarTh, -seasonYear)

## ----fpNEEUStar, include = FALSE, eval = FALSE---------------------------
#  str(uStarTh)
#  signif(unlist(uStarTh[1,5:7]),2)
#  EddyProc.C$sDATA$NEE_low <- EddyProc.C$sDATA$NEE_median <- EddyProc.C$sDATA$NEE_orig <-
#    EddyProc.C$sDATA$NEE_upper  <- EddyProc.C$sDATA$NEE
#  EddyProc.C$sDATA$NEE_orig[ EddyProc.C$sDATA$Ustar < unlist(uStarTh[1,4])] <- NA
#  EddyProc.C$sDATA$NEE_low[ EddyProc.C$sDATA$Ustar < unlist(uStarTh[1,5])] <- NA
#  EddyProc.C$sDATA$NEE_median[ EddyProc.C$sDATA$Ustar < unlist(uStarTh[1,6])] <- NA
#  EddyProc.C$sDATA$NEE_upper[ EddyProc.C$sDATA$Ustar < unlist(uStarTh[1,7])] <- NA
#  # need to produce fingerprints by hand in console - if exeucted from chunk it does not safe a pdf
#  EddyProc.C$sPlotFingerprint('NEE_orig', Dir.s = "plots_fingerprint", Format.s = "png")
#  EddyProc.C$sPlotFingerprint('NEE_low', Dir.s = "plots_fingerprint", Format.s = "png")
#  EddyProc.C$sPlotFingerprint('NEE_median', Dir.s = "plots_fingerprint", Format.s = "png")
#  EddyProc.C$sPlotFingerprint('NEE_upper', Dir.s = "plots_fingerprint", Format.s = "png")
#  EddyProc.C$sDATA$NEE_median <- EddyProc.C$sDATA$NEE_orig <-
#    EddyProc.C$sDATA$NEE_low <- EddyProc.C$sDATA$NEE_upper <- NULL

## ---- message = FALSE----------------------------------------------------
uStarThAnnual <- usGetAnnualSeasonUStarMap(uStarTh)[-2]
uStarSuffixes <- colnames(uStarThAnnual)[-1]
print(uStarThAnnual)

## ----gapfill, message = FALSE--------------------------------------------
EddyProc.C$sMDSGapFillAfterUStarDistr('NEE',
   UstarThres.df = uStarThAnnual,
   UstarSuffix.V.s = uStarSuffixes,
	 FillAll = TRUE
)

## ---- results = 'hold'---------------------------------------------------
grep("NEE_.*_f$",names(EddyProc.C$sExportResults()), value = TRUE)
grep("NEE_.*_fsd$",names(EddyProc.C$sExportResults()), value = TRUE)

## ----fpNEEOrig-----------------------------------------------------------
EddyProc.C$sPlotFingerprintY('NEE_U50_f', Year.i = 1998)

## ----fpNEEFilled, include = FALSE, eval = FALSE--------------------------
#  #```{r fpNEEFilled, results = 'hide', echo = FALSE, warn = FALSE, message = FALSE, fig.show = 'hide'}
#  # plot fingerprint of filled NEE for the median
#  EddyProc.C$sPlotFingerprint('NEE_U50_f', Dir.s = "plots_fingerprint", Format.s = "png")
#  #EddyProc.C$sPlotFingerprintY('NEE_U50_f', Year.i = 1998)
#  #
#  # Check that although using FillAll.b = TRUE still the original NEE is in NEE_f for valid records
#  dss <- cbind(EddyData.F, EddyProc.C$sExportResults())
#  head(dss$Ustar_U50_fqc)
#  plot( NEE_U50_f ~ NEE, subset(dss, Ustar_U50_fqc == 0) )

## ---- message = FALSE----------------------------------------------------
EddyProc.C$sSetLocationInfo(Lat_deg.n = 51.0, Long_deg.n = 13.6, TimeZone_h.n = 1)  
EddyProc.C$sMDSGapFill('Tair', FillAll.b = FALSE)     
EddyProc.C$sMDSGapFill('VPD', FillAll.b = FALSE)     

## ----partNight, message = FALSE------------------------------------------
#variable uStarSuffixes was defined above at the end of uStar threshold estimation
resPart <- lapply(uStarSuffixes, function(suffix){
					 EddyProc.C$sMRFluxPartition(Suffix.s = suffix)
				})

## ----fpGPPReco, include = FALSE, eval = FALSE----------------------------
#  # plot fingerprint of filled NEE for the median
#  EddyProc.C$sPlotFingerprint('GPP_U50_f', Dir.s = "plots_fingerprint", Format.s = "png")
#  EddyProc.C$sPlotFingerprint('Reco_U50', Dir.s = "plots_fingerprint", Format.s = "png")
#  #EddyProc.C$sPlotFingerprintY('GPP_U50_f', Year.i = 1998)
#  dss <- EddyProc.C$sExportResults()
#  grep("GPP",names(dss), value = TRUE)
#  summary(dss$GPP_U05_fqc)
#  plot( dss$GPP_U05_fqc ~ dss$NEE_U50_fqc )

## ------------------------------------------------------------------------
grep("GPP.*_f$|Reco",names(EddyProc.C$sExportResults()), value = TRUE)

## ----fingerPrintGPP------------------------------------------------------
EddyProc.C$sPlotFingerprintY('GPP_U50_f', Year.i = 1998)

## ----aggregateGPP--------------------------------------------------------
FilledEddyData.F <- EddyProc.C$sExportResults()
#suffix <- uStarSuffixes[2]
GPPAgg <- sapply( uStarSuffixes, function(suffix) {
	GPPHalfHour <- FilledEddyData.F[[paste0("GPP_",suffix,"_f")]]
	mean(GPPHalfHour, na.rm = TRUE)
})
print(GPPAgg)

## ---- results = 'hide'---------------------------------------------------
(max(GPPAgg) - min(GPPAgg)) / median(GPPAgg) 

## ---- eval = FALSE-------------------------------------------------------
#  sEstUstarThresholdDistribution(
#    nSample = 200, probs = seq(0.025,0.975,length.out = 39) )

## ---- results = 'hide', message = FALSE, warning = FALSE-----------------
FilledEddyData.F <- EddyProc.C$sExportResults()
CombinedData.F <- cbind(EddyData.F, FilledEddyData.F)
fWriteDataframeToFile(CombinedData.F, 'DE-Tha-Results.txt', Dir.s = tempdir())

