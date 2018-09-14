

rm(list=ls())

library(rasterList)

source('/STORAGE/projects/R-Packages/rasterList/R/stack.R')
source('/STORAGE/projects/R-Packages/rasterList/R/rasterList.R')
source('/STORAGE/projects/R-Packages/rasterList/R/LinkToList.R')



#
##' f <- system.file("external/test.grd", package="raster")
##' rr <- rasterList(f) 
##' rs <- as.RasterList(f)
##' # The package-provided datasets shall be only used as example datasets. 
precf <- system.file("map/precipitation.grd", package="rasterList")##
##' ## A resampled preciptation raster map based on CHIRS dataset:
##' ## Funk, Chris, Pete Peterson, Martin Landsfeld, Diego Pedreros, James Verdin, 
##' ## Shraddhanand Shukla, Gregory Husak, James Rowland, Laura Harrison, 
##' ## Andrew Hoell and Joel Michaelsen.
##' ## "The climate hazards infrared precipitation with stations - a new environmental 
##' ## record for monitoring extremes". Scientific Data 2, 150066. doi:10.1038/sdata.2015.66 2015. 
##' ## http://chg.geog.ucsb.edu/data/chirps/
##' ##
##' 
##' 
##' ## Sample L-moments
library(lmom)
##' \dontrun{
prec <- stack(precf)
ee <- extent(prec)
ee@xmax <- ee@xmin+0.2
ee@ymax <- ee@ymin+0.2
pfilename <-  '/STORAGE/projects/R-Packages/rasterList/inst/todo/prec_test.grd'

prec <- crop(prec,ee,filename=pfilename,overwrite=TRUE)
###ppp <- rasterList(prec,FUN=samlmu)
samlmom <- stack(rasterList(prec,FUN=samlmu))
##' ## Fitting a Random Probability Distribution: it is a 'rasterList' Object


fitdist <- rasterList(samlmom,FUN=pelgam)



##' }
##' 
##' precf <- system.file("map/Mekrou_precipitation.grd", package="rasterList")
##' prec <- stack(precf)
##'  # Set time
##' time <- as.Date(names(prec),format="X%Y.%m.%d")
##' year <- as.character(time,format="X%Y")
##' 
##' ## Compute Annual Precipitation (sum aggregration)
##' yearlyprec <- stackApply(x=prec,fun=sum,indices=year)
##' ## L-moments
##' samlmom <- stack(rasterList(yearlyprec,FUN=samlmu))
##' fitdist <- rasterList(samlmom,FUN=pelgam)
##' 
##' 
