# This is a test script for crop method
# 
# Author: Emanuele Cordano
###############################################################################
rm(list=ls())
set.seed(100)

library(rasterList)
library(lmom)

####
###source('/STORAGE/projects/R-Packages/rasterList/R/stack.R')

####







## TESTING R CODE: 
library(testthat)
context("Verify cropping a RasterList")

## UNDER DEVOLOPMENT


precf <- system.file("map/precipitation.grd", package="rasterList")
prec <- stack(precf)

inMemory <- c(TRUE,FALSE)


for (itm in inMemory) {
	
	if (itm==TRUE) {
		
		prec <- prec+0
		
	} else {
		
		prec_file <- rasterTmpFile('tt')
		prec <- writeRaster(prec,filename=prec_file,overwrite=TRUE)
		
	}
	## Sample L-moments 
	precl <- rasterList(prec,FUN=samlmu,nblocks=4)

	
	samlmom <- stack(precl)
	## Fitting a Random Probability Distribution: it is a 'rasterList' Object
	fitdist <- rasterList(samlmom,FUN=pelgam)
	
	##### ZOOM IN 
	## set a mask 
	mask <-raster( extent(fitdist)/4 )
	
	
	fitdist_masked <- crop ( x = fitdist,y=mask)
	
	####
	prec_crop <-  crop(x= prec , y= mask )
	samlmom_crop <- stack(rasterList(prec_crop,FUN=samlmu))
	fitdist_crop <- rasterList(samlmom_crop,FUN=pelgam)
	
	desc = sprintf("Testing final Results (inMemory=%s)",as.character(itm))
	
	message(desc)
	test_that(desc=desc,code=expect_equal(as.list(fitdist_crop@list),as.list(fitdist_masked@list), tolerance = .002, scale = 1))
	test_that(desc=desc,code=expect_equal(fitdist_crop@extent,fitdist_masked@extent, tolerance = .002, scale = 1))
}