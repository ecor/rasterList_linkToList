
####
#### https://www.r-bloggers.com/profiling-r-code/


rm(list=ls())

library(lmom)
library(rasterList)
library(raster)
library(stringr)
library(lineprof)

source('/STORAGE/projects/R-Packages/rasterList/R/rasterList.R') 
rasterList_R <- '/STORAGE/projects/R-Packages/rasterList/R' 
files <- list.files(rasterList_R,pattern=".R",full.name=TRUE)
##for (file in files) source(file)

####wpath <- '/H01/SHAREDWORK/ACEWATER2/data/db/climate/chirps/data/monthly/tif' 
###prec_input <- '/STORAGE/projects/R-Packages/clivar_lmom/data/africa_chirps.grd' 
prec_yearly_input <- '/STORAGE/projects/R-Packages/clivar_lmom/data/africa_chirps_yearly.grd' 
output <- '/STORAGE/projects/R-Packages/clivar_lmom/output' 
prec_yearly <- stack(prec_yearly_input)

fun_ <- function(x) {
	
	x <- as.list(x)
	
	
}



prof_rasterlist10 <- lineprof({ 
			prec_lmom_rasterlist10 <- stack(rasterList(prec_yearly,FUN=samlmu,nblocks=10))
		})





prof_rasterlist <- lineprof({ 
		prec_lmom_rasterlist <- stack(rasterList(prec_yearly,FUN=samlmu))
	})

#prof_rasterlist_nostack <- profr({ 
#			prec_lmom_rasterlist_v2 <- (rasterList(prec_yearly,FUN=samlmu))
#		})

prof_calc <- lineprof({
		prec_lmom_calc <- calc(prec_yearly,fun=samlmu,forceapply=TRUE)
	})


#
#for (it in names(prec_lmom_rasterlist)) {
#	
#	filename <- sprintf("%s/africa_lmoment_%s.tif",output,it)
#	writeRaster(prec_lmom_rasterlist[[it]],filename=filename,overwrite=TRUE)
#	print(filename)
#	
#	
#}
#

#
#require(profr)
#require(ggplot2)
#x = profr(example(glm))
#ggplot(x)

#require(proftools)
#Rprof(tmp <- tempfile())
#example(glm)
#Rprof()
#plotProfileCallGraph(readProfileData(tmp),
#		score = "total")
#proftools_example
