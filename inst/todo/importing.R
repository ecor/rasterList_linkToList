

####rm(list=ls())

library(rasterList)
source('/STORAGE/projects/R-Packages/rasterList/R/rasterList.R') 
source('/STORAGE/projects/R-Packages/rasterList/R/LinkToList.R') 
				
wpath <- '/DATA/PRECIPITATION/monthly/tif' 

tif <- list.files(wpath,full.name=TRUE,pattern=".tif")
out <- stack(tif[1:10])

ext <- extent(raster(system.file("map/africa/draindir/af_dir_30s.bil",package="hydrosheds")))

filename <- '/DATA/PRECIPITATION/monthly/precipitation_africa.grd'

outp <- crop(x=out,y=ext,filename=filename,overwrite=TRUE)



###ol <- rasterList(outp,nperfile=1000000)  ### TROVARE UN COMPROMESSO 

cc <- system.time(ol <- rasterList(outp,nperfile=100000))  ### TROVARE UN COMPROMESSO ## SISTWEMARE IL NUMERO DEI BLOCCHI!!!!

source('/STORAGE/projects/R-Packages/rasterList/R/stack.R') 
source('/STORAGE/projects/R-Packages/rasterList/R/raster.R')

uu <- stack(ol)
ccyl <- system.time(ul <- stack(ol))


