NULL


#' @title Creates a \code{\link{RasterList-class}}  object
#' 
#' @description The method \code{rasterList} is the constructor of a  \code{\link{RasterList-class}} from a generic \code{object}. 
#'
#' 
#' 
#' @param object the object to coerce 
#' @param list a \code{list} object to assign to the raster map. 
#' @param object.name character string containing the name to assign to \code{object}.
#' @param FUN function that can be used to apply to each element of the list in a \code{\link{RasterList-class}}
#' @param nperfile see \code{link{linkToList}}.
#' @param nblocks number of blocks (i.e. \code{link{linkToList}} .rda files) to be used when a \code{\link{RasterStack-class}} or \code{\link{RasterBrick-class}} object is coerced to a \code{\link{RasterList-class}} object.  
#' @param filename filename where to write the output, see \code{\link{writeRaster}} and \code{link{linkToList}}.
#' @param overwrite logical variable. If it is \code{TRUE} output is rewritten,  see \code{\link{writeRaster}}.
#' @param ... further arguments for \code{\link{raster}}(generic) or \code{FUN} (\code{\link{RasterList-class}})
#' 
#' @rdname rasterList
#' @export
#' 
#' @details The argument \code{FUN} is useful to create or transform \code{\link{RasterList-class}} from other Raster* classes. 
#' 
#' @importFrom raster ncell raster nlayers inMemory filename brick writeRaster
#' @importFrom methods as
#' @importFrom utils str
#' 
#' 
#' @examples 
#'
#' f <- system.file("external/test.grd", package="raster")
#' rr <- rasterList(f) 
#' rs <- as.RasterList(f)
#' # The package-provided datasets shall be only used as example datasets. 
#' precf <- system.file("map/precipitation.grd", package="rasterList")##
#' ## A resampled preciptation raster map based on CHIRS dataset:
#' ## Funk, Chris, Pete Peterson, Martin Landsfeld, Diego Pedreros, James Verdin, 
#' ## Shraddhanand Shukla, Gregory Husak, James Rowland, Laura Harrison, 
#' ## Andrew Hoell and Joel Michaelsen.
#' ## "The climate hazards infrared precipitation with stations - a new environmental 
#' ## record for monitoring extremes". Scientific Data 2, 150066. doi:10.1038/sdata.2015.66 2015. 
#' ## http://chg.geog.ucsb.edu/data/chirps/
#' ##
#' 
#' 
#' ## Sample L-moments
#'  library(lmom)
#' \dontrun{
#' prec <- stack(precf)
#' samlmom <- stack(rasterList(prec,FUN=samlmu))
#' ## Fitting a Random Probability Distribution: it is a 'rasterList' Object
#' fitdist <- rasterList(samlmom,FUN=pelgam)
#' }
#' 
#' precf <- system.file("map/Mekrou_precipitation.grd", package="rasterList")
#' prec <- stack(precf)
#'  # Set time
#' time <- as.Date(names(prec),format="X%Y.%m.%d")
#' year <- as.character(time,format="X%Y")
#' 
#' ## Compute Annual Precipitation (sum aggregration)
#' yearlyprec <- stackApply(x=prec,fun=sum,indices=year)
#' ## L-moments
#' samlmom <- stack(rasterList(yearlyprec,FUN=samlmu))
#' fitdist <- rasterList(samlmom,FUN=pelgam)
#' 
#' 


#precf <- system.file("map/precipitation.grd", package="rasterList")
#prec <- stack(precf)
# 
#sdd <- stackApply(x=prec,fun=sd,indices=1)
#avg <- stackApply(x=prec,fun=mean,indices=1)
# 
#prec_std <- (prec-avg)/sdd
#prec_ks <- rasterList(prec_std,FUN=ks.test,y="pgamma",
#shape=1,rate=1) ## Complete Kolgomorov-Smirnov fitting test 
# 
#pval_ks <- raster(prec_ks,FUN=function(x){x$p.value})
# 
# 

rasterList <- function (object=NULL,...)  {
	
	
	return(standardGeneric("rasterList"))
	
}

NULL
#' @title rasterList
#' @rdname rasterList
#' @aliases rasterList 
#' @export
#' 

as.RasterList <- function(object,...) {
	
	
	return(rasterList(object,...))
	
	
}

NULL
#' @title rasterList
#' @rdname rasterList
#' @method rasterList default
#' @aliases rasterList 
#' @export


setGeneric("rasterList",function (object=NULL,list=NULL,object.name=NA,...)  {
			
		
			if (is.null(object)) {
				
				out <- raster(...)
			} else { 
			
				out <- raster(object,...)
			
			}
			
	
									

			out <- rasterList(out,list=list,object.name=object.name) ## In the generic method no function (FUN) can be applied!!!!  
			
			
			return(out)
			
			
			
		})



NULL
#' @title rasterList
#' @rdname rasterList
#' @method rasterList RasterLayer
#' @aliases rasterList 
#' @export


setMethod("rasterList","RasterLayer",function (object,list=NULL,object.name=NA,...)  {
			
			
			
			
		 #####   if (inMemory(object) & is.null(list)) {
			if (is.null(list)) {
				list <- as.list(object[,])
				
			}
			out <- as(object,Class="RasterList")
			out <- rasterList(out,list=list,object.name=object.name,...)
			
			
			
			return(out)
			
			
			
		})




NULL
#' @title rasterList
#' @rdname rasterList
#' @method rasterList RasterStack
#' @aliases rasterList 
#' @export

setMethod("rasterList","RasterStack",function (object,...) {
			
			
			object <- brick(object)
			
			out <- rasterList(object,...)
		
			return(out)
			
		})




NULL

#' @title rasterList
#' @rdname rasterList
#' @method rasterList RasterBrick
#' @aliases rasterList 
#' @export


setMethod("rasterList","RasterBrick",function (object,object.name=NA,FUN=NULL,overwrite=TRUE,filename="",nperfile="default",nblocks=20,...)  {
			
			
			
			if (nperfile=="default") nperfile <-  floor(ncell(object)/nblocks)
			
			
			
			filename_object <- filename(object[[1]])
			
			if (is.null(filename)) filename <- NA
			filename <- filename[1]
			if (is.na(filename[1])) filename <- "default" 
			if (filename=="")   filename <- "default"
			if (filename!="default") filename_object <- filename
			
			
			
			if (filename_object=="") {
				out <- as(object[[1]],Class="RasterList")

				list <- apply(X=object[,],MARGIN=1,FUN=list)  ## create LinkToList HERE
				list <- lapply(X=list,FUN=unlist)
				if (!is.null(FUN)) list <- lapply(X=list,FUN=FUN,...)
			
			} else {
				
				funb <- function(x,b,func=FUN,...) { 
					
					o <- b[x]
					
					o <- apply(o,FUN=as.list,MARGIN=1)
					o <- lapply(o,FUN=unlist)
					
					
				
					
					if (!is.null(func))	o <- lapply(o,FUN=func,...)
					return(o)
					
				}
		
				
		
				
				if (filename=="default") {
				
					out <- as(object[[1]],Class="RasterList")
					
				} else {
					
					
					out <- writeRaster(object[[1]],filename=filename,overwrite=overwrite)
					out <- as(out,Class="RasterList")
					filename_object <- filename(out)
				
				}
				
				if (overwrite==FALSE) filename <- "default"
				list <- linkToList(object=NULL,filename=filename_object,nperfile=nperfile,length=ncell(object),FUN=funb,b=object,func=FUN,single.element=FALSE,...)
			}
			## This call has to be checked and/or removed!
			if (!is.na(object.name)) out@name <- as.character(object.name) ## 20190211
	    	if (length(out@name)==0) out@name <- as.character(object.name) ## 20190211
			out@list <- list ## 20190211
	## TEST THis lines after!!!
	##20190211		out <- rasterList(out,list=list,object.name=object.name)
			
			
			
			return(out)
			
			
			
		})





NULL





#' @title rasterList
#' @rdname rasterList
#' @method rasterList RasterList
#' @aliases rasterList 
#' @export


setMethod("rasterList","RasterList",function (object,list=NULL,object.name=NA,FUN=NULL,filename="",overwrite=FALSE,nperfile="default",nblocks=20,...)  {
			
		
		#	listInMemory <- TRUE
			hasFun <- !is.null(FUN)
			
		
			
			if (is.null(filename)) filename <- NA
			filename <- filename[1]
			if (is.na(filename[1])) filename <- "" 
			
			if (filename=="temporary") filename <- "default"
			if (filename=="default") filename <- rasterTmpFile('ll')
			
		    if (filename!="")   {
				
				list_ <- object@list
				object <- writeRaster(object[[1]],filename=filename,overwrite=overwrite)
				object <- as(object,Class="RasterList")
				object@list <- list_
			
			}
			
	
			
			if (!is.null(list)) {
				object@list <- list ### cCHECK IF THE RASTER IS inMemory or NOT!! 
			}	
			
			listInMemory <- !(class(object@list)=="LinkToList")
			if (nperfile=="default") nperfile <-  floor(ncell(object)/nblocks)
			
			listInMemory <- !(class(object@list)=="LinkToList") &   !(is.numeric(nperfile) & nperfile>0) 
			
				###listInMemory <- TRUE ## Add a condition if the list is a reference to other files and not in memory!!!
				
			
			if (is.null(object.name)) object.name <- NA 
			if (length(object.name)==0) object.name <- NA 
			
			if (!is.na(object.name)) object@name <- as.character(object.name)
			if (length(object@name)==0) object@name <- as.character(object.name)
		
			
			if (length(object@list)!=0 & (length(object@list)!=ncell(object))) {
			
				
				msg <- sprintf("Mismatch between length of the list and number of raster calles : %d %d in %s",length(list),ncell(object),object@name)
				##message(msg)
				stop(msg)
				
			}
			
		##	listInMemory <- TRUE ###FALSE
		
			
#			if (length(object@list)!=0 & (!is.null(FUN))) {
#				
#				listInMemory <- TRUE ## This line is temporary!!!
#			}	
			if (listInMemory) {	
				### CONSIDER IF FUN IS NULL !!!
			
			
				
				if (hasFun) object@list <- lapply(X=object@list,FUN=FUN,...)	## Here lapply___
				listInMemory <- TRUE
			}	else {
				
				
#				if (is.na(maxCellsperBlock))  maxCellsperBlock <- ncell(object)/100
					
				if (filename=="")filename <- filename(object[[1]])
				if (filename=="") filename <- "default"
				object@list <- linkToList(object@list,filename=filename,FUN=FUN,nperfile=nperfile,...)
		
				
			}
			
	
			
			
			
			
			return(object)
			
			
			
		})

































































