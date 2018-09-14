# TODO: Add comment
# 
# Author: cordaem
###############################################################################
NULL
#' Is a \code{\link{RasterList-class}} object ? 
#'
#' @param x a valid object 
#' 
#' 
#' @export 
#' 
#' @examples 
#' 
#' r <- rasterList()
#' is.RasterList(r)
#' rr <- raster()
#' is.RasterList(rr)
#' 
#' f <- system.file("external/test.grd", package="raster")
#' ra <- rasterList(f)
#' is.RasterList(rr)
#' 
#' 
#'

is.RasterList <- function(x) {
	
	
	
	
	## TO DO 
	out <- (class(x)=="RasterList")
				
	if (out==TRUE) {
		
		out <- (class(x@list)=="list") | (class(x@list)=="LinkToList")
		
		
		
	}
	if (out==TRUE) {
		
		out <- (class(raster(x))=="RasterLayer")
		
		
		
		
	}
	
	if (out==TRUE) {
		
		
		out <- (length(x@list)==ncell(x))
		
	}
	
	return(out)
	
	
	
	
	
}

