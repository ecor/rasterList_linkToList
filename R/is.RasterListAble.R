# TODO: Add comment
# 
# Author: cordaem
###############################################################################
NULL
#' Can it be coerced to a \code{\link{RasterList-class}} object ? 
#'
#' @param x a valid object 
#' 
#' 
#' @export 
#' 
#' @examples 
#' 
#' r <- rasterList()
#' is.RasterListAble(r)
#' rr <- raster()
#' is.RasterListAble(rr)
#' 
#' f <- system.file("external/test.grd", package="raster")
#' ra <- stack(c(f,f))
#' is.RasterListAble(ra)
#' 
#' 
#'

is.RasterListAble <- function(x) {
	
	
	
	
	## TO DO 
	out <- is.RasterList(x)
	if (out!=TRUE) {
		
		out <- (class(x) %in% c("RasterStack","RasterBrick"))
	}
	
	
	
	return(out)
	
	
	
	
	
}

