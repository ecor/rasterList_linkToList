# TODO: Add comment
# 
# Author: cordaem
###############################################################################
NULL
#' \code{Crop} methods for a \code{\link{RasterList-class}} object.
#'
#' @param x a valid object 
#' @param y a Spatial Object or an Extent
#' @param check.RasterList logical value. If it is \code{TRUE}, it checks the \code{x} \code{\link{RasterList-class}} object. Default is \code{FALSE}.
#' @param ... further arguments 
#' 
#' @export 
#'
#' 
#' @rdname crop
#' @method crop RasterList
#' @aliases crop 
#' 
#' @importFrom raster crop
#'  
#' @examples 
#' 
#' 
#' precf <- system.file("map/precipitation.grd", package="rasterList")
#' prec <- stack(precf)
#' \dontrun{
#' ## Sample L-moments 
#' 
#' library(lmom)
#' 
#' samlmom <- stack(rasterList(prec,FUN=samlmu))
#' ## Fitting a Random Probability Distribution: it is a 'rasterList' Object
#' fitdist <- rasterList(samlmom,FUN=pelgam)
#' 
#' ##### ZOOM IN 
#' ## set a mask 
#' mask <-raster( extent(fitdist)/4 )
#' 
#' 
#' fitdist_masked <- crop ( x = fitdist,y=mask)
#' }
#' 
#' 
#' 
#' 




setMethod('crop', signature(x='RasterList', y='ANY'),
		function(x,y,check.RasterList=TRUE,...) {
			
			
			if (check.RasterList==TRUE) {
				
				
				cond <- is.RasterList(x)
				
				
			}
			
			if (cond!=TRUE) {
				
				warning("Non-conformed RasterList for 'Crop' method  exit NULL!!")
				out <- NULL
				
			} else {
				xout <- raster(x[[1]])
				xout[] <- 1:ncell(xout)
			
				out <- crop(xout,y,...)
				
				if (length(x@list)>0) {
					list <- x@list[out[]]
				} else {
					
					list <- NULL
				}
				out <- rasterList(out,list=list)
			}
			
			return(out)
		###3out <- crop(x,y,...)
			
		})
