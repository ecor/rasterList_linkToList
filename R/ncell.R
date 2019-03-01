NULL
#'
#' Number of cells in a \code{\link{RasterList-class}} object
#' 
#' @param x a  \code{\link{RasterList-class}} object
#' 
#' @value This methods returns the number of cells and the number of elements of \code{x} 
#' @details This function is an implementation of the \code{ncell} created in the \code{raster} package. The number of cells corresponds to the number of elements of the associeted list.
#' @method ncell RasterList
#' @aliases ncell
#' @export
#' 
#' @examples 
#' 
#' f <- system.file("external/test.grd", package="raster")
#' ra <- rasterList(f)
#' is.RasterList(ra)
#' ncell(ra)
#' 
#' 


setMethod('ncell', signature(x='RasterList'), 
		function(x) {
			
			## Possible checks can be put here
			out <- length(x@list)
			return(out)
			
		}
)
