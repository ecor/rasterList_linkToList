# TODO: Add comment
# 
# Author: cordaem
###############################################################################
NULL
#' \code{Raster} methods for a \code{\link{RasterList-class}} object.
#'
#' @param x a valid \code{\link{RasterList-class}} object
#' @param ... further arguments 
#' 
#' @export 
#'
#' @rdname raster
#' @method raster RasterList
#' @aliases raster 
#'  
#' @seealso \code{\link{stack}},\code{\link{RasterListApply}}
#' @examples 
#' 
#' f <- system.file("external/test.grd", package="raster")
#' ur <- rasterList(raster(f),FUN=function(x,d){x+0:d},d=10)
#' 
#' r1 <- raster(ur)
#' r2 <- raster(ur,FUN=function(x){x[2]})
#' 

# RasterListApply
# ## TEST RASTER 
# 
# 
# 

#
#setMethod('raster', signature(x='RasterList'), 
#		function(x,FUN=NULL,...) {
#			
#		
#			x <- rasterList(object=x,FUN=FUN,...)
#
#			
#			out <- as(x,"RasterLayer")
#			
#			scalarize_f <- function(x){
#				
#					o <- try(as.numeric(as.vector(x))[1],silent=TRUE)
#					if (class(o)=="try-error") o <- 1*NA
#					return(o)
#			
#					}
#					
#			if (class(x@list)=="LinkToList") {
#				
#				out[] <- unlist(LinkToList2list(linkToList(object=x@list,FUN=scalarize_f)))
#				
#				
#			}		else {
#				<<-
#				
#				out[] <- sapply(X=x@list,FUN=scalarize_f) 
#			}
#			
#			
#			return(out)
#
#		}
#)
# @param FUN if it not \code{NULL} a function is applied to all elements of the \code{list} slot in \code{x}. 


setMethod('raster', signature(x='RasterList'), 
		function(x,...) {
			
			
			out <- try(stack(x,...),silent=TRUE)
		
			if (class(out)=="try-error") {
				
				out <- as(x,Class="RasterLayer")
				
			}  else {
				
				out <- out[[1]]
			}
			return(out)
			
		}
)


