
# TODO: Add comment
# 
# Author: cordaem
###############################################################################
NULL
#' Class \code{RasterList}
#' 
#' 
#' It contains \code{\link{RasterLayer-class}} with the following adjoint slots:
#'  \describe{
#'     \item{\code{list}:}{a list of generic object whese length corresponds to the number of cells. Each \code{list} element for each cell; }
#' 
#' 	   \item{\code{name}:}{an identification name of the object. Default is \code{NA}.}
#'  }
#' 
#' This class inherits the \code{\link{RasterLayer-class}} class considering each pixel of the raster is a generic object. 
#' 
#' @seealso \code{\link{raster}},\code{\link{Raster-class}}
#' 
#' 
#' 
#' @docType class 
#' @title RasterList-class
#' 
#' @keywords classes
#' 
#' @author Emanuele Cordano
#' @aliases RasterList-class
#' @name RasterList-class
#' @rdname RasterList-class
#' @export
# ##' @exportClass RasterList
## #' @importClass RasterLayer
#' 
#' @examples 
#' 
#' showClass("RasterList")
#' 


setClass('RasterList',
		slots=c(list="list",name="character"),
###		,contains='character')
		,contains='RasterLayer')

