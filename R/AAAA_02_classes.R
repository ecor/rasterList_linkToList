
NULL
#' S4 declaration for \code{LinkToList-class} S4 class object
#' 
#' 
#
#' 
#' @seealso \code{\link{LinkToList}}
#' 
#' 
#' @docType class 

#' 
#' @keywords classes
#' 
#' 
#' 
#' @aliases LinkToList
#' @name LinkToList-class
#' @rdname LinkToList-class

#' @keywords classes
#' @exportClass LinkToList
#' 
#' @examples 
#' 
#' showClass("LinkToList")
#' 

setClass("LinkToList",slots=list(source="data.frame"),contains="list")
