NULL
#' Coercion of a \link{LinkToList-class} object to a generic \link{list} object. 
#'
#' @param x a \link{LinkToList-class} object
#' @param ... forther arguments for \code{\link{[}} method
#'  
#'
#' @name as.list
#' @rdname as.list-methods 
#' @method as.list  LinkToList
#' @aliases as.list,LinkToList-method
#' @export
#' 
#' 

setMethod("as.list",
		signature(x = "LinkToList"),
		function(x,...) {
			
			LinkToList2list(x=x,...)
		}
)



NULL
#'
#' @export
#' @rdname as.list-methods 
#' 
#' 
#' 


LinkToList2list <- function (x, ...) 
	{
	
		l <- 1:length(x)
		out <- x[l,...,forceInMemory=TRUE]
		return(out)
		
	}


