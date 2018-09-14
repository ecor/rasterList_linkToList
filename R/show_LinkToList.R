
#' @title Print or Show S4 method for a \code{\link{LinkToList-class}} object
#' 
#' @description Prints a \code{\link{LinkToList-class}} object  
#' 
#' @param object a \code{\link{LinkToList-class}} object 
#'   
#' @importFrom methods show
#' @rdname show 
#' @method show LinkToList
#' @aliases show,LinkToList
#' @export
#' 
#' 




setMethod("show",
    signature(object = "LinkToList"),
    function (object) 
    {
        
		mm <- length(object)
		mm[mm>10] <- 10 
		print(object[1:mm,forceInMemory=TRUE])
		message("SOURCE:")
		print(object@source)
		
		return(0)
		
    }
)
