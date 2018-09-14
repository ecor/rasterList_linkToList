# TODO: Add comment
# 
# Author: cordaem
###############################################################################

NULL
#' @title Replace parts of \code{\link{LinkToList-class}}
#' 
#' @description Extract the ith element of a  \code{\link{LinkToList-class}}  object.
#'
#' 
#' 
#' @param x a \code{\link{LinkToList-class}} object 
#' @param i indices of the element to get
#' @param value a generic \code{list} or a \code{\link{LinkToList-class}} obbject containing the elements to be replaced,. It should of length 1 or equal to \code{i}. 
#' @param j,... further arguments 
#' 
#' 
#' @importFrom stringr str_split
#' 
#' @seealso \code{\link{rasterList}}
#' 
#' @name [<-
#' @rdname replace-methods 
#' @method [<- LinkToList
#' @aliases [<-,LinkToList,ANY,ANY,ANY-method
#' @export
#' 
#' 
#' @examples 
#' 
#' set.seed(123)
#' 
#' x <- list()
#' n <- 1000 
#' for (i in 1:n) {
#' 
#' 		x[[i]] <- rnorm(100)
#' }
#' 
#' ll <- linkToList(x)
#' 
#' filename <- rasterTmpFile("test100")
#' 
#' ll2 <- linkToList(x,nperfile=100)
#' 
#' ll2[c(9,10,800)]
#' ll2[c(9,800,100)]
#' x[c(9,10,800)] <- list("a","b","c")
#' ll2[c(9,10,800)] <- list("a","b","c")
#
#' if (identical(x,as.list(ll2))==FALSE) stop("x and ll2 should be identical!! Something went wrong!!")
#'   




setMethod('[<-',signature=c(x="LinkToList",i="ANY",j="ANY"),function (x,i,j,...,value)  {
							
	breaks <- c(x@source$start,x@source$end[nrow(x@source)])
	starts <- cut(i,breaks=breaks,right=FALSE,labels=breaks[-length(breaks)],include.lowest=TRUE)
	starts <- as.integer(as.character(starts))
	ind <- i-starts+1
	names(i) <- sprintf("X%d",i)
	names(starts) <- names(i)
	names(ind) <- names(starts)
	inn <- names(i)
	
	if (length(i)==length(value)) {
		
		
		
		
	} else if (length(value)==1) {
		
		value <- as.list(value)
		value <- value[rep(1,times=length(i))]
		
	} else {
		
		
		stop("i and values have different or incompatible  lengths!!!")
		
	}
	
	names(value) <- names(i)
		
	
	
	ustarts <- unique(starts)
	
	
	names(ustarts) <- sprintf("F%d",ustarts)
	###out <- x ## creaTE a likToLIst ...
	
	cnt <- 0
	
	for (it in names(ustarts)) {
		### TO DO IN THE AFTERNOON !!!!!
		sstart <- ustarts[it]
		il <- which(x@source$start==sstart)
		
		filename <- x@source$file[il]
		load(filename)
		iss <- ind[starts==sstart]
		
		
		
		xs[iss] <- value[names(iss)]
		
		save(xs,file=filename)
	
		xs <- NULL
		
	}
	
	

			
			
			
			
	return(x)
							
							
							
})



NULL
#' @name [[<-
#' @rdname replace-methods 
#' @method [[<- LinkToList
#' @aliases [[<-,LinkToList,ANY,missing,ANY-method
#' @export
#' 
#' 



setMethod('[[<-',signature=c(x="LinkToList",i="ANY",j="missing"),function (x,i,...,value)  {
			
			
			if (length(value)==0) value <- NA
			if (!is.list(value)) value <- list(value=value)
			x[i,...] <- value
			
			return(x)
			
			
			
})












