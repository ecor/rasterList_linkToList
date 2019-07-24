# TODO: Add comment
# 
# Author: cordaem
###############################################################################

NULL
#' @title Extract parts of \code{\link{LinkToList-class}}
#' 
#' @description Extract the ith element of a  \code{\link{LinkToList-class}}  object.
#'
#' 
#' 
#' @param x a \code{\link{LinkToList-class}} object 
#' @param i indices of the element to get 
#' @param forceInMemory logical value. Deafault is \code{FALSE}. If it is \code{TRUE}, method return a \code{list} saved in memomey, otheriwise it can return a  \code{\link{LinkToList-class}} object
#' @param clean logical value. Default value is \code{FALSE}. It is used in case \code{forceInMemory==TRUE} , if \code{clean==TRUE} external files containing output data are removed. See \code{\link{LinkToList2list}}.
#' @param filename,nperfile arguments passed to \link{linkToList} if \code{forceInMemory==FALSE}. 
#' 
#' 
#' 
#' @param j,drop,... further arguments 
#' 
#' 
#' @importFrom stringr str_split
#' 
#' @seealso \code{\link{linkToList}}
#' 
#' @name [
#' @rdname extract-methods 
#' @method [ LinkToList,ANY,ANY
#' @aliases [,LinkToList,ANY,ANY-method
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
#' x[c(9,10,800)]
#' 
#' x[[28]]
#' ll2[[28]]
#' 




setMethod('[',signature=c(x="LinkToList",i="ANY",j="ANY"),function (x,i,j,...,forceInMemory=FALSE,filename=rasterTmpFile(),nperfile=ceiling(length(i)/10),clean=FALSE,drop=TRUE)  {
							
	breaks <- c(x@source$start,x@source$end[nrow(x@source)])
	nnl <- length(breaks)
	if (breaks[nnl]==breaks[nnl-1]) breaks[nnl] <- breaks[nnl-1]+1
	
	starts <- cut(i,breaks=breaks,right=FALSE,labels=breaks[-nnl],include.lowest=TRUE)
	starts <- as.integer(as.character(starts))
	ind <- i-starts+1
	names(i) <- sprintf("X%d",i)
	names(starts) <- names(i)
	names(ind) <- names(starts)
	inn <- names(i)
	
	
	ustarts <- unique(starts)
	names(ustarts) <- sprintf("F%d",ustarts)
	out <- list() ## creaTE a likToLIst ...
	
	for (it in names(ustarts)) {
		### TO DO IN THE AFTERNOON !!!!!
		sstart <- ustarts[it]
		il <- which(x@source$start==sstart)
		
		
		load(x@source$file[il])
		iss <- ind[starts==sstart]
		out[[it]] <- xs[iss]
		
		names(out[[it]]) <- names(iss)
		xs <- NULL
		
	}
	
	
	out <- do.call(what=c,args=out)
	
	names(out) <- sapply(X=str_split(names(out),"[.]"),FUN=function(tt){tt[[2]]})
	
	out <- out[inn]
	names(out) <- NULL
	###
	if (forceInMemory!=TRUE) {
		
		out <- linkToList(out,filename=filename,nperfile=nperfile,clean=clean,)
		
	} 
			
			
			
			
	return(out)
							
							
							
})



NULL
#' @name [[
#' @rdname extract-methods 
#' @method [[ LinkToList,ANY,ANY
#' @aliases [[,LinkToList,ANY,missing-method
#' @export
#' 
#' 



setMethod('[[',signature=c(x="LinkToList",i="ANY",j="missing"),function (x,i,...)  {
			
			
			
			out <- x[i,...,forceInMemory=TRUE]
			out <- out[[1]]
			
			return(out)
			
			
			
})

## INSERIRE METEODO GENERICO LENGTH ....
## https://stat.ethz.ch/R-manual/R-devel/library/base/html/length.html
#> help(.Primitive)
#> oo <- .Primitive("length")
#> oo
#function (x)  .Primitive("length")
#> oo(8)
#[1] 1
#> oo <- .Primitive("length")




NULL
#' @name length
#' @rdname extract-methods 
#' @method length  LinkToList
#' @aliases length,LinkToList-method
#' @export
#' 
#' 
setMethod("length",
		signature(x = "LinkToList"),
		function (x) 
		{
			
			out <- max(x@source$end)
			return(out)
			
		}
)





