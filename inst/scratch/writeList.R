# TODO: Add comment
# 
# Author: cordaem
###############################################################################

NULL
#' 
#' It writes the list (e.g. the one of a \code{\link{RasterList-class}} object) into a set of files in order to save memory (e.g \class{\code{\link{LinkToList-class}}. 
#' 
#' 
#' @param x a \code{list} object
#' @param filename a character name using as a root fro the file names. 
#' @param nperfile maximum number of files 
#' @param ... further arguments for \code{\link{save}}
#' 
#' 
#' @importFrom raster rasterTmpFile
#' @importFrom stringr str_replace
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
#' ll <- writeList(x)
#' 
#' filename <- rasterTmpFile("test100")
#' 
#' ll2 <- writeList(x,nperfile=100)
#' 
#' 
#' 

writeList <- function (x,filename=rasterTmpFile('ll'),nperfile=length(x),...) {
	
	
	
	if (!is.list(x)) stop("error x should be a list!")
	
	out <- 
	
	if (class(x)!="LinkToList") {
	
	
		##out <- list()
		filen <- filename
		fileList <- str_replace(filen,extension(filen),"_list_%d_%d.rda")
	
		nl <- length(x)
		listInFiles <- TRUE
		start <- as.integer(seq(1,nl,by=nperfile))
	
		out$listfiles <- data.frame(start=start)
	
	
	
		out$listfiles$end <-  out$listfiles$start-1
		out$listfiles$end <- c(out$listfiles$end[-1],nl)
  
		out$listfiles$file <- sprintf(fileList,out$listfiles$start,out$listfiles$end) ### TO CONTINUE TOMORROW
		
	
	
	} else {
		
		
			warning("This functionality has not been yet implemented!")	
			
			
	}	
	
	
	apply(X=out$listfiles,FUN=function(v,x,...){  
				
				ee <- as.integer(v["start"]):as.integer(v["end"])
				xs <- x[ee]
				
				o <- save(xs,file=v["file"],...)  
				return(o)
			},x=x,MARGIN=1,...)
				
	
	return(out)
	
	
}


