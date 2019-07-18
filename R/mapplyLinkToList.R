NULL
#' Use of \link{mapply} functions with \link{LinkToList-class} objects
#' 
#' @param ... arguments
#' @param filename argument passed to \code{\link{linkToList}}.
#' 
#' 
#' 
#' @export
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
#' is.list(ll2)
#' 
#' ll3 <- linkToList(ll2,nperfile=100,FUN=mean)
#' ll3a <- linkToList(ll2,nperfile=100,FUN=mean,filename="/tmp/mapname.asc")
#' ll4 <- linkToList(x,nperfile=100,FUN=mean)
#' 
#' 
#' ll5 <- mapplyLinkToList(ll3,ll4,3,FUN=sum)
#' 
#' ll5a <- mapplyLinkToList(as.list(ll3),as.list(ll4),3,FUN=sum)
#' 
#' as.list(ll5)
#' ll5a
#' 


mapplyLinkToList <- function (...,filename="default")   {
	
	
	args <- list(...)
	
	###
	lout <- sapply(X=args,FUN=length)
	
	cond <- lout==1 | lout==max(lout)	
	
	cond <- all(cond)==TRUE
	
	
	if (cond==FALSE) {
		
		
		stop("Arguments have different lengths (sizes) !!!")
		
	}
	
	
	
	####
	cout <- sapply(X=args,FUN=class)
	
	coo <- which(cout=="LinkToList")
	
	if (length(coo)>0)  {
		
		out <- linkToList(args[coo][[1]],filename=filename)
		nr <- nrow(out@source)
		iloutm <- which(lout>1)
	
	
		
		
		for (i in 1:nr)  {
			
			start <- out@source$start[i]
			end <- out@source$end[i]
			args_m <- args
		
			
			args_m[iloutm] <- lapply(X=args_m[iloutm],FUN=function(u,start,end) {
						
					
						o <- (u[start:end])
						
						return(o)
					},start=start,end=end)
			
			args_m$SIMPLIFY <- FALSE
			sso <- do.call(what=mapply,args=args_m)
			
			out[start:end] <- sso
			
		}
		
		
		
	
		
		
		
	}  else { 
	
		
		args$SIMPLIFY <- FALSE
		
		out <- do.call(what=mapply,args=args)
	}
	
	
	
	
	return(out)
	
	
	
	
	
	
	
}