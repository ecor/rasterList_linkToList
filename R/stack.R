# TODO: Add comment
# 
# Author: cordaem
###############################################################################


#' @title Creates a \code{\link{RasterStack-class}}  object from a \code{\link{RasterList-class}}
#' 
#' @description The method transforms a \code{\link{RasterList-class}} into a \code{\link{RasterStack-class}} in case of the list elements are numeric vectors. 
#'
#' 
#' 
#' @param x a \code{rasterList-class} object 
#' @param filename,... further arguments for \code{\link{rasterList}}
#' 
#' @rdname stack
#' @method stack RasterList
#' @aliases stack 
#' @export
#' 
#' @importFrom raster ncell stack
#' @importFrom methods as
#' 
#' 
#' @seealso \code{\link{rasterList}}
#' 
#' @examples 
#'
#' f <- system.file("external/test.grd", package="raster")
#' 
#' ## Creates a simple generic RasterList
#' 
#' rl <- rasterList(f) 
#' 
#' list <- as.list(as.vector(rl))
#' list <- lapply(X=list,FUN=function (x) {c(x,x+10,x+15)}) 
#' 
#' rl <- rasterList(rl,list=list,object.name="test")
#' 
#' ss <- stack(rl)
#' 
#' il <- 8331
#' list[[il]] <- numeric(0)
#' rla <- rasterList(rl,list=list,object.name="test2")
#' sa <- stack(rla) 


setMethod("stack", signature(x='RasterList'), 
		function(x, filename="default",...) {
			
			if (length(filename)!=0) filename <- "default"
			if (filename[1]=="") filename <- "default"
			filename <- filename[1]
			
			out <- rasterList(object=x,filename=filename,...) ## check HERE !!!!
			
			if (length(out@list)==0) {
				
				out@list <- as.list(out[,])
				
			}
	
			
			####
			
			length__ <- function(x,...) {
				
				
				## THIS TO DO ## Neew to manage 0-length element as non-NULL entities
				## BROWSE ON WEB!!!
				
			}
			
			
			
			if (class(out@list)=="LinkToList") {
			
				## DO SOMETHING
				nn <- linkToList(object=out@list,FUN=names,filename="default")
			
				snn <- nn[[1]]
				
				l <- linkToList(object=out@list,FUN=as.vector)  ## TO work here to recreate a stack from a rasterList
			
				lnl <- length(l)
		
				lu <- linkToList(object=l,FUN=base::length)
				
				lu2 <- LinkToList2list(lu)
			##	lu2 <- LinkToList2list(lu,clean=TRUE)
			
				
				nl <- unlist(lu2)
				
				
	
				
			} else {
				
				nn <- lapply(X=out@list,FUN=names)
				snn <- nn[[1]]
				l <- lapply(X=out@list,FUN=as.vector)  ## TO work here to recreate a stack from a rasterList
				lnl <- length(l)
			
				nl <- sapply(X=l,FUN=base::length)
				
			}
			
			cond_inz <- nl!=0 & nl!=1
			if (all(cond_inz==FALSE)) cond_inz <- nl!=0
			
			cond_iz <- !cond_inz
			inz <- which(cond_inz)
		##	iz <-  which(nl==0 | nl==1)
			iz <- which(cond_iz)
			
	
			if (max(nl[inz],na.rm=FALSE)!=min(nl[inz],na.rm=FALSE)){
			
				if (length(out@name)<1) out@name <- "<NO_NAME>"
				r <- range(nl[inz])
			#	if (r[1]==-Inf) r[1] <- -9999
			#	if (r[1]==+Inf) r[2] <- +9999
				r <- as.integer(r)
			
				msg <- sprintf("RasterList-class Object %s cannot be coerced into a RasterStack-class object (%d %d)",out@name,r[1],r[2])
				
				stop(msg)
			}
			snn <- nn[[inz[1]]] ## EC20180226
			
			
			
			### modified by EC on 20180718
			
			if (length(iz)>0) {
				
				
				
				for (i in iz){
					
					val_x <- l[[i]]
				
					if (length(val_x)<1) val_x <- NA
				
					val_x <- array(val_x[1],nl[inz][1])
					
					l[[i]] <- val_x # -99999 ##val_x
					
				}
				
				
			}
			
			
			if (!is.null(snn)) {
				
				cond_names <- all(sapply(X=nn[inz],FUN=identical,y=snn))
			
			} else {
			
				cond_names <- FALSE
			}	
					
			## NEW CODE 
			
			nlayers <- nl[inz][1]
		
		
			
			rr <- as(out,"RasterLayer")
			
			
			if (class(l)=="LinkToList") {
				
				
				
				 out <- lapply(X=1:nlayers,FUN=function(i,l,rr){
						     			
							 uu <- linkToList(l,filename="default",FUN=function(x,i){(x[i])},i=i)
							 
							
						
							 
							 starts <- uu@source$start
							 ends <-  uu@source$end
					
							 for (ii in 1:length(starts)) {
								 
								 cells <- starts[ii]:ends[ii]
								
								 
								
								
								
								 rr[cells] <- unlist(uu[cells,forceInMemory=TRUE,clean=TRUE])
								 
								 
							 }
						#####	 unlink(uu@source$file) ## CREATE clean or remove.FILE function!!
							 return(rr)
					    },l=l,rr=rr)
				
						unlink(l@source$file) ## deleta all Files
				
				
			} else {
			
			
			
			
			## ENDD NEW CODE
			
			## HERE DATA ARE PUT IN A RASTER!!!!	
			
				l <- array(unlist(l),c(nl[inz][1],lnl))
			
			
			
				l <- apply(X=l,FUN=as.list,MARGIN=1)
				l <- lapply(X=l,FUN=unlist)
				out <- lapply(X=l,FUN=function(x,ra) { 
					o <- raster(ra) 
					o[] <- x
					
					return(o)
					},ra=out)
			
			}
			
			
			out <- stack(out)
			
			if (cond_names==TRUE) names(out) <- snn
			
			return(out)
		}
)
