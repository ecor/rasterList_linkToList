# TODO: Add comment
# 
# Author: cordaem
###############################################################################


NULL


#' @title Creates a \code{\link{LinkToList-class}}  object
#' 
#' @description The method \code{LinkToList} is the constructor of a  \code{\link{LinkToList-class}} from a generic \code{object}. 
#'
#' 
#' 
#' @param object the object to coerce 
#' @param filename a character name using as a root for the file names. 
#' @param nperfile maximum number of \code{*.rda} files 
#' @param length length of the \code{\link{LinkToList-class}}  object  to be created (e.g. if \code{object} is \code{NULL} or \code{list()}) 
#' @param FUN function that can be optionally processed per each element of the \code{\link{LinkToList-class}} object 
#' @param suffix suffix that can be optionally applied if \code{FUN} is not \code{NULL}. 
#' @param single.element logical parameter. Dafault is \code{TRUE}. If it is \code{TRUE} , \code{FUN} is appled to ach single element of the list, otharwise \code{FUN} is applied to teach entire \code{\link{RasterList-class}} block of elements.
#' @param ... further arguments , e.g. for \code{\link{save}}
#' 
#' @details 
#' 
#' This methods is used to write a list (e.g. the one of a \code{\link{RasterList-class}} object) into a set of files in order to save memory, 
#' the \code{\link{LinkToList-class}} object.
#' 
#'
#' @importFrom raster rasterTmpFile
#' @importFrom stringr str_replace str_replace_all
#' @importFrom raster extension
#' @importFrom raster extension
#' 
#' 
#' @rdname linkToList
#' @export
#' 
#' 
#' 
#' @examples 
#' 
#' 
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
linkToList <- function (object=NULL,...)  {
	
	
	return(standardGeneric("linkToList"))
	
}

NULL
#' @title linkToList
#' @rdname linkToList
#' @aliases linkToList 
#' @export
#' 

as.LinkToList <- function(object,...) {
	
	
	return(linkToList(object,...))
	
	
}

NULL
#' @title linkToList
#' @rdname linkToList
#' @method linkToList default
#' @aliases linkToList 
#' @export


setGeneric("linkToList",function (object=NULL,filename="default",FUN=NULL,nperfile=100,length=200,...)  {
			
			
	
			object <- list()
			out <- linkToList(object,filename=filename,nperfile=nperfile,FUN=FUN,length=length,...)
			
			####out@source <- NULL
			return(out)
			
			
			
		})



NULL
#' @title linkToList
#' @rdname linkToList
#' @method linkToList LinkToList
#' @aliases linkToList 
#' @export


setMethod("linkToList","LinkToList",function (object,filename="default",FUN=NULL,nperfile="not.used",suffix="default",...)  {
			
	        
			if (is.null(suffix)) suffix <- ""
			if (is.na(suffix))   suffix <- "default"
			
			if (is.null(filename)) filename <- NA
			
			
			lu2 <- LinkToList2list(lu)
			
			if (!is.null(FUN)) {
				
				if (is.null(suffix)) suffix <- ""
				if (is.na(suffix))   suffix <- "default"
				
				if (suffix=="default") {
					
					suffix <- sprintf("_%s_",str_replace_all(tempfile(pattern="fun",tmpdir=".",fileext=""),"./","")[1])
				}
				
				vv <- object@source$file
				suffixa <- "_list_"
				suffixm <- paste0(suffix,suffixa)
				object@source$file <- str_replace_all(vv,suffixa,suffixm)
				names(vv) <- object@source$file
				for (it in object@source$file) {
				
					load(vv[it])
				
					xs <- lapply(X=xs,FUN=FUN,...)
					
					
					save(xs,file=it)  

				}
				
				
			}
			
			if (is.null(filename)) filename <- NA
			
			if (!is.na(filename)) {
				
				if (filename=="default") filename <- rasterTmpFile() ## ('ll')
				
				#filen <- filename
				#fileList <- str_replace(filen,extension(filen),"_list_%d_%d.rda")
				filen <-  str_split(filename,"[.]")[[1]]
				filen <-  filen[-base::length(filen)]
				filen <-  paste(filen,collapse=".")
				fileList <-  paste0(filen,"_list_%d_%d.rda")
				
				out_source_file <- sprintf(fileList,object@source$start,object@source$end)
				
				names(out_source_file) <- object@source$file
			
				
				for (it in object@source$file) {
					
					from <- it
					to <- out_source_file[it]
					ofr <-try(file.copy(from=from, to=to,overwrite=TRUE),silent=TRUE)
				
					if (ofr!=TRUE)  { 
						out_source_file[it] <- it
						msg <- sprintf("%s was not copied as %s and %s's content has been replaced!",from,to,from)
						warning(msg)
					} 	
					
				}
				
				object@source$file <- out_source_file
				
				
			}
			
			
			
			
			return(object)
			
			
			
		})


NULL
#' @title linkToList
#' @rdname linkToList
#' @method linkToList list
#' @aliases linkToList 
#' @export

setMethod("linkToList","list",function (object,filename="default",FUN=NULL,nperfile=length,length=length(object),single.element=TRUE,...)  {
			
		
			lnn <- base::length(object)
			
			if (base::length(object)>0) length <- base::length(object)
			
			if (is.null(filename)) filename <- NA
			filename <- filename[1]
			if (is.na(filename[1])) filename <- "default" 
			
			
			if (filename=="default")  filename <- rasterTmpFile() ##('ll')
		
			out <- as(list(),"LinkToList")## EC 20180621
			
			filen <-  str_split(filename,"[.]")[[1]]
			filen <-  filen[-base::length(filen)]
			filen <-  paste(filen,collapse=".")
			fileList <-  paste0(filen,"_list_%d_%d.rda")
		
						
			nl <- length
			if (nperfile>=nl) nperfile <- nl 			
			start <- as.integer(seq(1,nl,by=nperfile))
			out@source <- data.frame(start=start)
			out@source$end <-  out@source$start-1
			out@source$end <- c(out@source$end[-1],nl)
			out@source$file <- sprintf(fileList,out@source$start,out@source$end) ### TO CONTINUE TOMORROW
			
		
			apply(X=out@source,FUN=function(v,x,funx,use.lapply,single.element,...){  
						
						ee <- as.integer(v["start"]):as.integer(v["end"])
					
						if (length(x)>0) {
							xs <- x[ee]
						} else {
							
							xs <- ee
						}	
					
						if (!is.null(FUN) & (single.element==TRUE)) {
							xs <- lapply(X=xs,FUN=funx,...) ## FARE UN APPLY QUI !!!!
						} else if (!is.null(FUN)) {
							
							xs <- funx(xs,...)
							
						}
					
						o <- save(xs,file=v["file"])  
						return(o)
					},x=object,MARGIN=1,funx=FUN,single.element=single.element,...)
			
			
	
			return(out)
			
			
			
			
		})



