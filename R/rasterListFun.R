NULL
#' Execution of the elements of a \code{RasterList} 
#' 
#' This fuction transmors a generic  \code{\link{RasterList-class}} object into another \code{\link{RasterList-class}} object where elemets are all \code{function}-type. 
#' 
#' @param object  an object to be coerced to \code{\link{RasterList-class}} 
#' @param ... further arguments for function contained in \code{object} 
#' 
#' @return This function works with \code{RasterList-class}  objects in which all elements of \code{object@list} slot are functions. It returns a "global" function that works at "raster" scale. The returned function will have the following usage signature: \code{fun(xval,...)} where one \code{xval} (if its lengths is different from 1) element is the applied to each element and ... are further common arguments.  
#' 
#' @export
#' 
#' @examples
#' 
#' library(sp)
#' library(rasterList)
#' library(soilwater)
#' set.seed(1234)
#' data(meuse.grid)
#' data(meuse)
#' coordinates(meuse.grid) <- ~x+y
#' coordinates(meuse) <- ~x+y
#' gridded(meuse.grid) <- TRUE
#' 
#' \dontrun{
#' soilmap <- stack(meuse.grid)[['soil']]
#' elevmap <- rasterize(x=meuse,y=soilmap,field="elev",fun=mean)
#' soilparcsv <- system.file("external/soil_data.csv",package="soilwater")
#' soilpar <- read.table(soilparcsv,stringsAsFactors=FALSE,header=TRUE,sep=",")
#' ## From help(meuse,help_type="html")
#' ##soil type according to the 1:50 000 soil map of the Netherlands. 
#' ## 1 = Rd10A (Calcareous weakly-developed meadow soils, light sandy clay); 
#' ## 2 = Rd90C/VII (Non-calcareous weakly-developed meadow soils, heavy sandy clay to light clay); 
#' ## 3 = Bkd26/VII (Red Brick soil, fine-sandy, silty light clay)
#' soiltype_id <- c(1,2,3)
#' soiltype_name <- c("sandy clay","sandy clay","silty clay loam")
#' 
#' meuse.soilrasterlist <- rasterList(soilmap,FUN=function(i,soiltype_name,soilpar){
#' 			
#' 		o <- NULL
#' 		if (!is.na(i)) {
#' 			ii <- which(soilpar$type==soiltype_name[i])	
#' 		    o <- soilpar[ii,]				
#' 			type <- o[["type"]]
#' 			o <- o[names(o)!="type"]
#' 			o <- o[names(o)!="Ks_m_per_hour"]
#' 			names(o)[names(o)=="Ks_m_per_sec"] <- "ks"
#' 			names(o)[names(o)=="swc"] <- "theta_sat"
#' 			names(o)[names(o)=="rwc"] <- "theta_res"
#' 			attr(o,"type") <- type
#' 			## add noise
#' 			noise <- rnorm(length(o))
#' 			o <- o*(1+0.005*noise)
#' 				
#' 			o["m"] <- 1-1/o["n"]
#' 				
#' 				
#' 		} else {
#' 				
#' 			o <- soilpar[which(soilpar$type==soiltype_name[1]),]
#' 			type <- o[["type"]]
#' 			o <- o[names(o)!="type"]
#' 			o <- o[names(o)!="Ks_m_per_hour"]
#' 			names(o)[names(o)=="Ks_m_per_sec"] <- "ks"
#' 			names(o)[names(o)=="swc"] <- "theta_sat"
#' 			names(o)[names(o)=="rwc"] <- "theta_res"
#' 			o[] <- NA
#' 		}
#' 			
#' 		return(o)
#' },soiltype_name=soiltype_name,soilpar=soilpar)
#' 
#' 
#' meuse.swclist <- rasterList(meuse.soilrasterlist,FUN=function(x) {
#' 			
#' 			o <- NA
#' ##			swc       rwc   alpha       n         m           ks
#' ##			9 0.4295507 0.1093227 3.39387 1.39617 0.2837546 2.018317e-07
#'			
#' 			
#' 			o <- function(psi,...,func="swc"){
#' 				
#' 				args <- c(list(psi=psi,...),as.list(x))
#' 				oo <- do.call(args=args,what=get(func))
#' 				return(oo)
#' 				
#' 			}
#' 			
#' 			
#' 			
#' 			
#' 			
#' 			
#' 			
#' 			return(o)
#' 			
#' 		})
#' 
#' 
#' ### RasterList with soil water retenction curves (One for each cell!) 
#' 
#' swcfunr <- rasterListFun(meuse.swclist)
#' 
#' 
#'
#' ## RasterLayer of soil water content from  a generic map of soil water pressure head
#' psi <- 0.2-(elevmap-(5))
#' psi[] <- -0.9+0.1*rnorm(ncell(psi[])) ## Alternatively to the values of the previous line!
#' soil_water_content <- raster(swcfunr(psi))
#' plot(soil_water_content)
#' 
#' 
#' 
#' ## RasterList-objact with data saved into files:
#' 
#' meuse.swclist2 <- rasterList(meuse.swclist,filename="default",nblocks=20)
#' swcfunr2 <- rasterListFun(meuse.swclist2)
#' ## RasterLayer of soil water content assuming a uniformly distrrubted pressure head 
#' psi <- -0.9
#' soil_water_content2 <- (swcfunr2(psi))
#' filename <- rasterTmpFile("swc2")
#' soil_water_content2 <- (swcfunr2(psi,filename=filename))
#' plot(raster(soil_water_content2))
#' ## END 
#' }
#' 
 
 rasterListFun <- function(object,...)  {
 	
 	
 ##CC 20190718	object <- rasterList(object)
	
	if (class(object@list)=="LinkToList") {
		tmpvv <- linkToList(object=object@list,FUN=function(xv){class(xv)=="function"},...)
		condfun <- all(unlist(LinkToList2list(tmpvv)))
		
	} else {
		
		condfun <- all(sapply(X=object@list,FUN=function(xv){class(xv)=="function"}))
	}
	####
	
	
	
	#####
	
	
 	if (condfun==FALSE) {
 		
 		msg <- sprintf("RasterList %s contains non-function objects!!",object@name)
 		stop(msg)
		
		
	}
	out <- function(xval,filename="temporary",...) {
		
		if (is.numeric(xval)) { 
		
			o <- rasterList(object,FUN=function(tt,...,xval=xval){tt(xval,...)},filename=filename,...,xval=xval)
			
			
		} else {	
			
			o <- RasterListApply(a=object,b=rasterList(xval),FUN=function(a,b,...){
					
					args <- list(b,...)
					what <- a
					oo <- do.call(what=what,args=args)
					return(oo)
				},filename=filename,...)
		}
		return(o)
		
	}
	
	
	return(out)
	
}







