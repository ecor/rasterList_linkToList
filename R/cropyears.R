# TODO: Add comment
# 
# Author: cordaem
###############################################################################
NULL

#' @title Crop a monthly or daily or sub-daily time series to a series starting from the beginning of the year and ending of the end of an year.
#' 
#' @description It crops a time series to a one staring from the beginning of the year and ending at the end of the year.
#'
#'
#' 
#' @param object time series to crop (or object containing time series to crop) 
#' @param start start date of time series, used  if \code{object} is missing or \code{NULL}
#' @param end end  date of time series, used  if \code{object} is missing or \code{NULL}
#' @param months_offset offset expressed in number of months from January 1st, used if the year is chosen to start from a different from January. 
#' @param timestamp character string containing the data frame field name for the date. Default is \code{"Date"}.
#' @param format_date string character used for date. Default is \code{"X\%Y.\%m.\%d"}.
#' @param ... further arguments
#' 
#' 
#' 
#' @name cropyears
#' @aliases cropyears
#' @rdname cropyears
#' @export
#' 
#' 
#' @importFrom lubridate ceiling_date floor_date days
#' @importFrom raster subset
#' 
#' 
#' @examples 
#'
#' start <- as.Date("1988-09-09")
#' end <-   as.Date("2016-02-02")
#' cropdates <- cropyears(start=start,end=end)
#' 
#' start <- as.Date("1979-01-01")
#' end <-   as.Date("2016-12-31")
#' cropdates <- cropyears(start=start,end=end)
#' 
#' 
#' format_date <- "X%Y.%m.%d"
#' dates <- start+1:700
#' vecn <- rnorm(700)
#' names(vecn) <- as.character(dates,format=format_date)
#' cropvect <- cropyears(vecn,format_date=format_date)
#' 
#' 
#' t2m <- stack(system.file("map/t2m_daily.grd",package="rasterList"))
#' cropt2m <- cropyears(t2m)
#' 
#' 
#' 
#' 

cropyears <- function (object=NULL,...)  {
	
	
	return(standardGeneric("cropyears"))
	
}


NULL
#' @title cropyears
#' @rdname cropyears
#' @method cropyears default
#' @aliases cropyears 
#' @export


setGeneric("cropyears",function (object=NULL,start,end,months_offset=c(0,6),...)  {
			
			
			out <- NULL
			if (start>end) {
				dd <- start 
				start <- end
				end <- dd
				
				
				
			}
			if (is.null(object)) {
				
				object <- seq(from=start,to=end,by="days")
				out <- cropyears(object=object,months_offset=months_offset,...)
			
			} else {
				
				out <- NULL
			}
			
			return(out)
		})





NULL
#' @title cropyears
#' @rdname cropyears
#' @method cropyears Date
#' @aliases cropyears 
#' @export

setMethod("cropyears","Date",function (object=NULL,months_offset=c(0,6),...)  {
			
			
			
			start <- min(object)
			end   <- max(object)
			
			months_offset <- months_offset[1]
			
			start_n <- ceiling_date(as.Date(start)-months(months_offset)-days(1),"year")+months(months_offset)
			end_n <- floor_date(as.Date(end)-months(months_offset)+days(1),"year")+months(months_offset)
			
			
			out <- object[(object)>=start_n & object<=end_n]
			
			return(out)
			
		})

NULL
#' @title cropyears
#' @rdname cropyears
#' @method cropyears data.frame
#' @aliases cropyears 
#' @export

setMethod("cropyears","data.frame",function (object=NULL,timestamp="Date",...)  {
			
			if (!(timestamp %in% names(object))) {
				
				stop("Error in Cropyears, no timestamp found")
				
			}
				
				
				
			dates <- object[,timestamp]
			
			iout <- which(dates %in% cropyears(dates,...))
			
			out <- object[,iout]
			
			
			return(out)
			
			
		})

NULL

#' @title cropyears
#' @rdname cropyears
#' @method cropyears character
#' @aliases cropyears 
#' @export

setMethod("cropyears","character",function (object=NULL,format_date="X%Y.%m.%d",...)  {
			
			n_object <- names(object)
			if (is.null(n_object)) n_object <- object
			
			dates <- as.Date(n_object,format=format_date)
			iout <- which(dates %in% cropyears(dates,...))
			out <- object[iout]
			
			
			return(out)
			
			
		})

NULL

#' @title cropyears
#' @rdname cropyears
#' @method cropyears numeric
#' @aliases cropyears 
#' @export

setMethod("cropyears","numeric",function (object=NULL,format_date="X%Y.%m.%d",...)  {
			
			n_object <- names(object)
			if (is.null(n_object)) {
				
				stop("No mames or tags incating dates have been found!")
				
			}
			
			dates <- as.Date(n_object,format=format_date)
			iout <- which(dates %in% cropyears(dates,...))
			out <- object[iout]
			
			
			return(out)
			
			
		})









NULL
#' @title cropyears
#' @rdname cropyears
#' @method cropyears RasterStack
#' @aliases cropyears 
#' @export


setMethod("cropyears","RasterStack",function (object=NULL,format_date="X%Y.%m.%d",...)  {
			
			dates <- as.Date(names(object),format=format_date)
			
			iout <- which(dates %in% cropyears(dates,...))
			
			out <- subset(object,subset=iout)
			
			
			return(out)
			
			
		})

NULL
#' @title cropyears
#' @rdname cropyears
#' @method cropyears RasterBrick
#' @aliases cropyears 
#' @export


setMethod("cropyears","RasterBrick",function (object=NULL,format_date="X%Y.%m.%d",...)  {
			
			
			out <- stack(object)
			
			
			return(out)
			
			
		})

NULL
#' @title cropyears
#' @rdname cropyears
#' @method cropyears list
#' @aliases cropyears 
#' @export


setMethod("cropyears","list",function (object,...)  {
			
			out <- lapply(X=object,FUN=cropyears,...)
			
			
			return(out)
			
			
		})
NULL
#' @title cropyears
#' @rdname cropyears
#' @method cropyears RasterList
#' @aliases cropyears 
#' @export


setMethod("cropyears","RasterList",function (object=NULL,format_date="X%Y.%m.%d",...)  {
			
			out <- object
			dates <- as.Date(names(object),format=format_date)
			
			iout <- which(dates %in% cropyears(dates,...))
			
			out <- list[object]
			
			
			
			
			
		})




