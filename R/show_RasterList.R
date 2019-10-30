
#' @title Print or Show S4 method for a \code{\link{RasterList-class}} object
#' 
#' @description Prints a \code{\link{RasterList-class}} object  
#' 
#' @param object a \code{\link{RasterList-class}} object 
#'   
#' 
#' @rdname show 
#' @method show RasterList
#' @aliases show,RasterList
#' @export
#' 
#' 

#
#
#setMethod ('show' , 'RasterLayer', 
#		function(object) {
#			cat('class       :' , class(object), '\n')
#			if (rotated(object)) {
#				cat('rotated     : TRUE\n')
#			}
#			if (nbands(object) > 1) { 
#				cat('band        :' , bandnr(object), ' (of ', nbands(object), ' bands)\n')	
#			}	
#			cat('dimensions  : ', nrow(object), ', ', ncol(object), ', ', ncell(object),'  (nrow, ncol, ncell)\n', sep="" ) 
#			cat('resolution  : ' , xres(object), ', ', yres(object), '  (x, y)\n', sep="")
#			cat('extent      : ' , object@extent@xmin, ', ', object@extent@xmax, ', ', object@extent@ymin, ', ', object@extent@ymax, '  (xmin, xmax, ymin, ymax)\n', sep="")
#			cat('coord. ref. :' , projection(object, TRUE), '\n')
#			
#			
#			if (hasValues(object)) {
#				fd <- object@data@fromdisk
#				if (fd) {
#					cat('data source :', filename(object), '\n')
#				} else {
#					cat('data source : in memory\n')			
#				}
#				cat('names       :', names(object), '\n')
#				if (object@data@haveminmax) {
#					cat('values      : ', minValue(object), ', ',  maxValue(object), '  (min, max)\n', sep="")
#				}
#			}
#			
#			
#			if (is.factor(object)) {
#				
#				x <- object@data@attributes[[1]]
#				nc <- NCOL(x)
#				
#				# this can actually happen, but x should be a data.frame anyway
#				#if (nc == 1) { # this should never happen
#				#	x <- data.frame(value=x)
#				#}
#				
#				maxnl <- 12
#				if (nc > maxnl) {
#					x <- x[, 1:maxnl]
#				}
#				
#				
#				#nfact <- sapply(1:ncol(x), function(i) is.numeric(x[,i]))
#				if (nrow(x) > 5) {
#					cat('attributes  :\n') 
#					r <- x[c(1, nrow(x)), ,drop=FALSE]
#					for (j in 1:ncol(r)) {
#						r[is.numeric(r[,j]) & !is.finite(r[,j]), j] <- NA
#					}	
#					r <- data.frame(x=c('from:','to  :'), r)
#					a <- colnames(x)
#					
#					colnames(r) <- c('    fields :', a)
#					colnames(r) <- c('', a)
#					rownames(r) <- NULL
#					if (nc > maxnl) {
#						r <- cbind(r, '...'=rbind('...', '...'))
#					}
#					print(r, row.names=FALSE)
#				} else {
#					cat('attributes  :\n') 
#					print(x, row.names=FALSE)
#				}
#				
#				
#			} else {
#				
#				z <- getZ(object)
#				if (length(z) > 0) {
#					name <- names(object@z)
#					if (is.null(name)) name <- 'z-value'
#					name <- paste(sprintf("%-12s", name), ':', sep='')
#					cat(name, as.character(z[1]), '\n')
#				}
#				
#				if (object@file@driver == 'netcdf') {
#					z <- attr(object@data, 'zvar')
#					if (!is.null(z)) { cat('zvar        :', z, '\n') } 
#					z <- attr(object@data, 'level')
#					if (!is.null(z)) { 
#						if (z>0) { 
#							cat('level       :', z, '\n')  
#						}
#					}
#				}
#			}
#			cat ('\n')
#		}
#)
#


setMethod("show",
    signature(object = "RasterList"),
    function (object) 
    {
        
		out <- raster(object)
		show(out)
		if (isS4(object@list)) {
			show(object@list)
			
		} else {
			print(object@list)
			
		}

		
		return(0)
		
    }
)
