
library(rasterList)


source('/STORAGE/projects/R-Packages/rasterList/R/extract.linktolist.R') 

set.seed(123)
 
x <- list()
n <- 1000 
for (i in 1:n) {
 
 		x[[i]] <- rnorm(100)
 }
 
ll <- linkToList(x) 
filename <- rasterTmpFile("test100")
 
ll2 <- linkToList(x,nperfile=100)

ll2[c(9,10,800)]
ll2[c(9,800,100)]
oo <- ll2[c(9,800,100)]

oo
stop("HERE")
x[c(9,10,800)] <- list("a","b","c")
ll2[c(9,10,800)] <- list("a","b","c")

if (identical(x,as.list(ll2))==FALSE) stop("x and ll2 should be identical!! Something went wrong!!")
  
	 