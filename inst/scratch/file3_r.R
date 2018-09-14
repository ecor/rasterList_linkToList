rm(list=ls())

library(rasterList)
source('/STORAGE/projects/R-Packages/rasterList/R/LinkToList.R')
set.seed(123)

x <- list()
n <- 1000
for (i in 1:n) {

		x[[i]] <- rnorm(5)
}

ll <- linkToList(x)

filename <- rasterTmpFile("test100")

ll2 <- linkToList(x,nperfile=100)

is.list(ll2)

ll3 <- linkToList(ll2,FUN=mean)
ll3a <- linkToList(ll2,nperfile=100,FUN=mean,filename="/tmp/mapname.asc")
ll4 <- linkToList(x,nperfile=100,FUN=mean)

