### https://stat.ethz.ch/R-manual/R-devel/library/parallel/doc/parallel.pdf
### http://www.stats.uwo.ca/faculty/yu/Rmpi/

## Exemplary variance filter executed on three different matrices in parallel.
## Can be used in gene expression analysis as a prefilter
## for the number of covariates.
rm(list=ls())
set.seed(1234)
library(parallel)
n <- 300   # observations
p <- 20000 # covariates
 ## Different sized matrices as filter inputs
 ## Matrix A and B form smaller work loads
 ## while matrix C forms a bigger workload (2*p)
library(stats)
A <- matrix(replicate( p,  rnorm(n, sd = runif(1, 0.1, 10))), n, p)
B <- matrix(replicate( p,  rnorm(n, sd = runif(1, 0.1, 10))), n, p)
C <- matrix(replicate(2*p, rnorm(n, sd = runif(1, 0.1, 10))), n, 2*p)
varFilter <- function (X, nSim = 20) {
	   for (i in 1:nSim) {
		    train <- sample(nrow(X), 2 / 3 * nrow(X))
		    colVars <- apply(X[train, ], 2, var)
		    keep <- names(head(sort(colVars, decreasing = TRUE), 100))
		     # myAlgorithm(X[, keep])
				}
			
	 }
 ## Runtime comparison -----------------------------------
		
		## mclapply with affinity.list
		## CPU mapping: A and B run on CPU 1 while C runs on CPU 2:
		affinity <- c(1,1,2)
		
ccl <- 		system.time(
				ool<- lapply(X = list(A,B,C)[2:3], FUN = varFilter))		
ccm <- system.time(
		   oos <- mclapply(X = list(A,B,C)[2:3], FUN = varFilter)) ###,
		##		            mc.preschedule = FALSE, affinity.list = affinity))
##   user  system elapsed
	 ## 34.909   0.873  36.720
	
	
	
