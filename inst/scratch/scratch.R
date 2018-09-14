# TODO: Add comment
# 
# Author: cordaem
###############################################################################
##install.packages("Rmpi") 
###>install.packages("snow")

if (is.loaded("mpi_initialize")) {
	message("fn is a string, gives better performance, will not check if valid")
} else {
	fn      <- match.fun(fn)
}
	
	nnodes.pc <- parallel::detectCores()
	if (is.loaded("mpi_initialize")) {
		if (verbose) message("[ MPI environment detected ]")
		nnodes.pc <- mpi.universe.size()
	} # IF End
	
	if (parallel=="parallel") {
		if (is.loaded("mpi_initialize")) {
			if (verbose) message("[ spinning up MPI cluster ]")
			ifelse(write2disk, 
					cl <- makeMPIcluster(outfile=logfile.fname),
					cl <- makeMPIcluster() )         
			pckgFn <- function(packages) {
				for(i in packages) library(i, character.only = TRUE)
			} # 'packFn' END
			clusterCall(cl, pckgFn, par.pkgs)
		} else { # if no mpi environment then fork cluster
			ifelse(write2disk,
					cl <- makeForkCluster(nnodes = par.nnodes, outfile=logfile.fname),
					cl <- makeForkCluster(nnodes = par.nnodes) )
		}
	} else if (parallel=="parallelWin") {      
		ifelse(write2disk,
				cl <- makeCluster(par.nnodes, outfile=logfile.fname),
				cl <- makeCluster(par.nnodes) )
		pckgFn <- function(packages) {
			for(i in packages) library(i, character.only = TRUE)
		} # 'packFn' END
		parallel::clusterCall(cl, pckgFn, par.pkgs)
		parallel::clusterExport(cl, ls.str(mode="function",envir=.GlobalEnv) )
		if (fn.name=="hydromod") {
			parallel::clusterExport(cl, model.FUN.args$out.FUN)
			parallel::clusterExport(cl, model.FUN.args$gof.FUN)
		} # IF end                   
	} # ELSE end       