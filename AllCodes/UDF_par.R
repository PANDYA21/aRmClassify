#### UDFs for multi core processing on one PC

library("parallel")
library("foreach")
library("doParallel")
library(iterators)
library(itertools)


### hack to mclapply on Windows with exporting the whole workspace to the cluster
mclapply.hackB <- function(...) {
  ## Create a cluster
  size.of.list <- length(list(...)[[1]])
  cl <- makeCluster(detectCores()-1) # ( min(size.of.list, detectCores()) )
  
  ## Find out the names of the loaded packages 
  loaded.package.names <- c(
    ## Base packages
    sessionInfo()$basePkgs,
    ## Additional packages
    names( sessionInfo()$otherPkgs ))
  
  tryCatch( {
    
    ## Copy over all of the objects within scope to
    ## all clusters. 
    this.env <- environment()
    while( identical( this.env, globalenv() ) == FALSE ) {
      clusterExport(cl,
                    ls(all.names=TRUE, env=this.env),
                    envir=this.env)
      this.env <- parent.env(environment())
    }
    clusterExport(cl,
                  ls(all.names=TRUE, env=globalenv()),
                  envir=globalenv())
    
    ## Load the libraries on all the clusters
    ## N.B. length(cl) returns the number of clusters
    parLapply( cl, 1:length(cl), function(xx){
      lapply(loaded.package.names, function(yy) {
        require(yy , character.only=TRUE)})
    })
    
    ## Run the lapply in parallel 
    return( parLapply( cl, ...) )
  }, finally = {        
    ## Stop the cluster
    stopCluster(cl)
  })
}
####


#### hack to mclapply on windows for saving memory
mclapplyWin <- function(x, fun, clust.exp = lsf(), ncores, 
                        envir = environment(), ...){
  if(missing(ncores)){
    library(parallel)
    ncores <- detectCores()-1
  }
  cl <- makeClusterB(ncores)
  clusterExport(cl = cl, list = clust.exp, envir = envir)
  tryCatch( {
    return(parLapply(cl = cl, x = x, fun = fun, ...))
  }, finally = {
    stopCluster(cl)
  })
}


####
foreachB <- function(...,x , fun){
  require("parallel")
  require("foreach")
  require("doParallel")
  require("doSNOW")
  
  # cl <- makeCluster(detectCores() - 1)
  # registerDoParallel(cl, cores = detectCores() - 1)
  cl <- makeCluster(detectCores() - 1, outfile="")
  registerDoSNOW(cl)
  
  ## export whole environment to cluster => RAM hungry 
  clusterExport(cl,
                ls(all.names=TRUE, env=globalenv()),
                envir=globalenv())
  
  tryCatch({
    this.env <- environment()
    while(identical( this.env, globalenv() ) == FALSE ){
      return(foreach(..., ii = isplitRows(data.table(x),
                                          chunks=detectCores()-1)) %dopar% {fun(ii)})
    }
  }, finally = {
      stopCluster(cl)
  })
}
####



#### make a cluster of ncores-1 size
makeClusterB <- function(ncores){
  require("parallel")
  require("foreach")
  require("doParallel")
  require("doSNOW")
  if(missing(ncores)){
    ncores <- detectCores() - 1
  }
  # cl <- makeCluster(detectCores() - 1)
  # registerDoParallel(cl, cores = detectCores() - 1)
  cl <- makeCluster(ncores, outfile="")
  registerDoSNOW(cl)
  return(cl)
}

####


### parLapply modified
doParB <-function(list.exp, exp.func, load.pack, par.func, x, fun, inFor){
  library("parallel")
  library("foreach")
  library("doParallel")
  library("iterators")
  library("itertools")
  require("parallel")
  require("foreach")
  require("doParallel")
  require("doSNOW")
  
  ncores <- detectCores() - 1
  cl <- makeCluster(ncores, outfile="")
  registerDoSNOW(cl)
  
  ## tryCatch-return and finally-stop
  tryCatch( {
    if(missing(list.exp)){
      if(exp.func == TRUE){
        clusterExport(cl = cl, list = lsf(), envir = .GlobalEnv)
      }else{
        clusterExport(cl = cl, list = ls(envir = .GlobalEnv), envir = .GlobalEnv)
      }
    }else{
      clusterExport(cl = cl, list = list.exp, envir = .GlobalEnv)
    }
    
    if(load.pack == TRUE){
      ## get the names of the loaded pakages from the global environment
      loaded.package.names <- c(sessionInfo()$basePkgs, names( sessionInfo()$otherPkgs ))
      ## Load the libraries on all the clusters
      parLapply(cl, 1:length(cl), 
                function(xx){
                  lapply(loaded.package.names, function(yy) {
                    require(yy , character.only=TRUE)})
                })
    }
    
    if(par.func == "parLapply"){
      #### do parLapply from snow package
      return(parLapply(cl = cl, x = x, fun = fun) )
      
    }else if(par.func == "foreach"){
      ### do foreaqch from foreach package
      return(foreach(ii = isplitRows(data.table(x),
                                     chunks=detectCores()-1)) %dopar% {
                                       # fun(ii)
                                       eval(inFor, envir = .GlobalEnv)
                                    })
    }
    
  }, finally = {        
    stopCluster(cl)
  } )
}




