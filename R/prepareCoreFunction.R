#' @export
#' @name prepareCore
#' @title prepare number of cores to use
#' @author Haniel Cedraz
#' @details December 2021
#' @usage
#' prepareCore(nThreads = 8)
#' @description
#' prepare number of cores to use
#'
#' @param nThreads
#' \code{integer.} Number of processors to use. Default 8.
#' @importFrom parallel detectCores
#' @importFrom glue glue
#' @export


prepareCore <- function(nThreads = 8){
  # if opt_procs set to 0 then expand to samples by targets

  if (detectCores() < nThreads) {
    write(glue("The number of cores specified ({nThreads}) is greater than the number of cores available ({detectCores()})"), stdout())
    paste('Using ', detectCores(), 'threads')
    nThreads <- detectCores()
  }


  #if (detectCores() < opt$procs) nThreads <- detectCores()
  write(paste("Using", nThreads, "processors", sep = " "),stdout())
  return(nThreads)
}


#prepareCore(nThreads = 10)
