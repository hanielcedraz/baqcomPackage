#' @export
#' @name sampleList
#' @title Creating sample list to run mcapply parallel
#' @author Haniel Cedraz
#' @details December 2021
#' @usage
#' sampleList(samples, reads_folder, column, fileType, libraryType)
#' @description
#' Function to create sample list to run mcapply parallel
#'
#' @param samples
#' \code{Character.} The filename of the sample file. Default samples.txt.
#' @param reads_folder
#' \code{Character.} Directory where the raw sequence data is stored. Default 00-Fastq.
#' @param column
#' \code{Character.} Column name from the sample sheet to use as read folder names. Default SAMPLE_ID
#' @param fileType
#' \code{Character.} The file type to use. Available: 'fastq.gz', 'bam' or 'sam'.
#' @param libraryType
#' \code{Character.} The library type to use. Available: 'pairEnd' or 'singleEnd'.
#' @importFrom glue glue
#' @export




sampleList <- function(samples, reads_folder, column, fileType, libraryType) {

  samples <- as.data.frame(samples)
  aceptedFileTypes <- c("fastq.gz", "bam", "sam")


  if (!fileType %in% aceptedFileTypes) {
    stop(glue("File type ({fileType}) not found, please provide one of 'fastq.gz', 'bam' or 'sam'"))
  }

  aceptedLibraryType <- c("pairEnd", "singleEnd")
  if (!libraryType %in% aceptedLibraryType) {
    stop(glue("Library type ({libraryType}) not found, please provide one of 'pairEnd' or 'singleEnd'"))
  }


  if (!file.exists(reads_folder)) {
    stop(glue("reads_folder {reads_folder} does not exist\n"))

  }
  ### column SAMPLE_ID should be the sample name
  ### rows can be commented out with #
  if (libraryType == "pairEnd") {
    if (!all(c(column, "Read_1", "Read_2") %in% colnames(samples))) {
      stop(glue("Expecting the three columns {column}, Read_1 and Read_2 in samples file (tab-delimited)\n"))

    }
  }


  if (fileType == "fastq.gz") {
    if (libraryType == "pairEnd") {
      sampleList <- list()
      for (i in 1:nrow(samples)) {
        reads <- dir(path = file.path(reads_folder), pattern = "fastq.gz$", full.names = TRUE)
        #reads <- dir(path=file.path(reads_folder, samples[i,column]), pattern = "fastq.gz$", full.names = TRUE)
        map <- lapply(c("_R1","_R2"), grep, x = reads, value = TRUE)
        names(map) <- c("R1","R2")
        map$sampleName <-  samples[i,column]
        map$R1 <- samples[i,2]
        map$R2 <- samples[i,3]
        sampleList[[paste(map$sampleName)]] <- map
        #sampleList[[paste(map$sampleName)]]
      }
      write(paste("Setting up",length(sampleList),"jobs"), stdout())
      return(sampleList)
    } else if (libraryType == "singleEnd") {
      sampleList <- list()
      for (i in 1:nrow(samples)) {
        reads <- dir(path = file.path(reads_folder), pattern = "fastq.gz$", full.names = TRUE)
        #reads <- dir(path=file.path(reads_folder, samples[i,column]), pattern = "fastq.gz$", full.names = TRUE)
        map <- lapply(c("_SE"), grep, x = reads, value = TRUE)
        names(map) <- c("SE")
        map$sampleName <-  samples[i,column]
        #map$SE <- map$SE[i]
        map$SE <- samples[i,2]
        sampleList[[paste(map$sampleName)]] <- map
        #sampleList[[paste(map$sampleName)]]
      }
      write(paste("Setting up",length(sampleList),"jobs"), stdout())
      return(sampleList)
    }

  } else if (fileType == "bam") {
    sampleList <- list()
    for (i in 1:nrow(samples)) {
      reads <- dir(path = file.path(reads_folder), pattern = "bam$", full.names = TRUE)
      map <- lapply(c(".bam"), grep, x = reads, value = TRUE)
      names(map) <- c("bam")
      map$sampleName <-  samples[i,column]
      map$bam_sorted_pos <- map$bam[i]

      sampleList[[paste(map$sampleName)]] <- map
      sampleList[[paste(map$sampleName, sep = "_")]]
    }
    write(paste("Setting up",length(sampleList),"jobs"), stdout())
    return(sampleList)

  } else if (fileType == "sam") {
    sampleList <- list()
    for (i in 1:nrow(samples)) {
      reads <- dir(path = file.path(reads_folder), pattern = "sam$", full.names = TRUE)
      map <- lapply(c(".sam"), grep, x = reads, value = TRUE)
      names(map) <- c("sam")
      map$sampleName <-  samples[i,column]
      map$bam_sorted_pos <- map$bam[i]

      sampleList[[paste(map$sampleName)]] <- map
      sampleList[[paste(map$sampleName, sep = "_")]]
    }
    write(paste("Setting up",length(sampleList),"jobs"), stdout())
    return(sampleList)
  }
}
