% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/createSampleListFunctionOld.R
\name{createSampleListOld}
\alias{createSampleListOld}
\title{Creating sample list to run mcapply parallel}
\usage{
createSampleListOld(samples, reads_folder, column = "SAMPLE_ID", fileType = NULL, libraryType = "pairEnd", samplesFromSTAR = FALSE, step = NULL)
}
\arguments{
\item{samples}{\code{Character.} The filename of the sample file. Default samples.txt.}

\item{reads_folder}{\code{Character.} Directory where the raw sequence data is stored. Default 00-Fastq.}

\item{column}{\code{Character.} Column name from the sample sheet to use as read folder names. Default SAMPLE_ID}

\item{fileType}{\code{Character.} The file type to use. Available: 'fastq.gz', 'bam' or 'sam'. Default NULL}

\item{libraryType}{\code{Character.} The library type to use. Available: 'pairEnd' or 'singleEnd'. Default pairEnd}

\item{samplesFromSTAR}{\code{logical.} Whether want to count reads from STAR mapped files. Default FALSE}

\item{step}{\code{Character} Which step to run, Quality Controls or Mapping. Default NULL}
}
\description{
Function to create sample list to run mcapply parallel
}
\details{
December 2021
}
\author{
Haniel Cedraz
}
