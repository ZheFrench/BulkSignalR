% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/resource.R
\name{.formatPathwaysFromTxt}
\alias{.formatPathwaysFromTxt}
\title{Read dataframe from txt file}
\usage{
.formatPathwaysFromTxt(file, resourceName = NULL)
}
\arguments{
\item{file}{Path to a tabular file.}

\item{resourceName}{Two options "GO-BP"  "REACTOME".}
}
\value{
Dataframe with pathwayID, geneName and pathwayName
}
\description{
Read dataframe from txt file
}
