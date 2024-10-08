% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/resource.R
\name{createResources}
\alias{createResources}
\title{Create all resources.}
\usage{
createResources(onRequest = TRUE, verbose = FALSE)
}
\arguments{
\item{onRequest}{logical True if you force
download again. This will overwrite
pre-existing database. Default is True.}

\item{verbose}{Default is FALSE}
}
\value{
Returns `NULL`, invisibly.
}
\description{
Create cache for all resources (pathways, or PWC network)
downloaded from the web when library is first loaded.
This part is handled with BiocFileCache.
Otherwise datatabase, is handled by another process
not relying on BiocFileCache instance.
}
\examples{
if (FALSE) {
    createResources()
}
}
