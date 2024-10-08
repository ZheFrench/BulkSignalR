% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BSRDataModelComp-class.R
\name{addClusterComp}
\alias{addClusterComp}
\alias{addClusterComp,BSRDataModelComp-method}
\title{Add a comparison between two clusters of samples}
\usage{
\S4method{addClusterComp}{BSRDataModelComp}(obj, cmp, cmp.name)
}
\arguments{
\item{obj}{A BSRDataModelComp object output by
\code{\link{as.BSRDataModelComp}}.}

\item{cmp}{A BSRClusterComp object to add.}

\item{cmp.name}{The name of the comparison to add.}
}
\value{
A BSRDataModelComp object.
}
\description{
Add a comparison to a BSRDataModelComp object.
}
\details{
Add \code{cmp} to the list of comparisons contained in
\code{obj}.
}
\examples{
# prepare data
data(sdc, package = "BulkSignalR")
normal <- grep("^N", names(sdc))
bsrdm <- prepareDataset(sdc[, -normal])

# define the comparison
bsrdm.comp <- as.BSRDataModelComp(bsrdm)
colA <- as.integer(1:5)
colB <- as.integer(8:15)
n <- nrow(ncounts(bsrdm.comp))
stats <- data.frame(
    pval = runif(n), logFC = rnorm(n, 0, 2),
    expr = runif(n, 0, 10)
)
rownames(stats) <- rownames(ncounts(bsrdm.comp))
bsrcc <- defineClusterComp(bsrdm.comp, colA, colB, stats)
bsrdm.comp <- addClusterComp(bsrdm.comp, bsrcc, "random.example")
}
