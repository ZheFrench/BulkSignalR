% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BSRInferenceComp-class.R,
%   R/BSRSignatureComp-class.R
\name{tgLogFC}
\alias{tgLogFC}
\alias{tgLogFC,BSRInferenceComp-method}
\alias{tgLogFC,BSRSignatureComp-method}
\title{Target gene logFC accessor}
\usage{
\S4method{tgLogFC}{BSRInferenceComp}(x)

\S4method{tgLogFC}{BSRSignatureComp}(x)
}
\arguments{
\item{x}{BSRSignature}
}
\value{
tgLogFC
}
\description{
Target gene logFC accessor

Target gene logFC accessor
}
\examples{
if(FALSE){
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

# infer ligand-receptor interactions from the comparison
bsrinf <- initialInference(bsrdm.comp, max.pval = 1, "random.example")

tgLogFC(bsrinf)
}
}
