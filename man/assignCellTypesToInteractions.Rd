% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cellTypeAssign.R
\name{assignCellTypesToInteractions}
\alias{assignCellTypesToInteractions}
\title{Assign cell types to L-R interactions}
\usage{
assignCellTypesToInteractions(
  bsrdm,
  bsrinf,
  ct.scores,
  normalize.scores = TRUE,
  min.weight = 0.1,
  min.r2 = 0.25,
  min.r2.after = 0.35,
  lasso = TRUE,
  qval.thres = 0.001
)
}
\arguments{
\item{bsrdm}{A BSRDataModel object.}

\item{bsrinf}{A BSRInference object.}

\item{ct.scores}{A matrix of cell type signature scores.}

\item{normalize.scores}{A logical indicating whether scores should be
normalized before assigning cell types.}

\item{min.weight}{Minimum weight to keep in the linear model (cell types
with lower weights will be discarded) if \code{lasso==TRUE}. Otherwise,
minimum correlation coefficient of each individual cell type.}

\item{min.r2}{Minimum r2 between a candidate cell type and a L-R gene
signature score.}

\item{min.r2.after}{Minimum r2 between the proposed linear model and
a L-R gene signature score to retain the model.}

\item{lasso}{Logical indicating that the LASSO (or linear regression if
only one cell type satisfies the \code{min.r2} criterion) should be
used. Otherwise, Spearman linear correlation is used.}

\item{qval.thres}{Maximum Q-value of the L-R pairs to be considered.}
}
\value{
A data.frame containing the cell type assignments for each
L-R interaction. Unique interactions are considered only (thanks to
\code{"\link[=BSRInference-class]{reduceToBestPathway}"} that is applied
internally). An interaction can be associated with several cell types
or none. In case it is associated with a single cell type, it is labelled
autocrine (indicative only).

Cell type signature scores must be provided. They can be computed
with BulkSignalR utility function \code{"\link{scoreSignatures}"}, but
also any other external tool such as CIBERSORT or BisqueRNA. In case
such a tool would score cell types in a nonlinear fashion, we
recommend to transform the score matrix to restore a linear relationship
cell type abundance/score. By default, cell type (and L-R gene
signature) scores are normalized between 0 and 1 to make the weights of each
cel type in the linear models as comparable as possible.
}
\description{
Generate a data.frame linking interactions to cell types.
}
\examples{
print("assignCellTypesToInteractions")
data(sdc, package = "BulkSignalR")
data(bsrinf.example, package = "BulkSignalR")

bsrdm <- prepareDataset(counts = sdc)

# microenvironment cell populations
data(immune.signatures, package = "BulkSignalR")
immune.signatures <- immune.signatures[immune.signatures$signature \%in\%
    c(
        "B cells", "Dentritic cells", "Macrophages",
        "NK cells", "T cells", "T regulatory cells"
    ), ]
data("tme.signatures", package = "BulkSignalR")
signatures <- rbind(immune.signatures, tme.signatures[
    tme.signatures$signature \%in\% c("Endothelial cells", "Fibroblasts"),
])
tme.scores <- scoreSignatures(bsrdm, signatures)

# assignment
lr2ct <- assignCellTypesToInteractions(bsrdm, bsrinf.example, tme.scores)
}
