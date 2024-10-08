% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BSRInference-class.R
\docType{class}
\name{BSRInference-class}
\alias{BSRInference-class}
\title{BulkSignalR Inference Object}
\description{
An S4 class to represent inferred ligand-receptor interactions.
}
\details{
This class contains inferred LR interactions along with
their statistical significance. Data representation supports subsequent
reductions to pathways, etc. See reduction functions
\code{"\link[=BSRInference-class]{reduceToBestPathway}"},
\code{"\link[=BSRInference-class]{reduceToLigand}"},
\code{"\link[=BSRInference-class]{reduceToReceptor}"} and
\code{"\link[=BSRInference-class]{reduceToPathway}"}.
}
\section{Slots}{

\describe{
\item{\code{LRinter}}{A data frame describing the (ligand,receptor,pathway) triples
with P- and Q-values.}

\item{\code{ligands}}{A list of ligands, one entry per LR interaction.}

\item{\code{receptors}}{A list of receptors, one entry per LR interaction.}

\item{\code{t.genes}}{A list of target genes, one entry per LR interaction.}

\item{\code{tg.corr}}{A list of target gene correlations to the receptor, one
entry per interaction}

\item{\code{inf.param}}{The parameters used for the inference.}
}}

\examples{
data(sdc,package='BulkSignalR')
normal <- grep("^N", names(sdc))

bsrdm <- prepareDataset(sdc[,-normal])
if(FALSE){
bsrdm <- learnParameters(bsrdm, quick=FALSE,
plot.folder=".", verbose=TRUE)
bsrinf <- initialInference(bsrdm)
}
}
