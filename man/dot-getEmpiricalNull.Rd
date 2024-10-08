% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/modelTraining.R
\name{.getEmpiricalNull}
\alias{.getEmpiricalNull}
\title{Sampling of correlations downstream the receptors null distribution}
\usage{
.getEmpiricalNull(
  ncounts,
  n.rand = 5,
  min.cor = -1,
  with.complex = TRUE,
  max.pw.size = 400,
  min.pw.size = 5,
  min.positive = 4
)
}
\arguments{
\item{ncounts}{A matrix or table of normalized read counts.}

\item{n.rand}{The number of repetitions.}

\item{min.cor}{The minimum ligand-receptor Spearman correlation
required.}

\item{with.complex}{A logical indicating whether receptor co-complex
members should be included in the target genes.}

\item{max.pw.size}{Maximum pathway size to consider from the pathway
reference.}

\item{min.pw.size}{Minimum pathway size to consider from the pathway
reference.}

\item{min.positive}{Minimum number of target genes to be found in a given
pathway.}
}
\value{
A list of \code{n.rand} tables such as output by
  \code{.checkReceptorSignaling}. Each table is computed from a randomized
  expression matrix (randomized \code{ncounts}).
}
\description{
Perform receptor downstream analysis with
\code{.checkReceptorSignaling} based on randomized expression data and
ligand-receptor pairs selected from the same randomized data.
}
\details{
A large number of correlations 
  (ligand-receptor and receptor-downstream target genes) 
  is reported in each randomized matrix. Therefore,
  \code{n.rand} should be
  given a modest value to avoid unnecessarily long computations.

  See \code{\link{.checkReceptorSignaling}} for more details about the
  parameters.
}
\keyword{internal}
