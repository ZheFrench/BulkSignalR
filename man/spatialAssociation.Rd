% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/spatial.R
\name{spatialAssociation}
\alias{spatialAssociation}
\title{Statistical association of scores with area labels}
\usage{
spatialAssociation(
  scores,
  areas,
  test = c("Kruskal-Wallis", "ANOVA", "Spearman", "r2"),
  label.col = "label",
  idSpatial.col = "idSpatial",
  fdr.proc = c("BH", "Bonferroni", "Holm", "Hochberg", "SidakSS", "SidakSD", "BY", "ABH",
    "TSBH")
)
}
\arguments{
\item{scores}{A matrix of scores, one L-R interaction per row and
spatial locations in the columns. This matrix is typically obtained
from BulkSignalR functions \code{scoreLRGeneSignatures} or \code{scScoring}.}

\item{areas}{A data.frame containing at least the x and y
coordinates of the locations, the unique IDs of spatial locations, and
a label column.}

\item{test}{The chosen statistical test or statistics
(see details below).}

\item{label.col}{Column name in \code{areas} containing area labels.}

\item{idSpatial.col}{Column name in \code{areas} containing the unique
IDs of spatial locations.}

\item{fdr.proc}{Multiple hypothesis correction procedure, see
\code{multtest}.}
}
\value{
A data.frame with the names of the interactions, the value of the
chosen statistics, and the corresponding Q-value.
}
\description{
Compute the statistical association of L-R interaction score spatial
distributions with tissue area labels.
Not limited to BulkSignalR gene signature scores.
}
\details{
In case the
nonparametric Kruskal-Wallis test is chosen, additional columns are provided
testing each label for significantly larger scores (Kruskal-Wallis is global
and only says whether one or several labels show a bias). Individual
labels are tested with Wilcoxon and two columns are added *per* label,
one for the statistics and one for a Bonferroni-corrected P-value over
all the labels.

In case an actual statistical test is chosen, a parametric test (ANOVA) and
a non-parametric test (Kruskal-Wallis) are available for the global analysis.
Individual labels are tested with T-tests or Wilcoxon (Bonferroni-corrected)
accordingly.

In case a statistics is preferred, Spearman correlation or explained variance
(r2 or coefficient of determination, through linear models) 
are available. They mesure the relationship
between each individual area and \code{scores}. For the explained variance,
a global value (R2) is also computed from a multi-linear model (the same as
what is used for the ANOVA).
}
\examples{
print("spatialAssociation")
if (FALSE) {
    spatialAssociation(scores, areas)
}
}
