% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/visualization.R
\name{bubblePlotPathwaysLR}
\alias{bubblePlotPathwaysLR}
\title{Bubble Plot to explore LR & Pathways}
\usage{
bubblePlotPathwaysLR(
  bsrinf,
  pathways,
  qval.thres = 1,
  filter.L = NULL,
  filter.R = NULL,
  path = "./",
  filename = NULL,
  color = "#16a647",
  width = 16,
  height = 7,
  pointsize = 6,
  format = c("pdf", "svg", "png")
)
}
\arguments{
\item{bsrinf}{BulkSignalR inference object.}

\item{pathways}{Vector of pathway names to keep.}

\item{qval.thres}{Maximum Q-value.}

\item{filter.L}{Vector of ligands to keep.}

\item{filter.R}{Vector of receptors to keep.}

\item{path}{Path directory to plot.}

\item{filename}{An output filename, NULL by default to display on screen.}

\item{color}{Main color used for the gradient.}

\item{width}{Global image width in cm.}

\item{height}{Global image height in cm.}

\item{pointsize}{Global pointsize.}

\item{format}{File format.}
}
\value{
A bubble plot displayed in the current viewport or in a file
in case a filename was provided.

This is a convenience function to propose a simple way
of representing LR - Pathways association
with their respective correlation
and Q-values.
}
\description{
Quick check to observe LR - Pathways association
with their respective correlation
and Q-values.
}
\examples{
print("bubblePlotPathwaysLR")
data(bsrinf.example, package = "BulkSignalR")
pathways <- LRinter(bsrinf.example)[1,c("pw.name")]
bubblePlotPathwaysLR(bsrinf.example,
    pathways = pathways,
    qval.thres = 1,
    path = "./",
    color = "red",
    filename = "sdc_bubble",
    width = 16,
    height = 7,
    pointsize = 8
)
}
