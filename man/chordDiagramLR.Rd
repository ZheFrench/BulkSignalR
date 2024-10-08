% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/visualization.R
\name{chordDiagramLR}
\alias{chordDiagramLR}
\title{Chord Diagram of LR interactions with correlations}
\usage{
chordDiagramLR(
  bsrinf,
  pw.id.filter = NULL,
  qval.thres = 1,
  ligand = NULL,
  receptor = NULL,
  path = "./",
  filename = NULL,
  limit = 20,
  format = c("pdf", "svg", "png"),
  width = 4,
  height = 4
)
}
\arguments{
\item{bsrinf}{bsrinf object}

\item{pw.id.filter}{One Pathway ID accepted only to}

\item{qval.thres}{threshold over Q-value.}

\item{ligand}{Ligand
of the LR pair that you want to
highlight in the chord diagram.}

\item{receptor}{Receptor
of the LR pair that you want to highlight
in the chord diagram.}

\item{path}{Path where to create the file containing the plot.}

\item{filename}{Filename for the plot or NULL (default) for display
on screen.}

\item{limit}{Number of interactions you can visualize.}

\item{format}{pdf / png / svg.}

\item{width}{width of image in inches.}

\item{height}{height of image in inches.}
}
\value{
Circos Plot on the screen or a file
}
\description{
Chord diagram.
}
\examples{
print("chordDiagramLR")
data(bsrinf.example, package = "BulkSignalR")

chordDiagramLR(bsrinf.example,
    pw.id.filter = "R-HSA-2173782",
    ligand = "A2M",
    receptor = "LRP1",
    path = "./",
    filename = "sdc_chord",
    limit = 20,
    width = 5,
    height = 4.5
)
}
