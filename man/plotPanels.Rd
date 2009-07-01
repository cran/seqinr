\name{plotPanels}
\alias{plotPanels}
\title{Representation of Amplicon Size Ranges of a STR kit.}
\description{Plot amplicon size ranges grouped by dye color.}
\usage{plotPanels(kitname, data, xlim = NULL, cex = 0.75, alpha = 0.5)}
\arguments{
  \item{kitname}{string of characters for the kit name.}
  \item{data}{an output from the \code{\link{readPanels}} function.}
  \item{xlim}{x-axis range.}
  \item{cex}{character expansion factor.}
  \item{alpha}{alpha transparency chanel for colors.}
}
\value{none}
\author{J.R. Lobry}

\seealso{ \code{\link{readPanels}}.}

\examples{
path1 <- system.file("abif/AmpFLSTR_Panels_v1.txt", package = "seqinr")
res1 <- readPanels(path1)

par(mfrow = c(2,1))
plotPanels("Identifiler_v1", res1)
plotPanels("SEfiler_v1", res1)

}
