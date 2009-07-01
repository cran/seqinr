\name{readPanels}
\alias{readPanels}
\title{Import GenMapper Panels configuration file}
\description{
In a Panel configuration file there is a description for a given identification
kit of the marker names, their dye label color, expected size range,
expected positive control genotypes, number of bases in core repeat,
stutter percentages, and allele names.
}
\usage{
readPanels(file,
  colnames = c("marker", "dye.col", "min.bp", "max.bp", "exp.pcg", "repeat.bp",
    "stutter.pc", "uknw", "allele names"))}
\arguments{
  \item{file}{The name of the Panel configuration file.}
  \item{colnames}{The names to be used for the columns of the data.frames.}
}
\value{
A list whose first element is the file header info and following elements
data.frames, one for each kit encountered in the file.
}
\details{
Number of bases in core repeat is set to 9 for Amelogenin locus.
}
\references{ 
\code{citation("seqinR")}
}
\author{J.R. Lobry}
\seealso{\code{\link{readBins}}, \code{\link{plotPanels}}.}
\examples{
#
# Check that we can read the 2 exemple files in the seqinR package:
#
path1 <- system.file("abif/AmpFLSTR_Panels_v1.txt", package = "seqinr")
res1 <- readPanels(path1)
path2 <- system.file("abif/Promega_Panels_v1.txt", package = "seqinr")
res2 <- readPanels(path2)
#
# Show the kits described in res1:
#
names(res1)
#
# Show some data for a given kit:
#
res1[["Identifiler_v1"]][, 1:7]
#
# Plot a simple summary of two kits:
#
par(mfrow = c(2,1))
plotPanels("Identifiler_v1", res1)
plotPanels("PowerPlex_16_v1", res2)

#
# Simple quality check since seqinR 2.0-4 with a file which containing
# a non constant number of tabulations as separator:
#
path3 <- system.file("abif/Prototype_PowerPlex_EP01_Pa.txt", package = "seqinr")
res3 <- readPanels(path3)
}
