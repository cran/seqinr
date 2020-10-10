\name{read.abif}
\alias{read.abif}
\title{Read ABIF formatted files}
\description{
ABIF stands for Applied Biosystem Inc. Format, a binary fromat modeled after TIFF format.
Corresponding files usually have an \code{*.ab1} or \code{*.fsa} extension.
}
\usage{
read.abif(filename, max.bytes.in.file = file.info(filename)$size, 
 pied.de.pilote = 1.2, verbose = FALSE)
}
\arguments{
  \item{filename}{The name of the file.}
  \item{max.bytes.in.file}{The size in bytes of the file, defaulting to what is returned by \code{\link{file.info}}}	
  \item{pied.de.pilote}{Safety factor: the argument \code{n} to \code{\link{readBin}} is set as \code{pied.de.pilote*max.bytes.in.file}.}	
  \item{verbose}{logical [FALSE]. If TRUE verbose mode is on.}	
}
\details{
All data are imported into memory, there is no attempt to read items on the fly.
}
\value{
A list with three components: \code{Header} which is a list that contains various low-level information,
among which \code{numelements} is the number of elements in the directory and \code{dataoffset}
the offset to find the location of the directory. \code{Directory} is a data.frame for the directory
of the file with the number of row being the number of elements in the directory and the 7
columns describing various low-level information about the elements. \code{Data} is a list
with the number of components equal to the number of elements in the directory. 
}
\references{ 
\code{citation("seqinR")}

Anonymous (2006) Applied Biosystem Genetic Analysis Data File Format.
Available at \url{https://www.thermofisher.com/at/en/home/brands/applied-biosystems.html}.
Last visited on 03-NOV-2008.

The figure in the example section is an attempt to reproduce figure 1A from:

Krawczyk, J.,  Goesmann, A., Nolte, R., Werber, M., Weisshaar, B. (2009)
Trace2PS and FSA2PS: two software toolkits for converting trace and fsa files to PostScript format.
\emph{Source Code for Biology and Medicine}, \bold{4}:4.

}
\author{J.R. Lobry}

\seealso{
\code{\link{readBin}} which is used here to import the binary file and \code{\link{file.info}} to
get the size of the file. See \code{\link{JLO}} for the files used in quality check.
}

\examples{
#
# Quality check:
#

data(JLO)
JLO.check <- read.abif(system.file("abif/2_FAC321_0000205983_B02_004.fsa", 
  package = "seqinr"))
stopifnot(identical(JLO, JLO.check))

#
# Try to reproduce figure 1A from Krawczyk et al. 2009:
#

Krawczyk <- read.abif(system.file("abif/samplefsa2ps.fsa", 
  package = "seqinr"))$Data
x <- 1:length(Krawczyk[["DATA.1"]])
par(mar = c(2,4,2,0)+0.1, cex = 0.5)
plot(x, Krawczyk[["DATA.1"]], type = "l", col = "blue",
  ylab = "", xlab = "",
  ylim = c(-2000, 10000), cex = 0.5,
  main = "Figure 1A from Krawczyk et al. 2009",
  xaxs = "i", yaxs = "i",
  xaxt = "n", yaxt = "n")
axis(1, at = seq(2000, 24000, by = 2000))
axis(2, at = seq(-1000, 10000, by = 1000), las = 1)
lines(x, Krawczyk[["DATA.2"]], col = "green")
lines(x, Krawczyk[["DATA.3"]], col = "black")
lines(x, Krawczyk[["DATA.4"]], col = "red")

}
