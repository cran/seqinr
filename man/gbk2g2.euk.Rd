\name{gbk2g2.euk}
\alias{gbk2g2.euk}
\title{ Conversion of a GenBank format file into a glimmer-like
  one. Eukaryotic version.}
\description{
This function reads a file in GenBank format and converts the features
corresponding to CDS (Coding Sequences) into a format similar to
glimmer program output. This function is specifically made for
eukaryotic sequences, i.e. with introns. 
}
\usage{
gbk2g2.euk(gbkfile = system.file("sequences/ame1.gbk", package ="seqinr"),
g2.coord = "g2.coord")
}

\arguments{
  \item{gbkfile}{ The name of the GenBank file }
  \item{g2.coord}{ The name of the output file }
}
\details{
This function returns the coordinates of the exons annotated in the
GenBank format file. 
}
\value{
A data frame with three columns will be written to the \code{g2.coord}
file. The first column corresponds to the name of the gene, given in the
GenBank file through the \code{/gene} feature. The second and third
column contain the start and the stop position of the exon.
}
\references{
  \code{citation("seqinr")}
}
\author{J.R. Lobry, A. Necşulea}
\seealso{ \code{\link{oriloc}}, \code{\link{gbk2g2}} }
\examples{
 \dontrun{ gbk2g2.euk() }
}
\keyword{utilities}
