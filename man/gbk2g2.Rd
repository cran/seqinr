\name{gbk2g2}
\alias{gbk2g2}
\title{ Conversion of a GenBank format file into a glimmer-like one }
\description{
This function reads a file in GenBank format and converts the features
corresponding to CDS (Coding Sequences) into a format similar to
glimmer program output.
}
\usage{
gbk2g2(gbkfile = "ftp://pbil.univ-lyon1.fr/pub/logiciel/oriloc/ct.gbk",
g2.coord = "g2.coord")
}

\arguments{
  \item{gbkfile}{ The name of the GenBank file }
  \item{g2.coord}{ The name of the glimmer-like file }
}
\details{
Partial CDS (either 5' or 3') and join in features are discarded.
}
\value{
}
\references{
  \code{citation("seqinr")}
}
\author{J.R. lobry }
\seealso{ \code{\link{oriloc}} }
\examples{
 \dontrun{ gbk2g2() }
}
\keyword{utilities}
