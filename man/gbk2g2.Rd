\name{gbk2g2}
\alias{gbk2g2}
\title{ Conversion of a GenBank format file into a glimmer-like one }
\description{
This function reads a file in GenBank format and converts the features
corresponding to CDS (Coding Sequences) into a format similar to
glimmer program output.
}
\usage{
gbk2g2(gbkfile =  "ftp://pbil.univ-lyon1.fr/pub/seqinr/data/ct.gbk",
g2.coord = "g2.coord")
}

\arguments{
  \item{gbkfile}{ The name of the GenBank file }
  \item{g2.coord}{ The name of the output file in glimmer-like format }
}
\details{
Partial CDS (either 5' or 3') and join in features are discarded.
}
\value{
The input file is returned invisibly.
}
\references{
  \code{citation("seqinr")}
}
\author{J.R. Lobry}
\seealso{ \code{\link{oriloc}} which uses glimmer-like files, 
  \code{\link{gbk2g2.euk}} for eukaryotic sequences with introns.}
\examples{
  \dontrun{ # need internet connection
  	suppressWarnings(gbk2g2(g2.coord = "gbk2g2.test"))
  	res <- read.table("gbk2g2.test")
  	head(res)
  	stopifnot(nrow(res) == 892)
  }
}
\keyword{utilities}
