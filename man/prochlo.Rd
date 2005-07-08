\name{prochlo}
\alias{prochlo}
\docType{data}
\title{ Zscore on three strains of Prochlorococcus marinus }
\description{
This dataset contains the zscores computed with the codon model on all
CDS from 3 strains of Procholorococcus marinus (as retrieved from Genome
Reviews database on June 16, 2005)
}
\usage{
data(prochlo)
}
\format{
  List of three dataframes of the zscore of each of the 16 dinucleotides
  on each CDS retrieved from the specific strain.

\describe{
  \item{BX548174}{strain adapted to living at a depth of 5
  meters (high levels of UV exposure)
  \code{base} model on each intergenic sequence}
  \item{AE017126}{strain adapted to living at a depth of 120
  meters (low levels of UV exposure)}
  \item{BX548175}{strain adapted to living at a depth of 135
  meters (low levels of UV exposure)}
}
}
\references{
  Palmeira, L., Guéguen, L. and Lobry JR. (in prep) UV-targeted dinucleotides are not depleted in light-exposed Prokaryotic genomes.
  
  \code{citation("seqinr")}
}
\seealso{ \code{\link{zscore}} }
\examples{
data(prochlo)
}
\keyword{ datasets }
