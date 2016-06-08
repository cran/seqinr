\name{ucoweight}
\alias{ucoweight}
\title{Weight of each synonymous codon}
\description{
  Returns a list containing, for each of the 20 amino acids + STOP
  codon, the codon usage bias of each of the synonymous codon according
  to a given codon sequence.
}
\usage{ucoweight(sequence, numcode = 1)}
\arguments{
  \item{sequence}{ A nucleic acids sequence }
  \item{numcode}{ The genetic code number as in \code{translate} }
}
\value{
  a list containing, for each of the 20 amino acids and STOP
  codon (list tags), the weight of each synonymous codon (including the
  original one).
}
\references{
  \code{citation("seqinr")}
}
\author{L. Palmeira}
\seealso{ \code{\link{synsequence}} }
\examples{
  data(ec999)
  ucoweight(ec999[1][[1]])
}
\keyword{ utilities }
