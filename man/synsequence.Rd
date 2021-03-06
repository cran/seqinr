\name{synsequence}
\alias{synsequence}
\title{Random synonymous coding sequence generation}
\description{
  Generates a random synonymous coding sequence, according to a certain
  codon usage bias
}
\usage{synsequence(sequence, numcode = 1, ucoweight = NULL)}
\arguments{
  \item{sequence}{ A nucleic acids sequence }
  \item{numcode}{ The genetic code number as in \code{translate} }
  \item{ucoweight}{ A list of weights containing the desired codon usage
    bias as generated by \code{ucoweight} }
}
\value{
  a sequence translating to the same protein sequence as the original
  one (cf. \code{translate}), but containing synonymous codons  
}
\references{
  \code{citation("seqinr")}
}
\author{L. Palmeira}
\seealso{ \code{\link{ucoweight}} }
\examples{
  data(ec999)
  sequence=ec999[1][[1]]
  synsequence(sequence,1,ucoweight(sequence))
}
\keyword{ utilities }
