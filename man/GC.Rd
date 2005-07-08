\name{G+C Content}
\alias{GC}
\alias{GC1}
\alias{GC2}
\alias{GC3}
\title{Calculates the fractional G+C content of nucleic acid sequences}
\description{
  Calculates the fraction of G+C bases of the input nucleic acid
  sequence(s). It reads in nucleic acid sequences, sums the number of
  'G' and 'C' bases and writes out the result as the fraction (in the
  interval 0.0 to 1.0) of the length of the whole sequence. Global G+C
  content \code{GC}, G+C in the first position of the codon bases
  \code{GC1}, G+C in the second position of the codon bases
  \code{GC2}, and G+C in the third position of the codon bases
  \code{GC3} can be computed.
}
\usage{
GC(seq)
GC1(seq)
GC2(seq)
GC3(seq)
}
\arguments{
  \item{seq}{any nucleic acid sequence}
}
\value{
  \code{GC} returns the fraction of G+C as a numeric vector,
  \code{GC1} returns the fraction of G+C at first codon position as a numeric vector,
  \code{GC2} returns the fraction of G+C at second codon position  as a numeric vector,
  \code{GC3} returns the fraction of G+C at third codon position  as a numeric vector.
}
\references{ 
  \code{citation("seqinr")}
}
\author{D. Charif and L. Palmeira}
\examples{
   s=s2c("agtctggggggccccttttaagtagatagatagctagtcgta")
   GC(s)
   GC1(s)
   GC2(s)
   GC3(s)
}
\keyword{manip}
