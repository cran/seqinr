\name{GC2}
\alias{GC2}
\title{Calculates the fractional GC2 content of nucleic acid sequences}
\description{
This calculates the fraction of G+C in the second position of the codon bases of the input nucleic acid sequence(s)}
\usage{
GC2(seq)
}
\arguments{
  \item{seq}{any nucleic acid sequence}
}
\value{
  It returns the fraction of GC2 as a numeric vector
}
\references{ 
To have an overview of the seqinR's functionnality, please consult this vignette: 
Charif, D., Lobry, J.R. (2005) SeqinR: a contributed package to the R project for statistical
computing devoted to biological sequences retrieval and analysis. Springer Verlag, \emph{Biological and Medical Physics/Biomedical Series}, in preparation. \cr
}
\author{D. Charif }
\examples{
   s=s2c("agtctggggggccccttttaagtagatagatagctagtcgta")
   GC2(s)
}
\keyword{manip}
