\name{GC}
\alias{GC}
\title{Calculates the fractional GC content of nucleic acid sequences}
\description{
This calculates the fraction of G+C bases of the input nucleic acid sequence(s)
It reads in nucleic acid sequences, sums the number of 'G' and 'C' bases and writes out the result as the fraction (in the interval 0.0 to 1.0) of the length of the whole sequence. 
}
\usage{
GC(seq)
}
\arguments{
  \item{seq}{any nucleic acid sequence}
}
\value{
  It returns the fraction of G+C as a numeric vector
}
\references{ 
To have an overview of the seqinR's functionnality, please consult this vignette: 
Charif, D., Lobry, J.R. (2005) SeqinR: a contributed package to the R project for statistical
computing devoted to biological sequences retrieval and analysis. Springer Verlag, \emph{Biological and Medical Physics/Biomedical Series}, in preparation. \cr
}
\author{D. Charif }
\examples{
   s=s2c("agtctggggggccccttttaagtagatagatagctagtcgta")
   GC(s)
}
\keyword{manip}
