\name{comp}
\alias{comp}
\title{ complement a nucleic sequence }
\description{
    Complement a sequence, for instance if the sequence is \code{"a","c","g","t"} it returns \code{"t","g","c","a"}. All other values will return NA.
}
\usage{
comp(seq)
}
\arguments{
  \item{seq}{ a vector of chars }
}
\value{
  It returns a vector of character which is the complement of a sequence.
}
\author{ D. Charif }
\references{ 
To have an overview of the seqinR's functionnality, please consult this vignette: 
Charif, D., Lobry, J.R. (2005) SeqinR: a contributed package to the R project for statistical
computing devoted to biological sequences retrieval and analysis. Springer Verlag, \emph{Biological and Medical Physics/Biomedical Series}, in preparation. \cr
}
\examples{
   a=s2c("acgtgctcgtatatatatgccc")
   comp(a)		
   b=s2c("acgtXctcgtatatatatgccc")
   comp(b)
}
\keyword{ manip }
