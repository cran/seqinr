\name{invers}
\alias{invers}
\title{invers}
\description{
 Reverse a sequence. For example, \code{"a","c","g","t"} is transformed into \code{"t","g","c","a"}.
}
\usage{
invers(seq)
}
\arguments{
  \item{seq}{ A vector of chars }
}
\value{
 return a vector of character.
}
\references{ 
To have an overview of the seqinR's functionnality, please consult this vignette: 
Charif, D., Lobry, J.R. (2005) SeqinR: a contributed package to the R project for statistical
computing devoted to biological sequences retrieval and analysis. Springer Verlag, \emph{Biological and Medical Physics/Biomedical Series}, in preparation. \cr
}
\author{D. Charif}
\seealso{ \code{\link{rev}} }
\examples{
s=s2c("acgtgctcgtataatatgccc")
invers(s)
}
\keyword{manip}
