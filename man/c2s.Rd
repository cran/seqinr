\name{c2s}
\alias{c2s}
\title{ conversion of a vector of chars into a string }
\description{
  This is a simple utility function to convert a vector of chars
such as c("m", "e", "r", "g", "e", "d") into a single string such
as "merged".
}
\usage{
c2s(chars = c("m", "e", "r", "g", "e", "d"))
}
\arguments{
  \item{chars}{ a vector of chars }
}
\value{
  a string
}
\author{ J.R. Lobry }
\references{ 
To have an overview of the seqinR's functionnality, please consult this vignette: 
Charif, D., Lobry, J.R. (2005) SeqinR: a contributed package to the R project for statistical
computing devoted to biological sequences retrieval and analysis. Springer Verlag, \emph{Biological and Medical Physics/Biomedical Series}, in preparation. \cr
}
\seealso{ \code{\link{s2c}} }
\examples{
c2s( c("m","e","r","g","e","d") )
}
\keyword{utilities}
