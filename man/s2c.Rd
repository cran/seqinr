\name{s2c}
\alias{s2c}
\title{ conversion of a string into a vector of chars }
\description{
  This is a simple utility function to convert a string such
as "BigBang" into a vector of chars such as "B" "i" "g" "B" "a" "n" "g"
}
\usage{
s2c(string)
}
\arguments{
  \item{string}{ a string of chars }
}
\value{
  a vector of chars
}
\references{ 
To have an overview of the seqinR's functionnality, please consult this vignette: 
Charif, D., Lobry, J.R. (2005) SeqinR: a contributed package to the R project for statistical
computing devoted to biological sequences retrieval and analysis. Springer Verlag, \emph{Biological and Medical Physics/Biomedical Series}, in preparation. \cr
}
\author{ J.R. Lobry }
\seealso{ \code{\link{c2s}} }
\examples{
s2c( "BigBang" )
}
\keyword{utilities}
