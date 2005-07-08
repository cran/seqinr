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
  \code{citation("seqinr")}
}
\author{ J.R. Lobry }
\seealso{ \code{\link{c2s}} }
\examples{
s2c( "BigBang" )
}
\keyword{utilities}
