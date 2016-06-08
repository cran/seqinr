\name{s2c}
\alias{s2c}
\title{ conversion of a string into a vector of chars }
\description{
  This is a simple utility function to convert a single string such
  as \code{"BigBang"} into a vector of chars such as
  \code{c("B", "i", "g", "B", "a", "n", "g")}.
}
\usage{
s2c(string)
}
\arguments{
  \item{string}{ a string of chars }
}
\value{
  a vector of chars. If supplied argument is not a single string, a warning is
  issued and NA returned.
}
\references{
  \code{citation("seqinr")}
}
\author{J.R. Lobry}
\seealso{ \code{\link{c2s}} }
\examples{
stopifnot(all(s2c("BigBang") == c("B", "i", "g", "B", "a", "n", "g")))
}
\keyword{utilities}
