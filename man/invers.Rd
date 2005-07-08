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
  \code{citation("seqinr")}
}
\author{D. Charif}
\seealso{ \code{\link{rev}} }
\examples{
s=s2c("acgtgctcgtataatatgccc")
invers(s)
}
\keyword{manip}
