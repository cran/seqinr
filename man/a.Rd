\name{a}
\alias{a}
\title{Converts amino-acid three-letter code into the one-letter one}
\description{
This is a vectorized function to convert three-letters amino-acid code
into the one-letter one, for instance "Ala" into "A".
}
\usage{
a(aa)
}
\arguments{
  \item{aa}{ A vector of string. All strings are 3 chars long.}
}
\details{
Allowed character values for \code{aa} are given by \code{aaa()}.
All other values will generate a warning and return NA.
Called without arguments, \code{a()} returns the list of all possible
output values.
}
\value{
A vector of single characters.
}
\references{
The IUPAC one-letter code for aminoacids is described at: 
\url{https://www.bioinformatics.org/sms/iupac.html}\cr
\code{citation("seqinr")}
}
\author{D. Charif, J.R. Lobry }
\seealso{\code{\link{aaa}}, \code{\link{translate}} }
\examples{
  #
  # Show all possible input values:
  #
  
  aaa()
  
  #
  # Convert them in one letter-code:
  #
  
  a(aaa())
  
  #
  # Check consistency of results:
  #
  
  stopifnot( aaa(a(aaa())) == aaa())
  
  #
  # Show what happens with non-allowed values:
  #
  
  a("SOS") # should be NA and a warning is generated
}
\keyword{ utilities }
