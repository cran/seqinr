\name{aaa}
\alias{aaa}
\title{Converts amino-acid one-letter code into the three-letter one}
\description{
This is a vectorized function to convert one-letter amino-acid code
into the three-letter one, for instance "A" into "Ala".}
\usage{
aaa(aa)
}
\arguments{
  \item{aa}{ A vector of single characters.}
}
\details{
Allowed character values for \code{aa} are given by \code{a()}.
All other values will generate a warning and return NA.
Called without arguments, \code{aaa()} returns the list of
all possible output values.}
\value{
A vector of char string. All strings are 3 chars long.
}
\references{
The IUPAC one-letter code for aminoacids is described at: 
\url{https://www.bioinformatics.org/sms/iupac.html}
\code{citation("seqinr")}
}
\author{J.R. Lobry}
\seealso{\code{\link{a}}, \code{\link{translate}} }
\examples{
  #
  # Show all possible input values:
  #
  
  a()
  
  #
  # Convert them in one letter-code:
  #
  
  aaa(a())
  
  #
  # Check consistency of results:
  #
  
  stopifnot(a(aaa(a())) == a())
  
  #
  # Show what happens with non-allowed values:
  #
  
  aaa("Z") # should be NA and a warning is generated
}
\keyword{ utilities }
