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
  \item{aa}{ A vector of single characters }
}
\details{
Allowed character values for \code{aa} are *ACDEFGHIKLMNPQRSTVWY.
All other values will generate a warning and return NA. }
\value{
A vector of char string. All strings are 3 chars long, run
example(aaa) to see them.
}
\references{
The IUPAC one-letter code for aminoacids is described at: 
\url{http://www.chem.qmul.ac.uk/iupac/AminoAcid/}
\code{citation("seqinr")}
}
\author{ J.R. Lobry }
\seealso{ \code{\link{translate}}, \code{\link{a}} }
\examples{
  # show all allowed values
  allowed <- s2c("*ACDEFGHIKLMNPQRSTVWY")
  aaa(allowed)
  # show what's happen with non-allowed values
  aaa("Z") # should be NA and a warning is generated
}
\keyword{ utilities }
