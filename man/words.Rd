\name{words}
\alias{words}
\title{ To get all words from an alphabet.}
\description{
Generates a vectors of all the words from a given alphabet,
with right positions varying faster, for instance if the
\code{alphabet} is \code{(c("0","1")} and the \code{length}
is 2 you will obtain \code{c("00", "01", "10", "11")}
}
\usage{
words(length = 3, alphabet = s2c("acgt"))
}
\arguments{
  \item{length}{ the number of characters in the words }
  \item{alphabet}{ a vector of characters }
}
\value{
A vector of string whith \code{length} characters. 
}
\author{ J.R. Lobry }
\references{ 
  \code{citation("seqinr")}
}
\seealso{ \code{\link{kronecker}}, \code{\link{outer}} }
\examples{
## Get all codons
words()
## Get all codons with a c g u for bases
words(alphabet = c("a", "c", "g", "u"))
## Get all tetranucleotides
words(length = 4)
## Get all dipeptides
words(length = 2, alphabet = s2c("ACDEFGHIKLMNPQRSTVWY"))
}
\keyword{ utilities }
