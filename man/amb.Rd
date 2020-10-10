\name{amb}
\alias{amb}
\title{Expansion of IUPAC nucleotide symbols}
\description{
This function returns the list of nucleotide matching a given IUPAC
nucleotide symbol, for instance \code{c("c", "g")} for \code{"s"}.
}
\usage{
amb(base, forceToLower = TRUE, checkBase = TRUE,
IUPAC = s2c("acgturymkswbdhvn"), u2t = TRUE)
}
\arguments{
  \item{base}{an IUPAC symbol for a nucleotide as a single character}
  \item{forceToLower}{if TRUE the base is forced to lower case}
  \item{checkBase}{if TRUE the character is checked to belong to the allowed IUPAC symbol list}
  \item{IUPAC}{the list of allowed IUPAC symbols}
  \item{u2t}{if TRUE "u" for uracil in RNA are changed into "t" for thymine in DNA}
}
\details{
Non ambiguous bases are returned unchanged (except for "u" when u2t is TRUE).
}
\value{
When base is missing, the list of IUPAC symbols is returned, otherwise
a vector with expanded symbols.
}
\references{ 

The nomenclature for incompletely specified bases in nucleic acid sequences
at: \url{https://www.ncbi.nlm.nih.gov/pmc/articles/PMC341218/}

\code{citation("seqinr")}
}
\author{J.R. Lobry}

\seealso{See \code{\link{bma}} for the reverse operation. 
Use \code{\link{tolower}} to change upper case letters into
lower case letters.}

\examples{
#
# The list of IUPAC symbols:
#

amb()

#
# And their expansion:
#

sapply(amb(), amb)

}
\keyword{utilities}
