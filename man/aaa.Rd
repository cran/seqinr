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
To have an overview of the seqinR's functionnality, please consult this vignette: 
Charif, D., Lobry, J.R. (2005) SeqinR: a contributed package to the R project for statistical
computing devoted to biological sequences retrieval and analysis. Springer Verlag, \emph{Biological and Medical Physics/Biomedical Series}, in preparation. \cr
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
