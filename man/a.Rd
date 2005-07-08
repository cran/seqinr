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
  \item{aa}{ A vector of string. All strings are 3 chars long. }
}
\details{
Allowed character values for \code{aa} are: Stp, Ala, Cys, Asp, Glu, Phe, Gly, His, Ile, Lys, Leu, Met, Asn, Pro, Gln, Arg, Ser, Thr, Val, Trp, Tyr.
All other values will generate a warning and return NA. 
}
\value{
A vector of single characters. Character values are *ACDEFGHIKLMNPQRSTVWY.
}
\references{
The IUPAC one-letter code for aminoacids is described at: 
\url{http://www.chem.qmul.ac.uk/iupac/AminoAcid/} 
\code{citation("seqinr")}
}
\author{ D. Charif }
\seealso{ \code{\link{translate}}, \code{\link{aaa}} }
\examples{
  # show all allowed values
  allowed <- c("Stp", "Ala", "Cys", "Asp", "Glu", "Phe", "Gly", "His", "Ile",
           "Lys", "Leu", "Met", "Asn", "Pro", "Gln", "Arg", "Ser", "Thr",
           "Val", "Trp", "Tyr")
  a(allowed)
  # show what's happen with non-allowed values
  a("SOS") # should be NA and a warning is generated
}
\keyword{ utilities }
