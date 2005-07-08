\name{getSequence}
\alias{getSequence}
\alias{getSequence.SeqAcnucWeb}
\alias{getSequence.SeqFastadna}
\alias{getSequence.SeqFastaAA}
\alias{getSequence.SeqFrag}
\alias{getSequence.default}
\alias{getNumber.socket}
\alias{getSequenceSocket}
\title{ Generic Function To Get a sequence }
\description{
   Method for the classes of sequences:  \code{SeqFastadna},  \code{SeqFastaAA},  \code{SeqAcnucWeb} and \code{SeqFrag}. 
}
\usage{
getSequence(object, as.string = FALSE)
}
\arguments{
  \item{object}{an object of class \code{SeqFastadna},  \code{SeqFastaAA},  \code{SeqAcnucWeb} or \code{SeqFrag}.}
  \item{as.string}{if TRUE sequences are returned as a string instead of a vector of chars.}
}
\value{
 A vector of chars containing the sequence (default) or a string when \code{as.string} is set to TRUE.
}
\references{
  \code{citation("seqinr")}
}
\author{ D. Charif } 
\seealso{ \code{\link{getFrag}}, \code{\link{c2s}} }
\examples{
 methods(getSequence)
}
\keyword{ utilities }
