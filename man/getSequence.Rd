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
getSequence(object)
}
\arguments{
  \item{object}{an object of class \code{SeqFastadna},  \code{SeqFastaAA},  \code{SeqAcnucWeb} or \code{SeqFrag}.}
}
\value{
 A vector of chars containing the sequence
}
\references{ 
To have an overview of the seqinR's functionnality, please consult this vignette: 
Charif, D., Lobry, J.R. (2005) SeqinR: a contributed package to the R project for statistical
computing devoted to biological sequences retrieval and analysis. Springer Verlag, \emph{Biological and Medical Physics/Biomedical Series}, in preparation. \cr
}
\author{ D. Charif } 
\seealso{ \code{\link{getFrag}}, \code{\link{c2s}} }
\examples{
 methods(getSequence)
}
\keyword{ utilities }
