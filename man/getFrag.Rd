\name{getFrag}
\alias{getFrag}
\alias{getFrag.SeqAcnucWeb}
\alias{getFrag.SeqFastadna}
\alias{getFrag.SeqFastaAA}
\alias{getFrag.SeqFrag}
\alias{getFrag.default}
\title{ Generic Function to obtain a fragment of a sequence }
\description{
  Methods for the classes of sequences: \code{SeqFastadna},  \code{SeqFastaAA},  \code{SeqAcnucWeb}, \code{SeqFrag}. 
}
\usage{
getFrag(object, begin, end)
}
\arguments{
  \item{object}{ an object of the class \code{SeqFastadna}, \code{SeqFastaAA},  \code{SeqAcnucWeb} or \code{SeqFrag} }
  \item{begin}{ First base }
  \item{end}{ Last base }
}
\value{
  An object of class SeqFrag, which is a vector of chars with many attributes:
  \item{seqMother}{ the name of the sequence from which the sequence comes}
  \item{begin}{ the position of the first base of the fragment on the mother sequence}
  \item{end}{ the position of the last base of the fragment on the mother sequence }
  \item{class}{ the class of the new sequence i.e. \code{SeqFrag} } 
}
\references{ 
  \code{citation("seqinr")}
}
\author{ D. Charif } 
\seealso{ \code{\link{getSequence}} }
\examples{
  methods(getFrag)
}
\keyword{ utilities }
