\name{getLocation}
\alias{getLocation}
\alias{getLocation.SeqAcnucWeb}
\alias{getLocation.default}
\alias{getLocationSocket}
\alias{getExon}
\title{ Generic Function To obtain the Position of a Sequence on the Parent Sequence }
\description{
  Methods for the class of sequences: \code{SeqAcnucWeb}. If the sequence is a subsequence (e.g. coding sequence), the function will return the position of each exon on the parent sequence.
}
\usage{
getLocation(object)
}
\arguments{
  \item{object}{ an object of the class \code{SeqAcnucWeb}.}
}
\value{
  a list giving the positions of the sequence on the parent sequence.
}
\references{ 
To have an overview of the seqinR's functionnality, please consult this vignette: 
Charif, D., Lobry, J.R. (2005) SeqinR: a contributed package to the R project for statistical
computing devoted to biological sequences retrieval and analysis. Springer Verlag, \emph{Biological and Medical Physics/Biomedical Series}, in preparation. \cr
}
\author{ D. Charif } 
\seealso{ \code{\link{SeqAcnucWeb}}, \code{\link{query}} }
\examples{
  methods(getLocation)
}
\keyword{ utilities }
