\name{getLength}
\alias{getLength}
\alias{getLength.SeqAcnucWeb}
\alias{getLength.SeqFastadna}
\alias{getLength.SeqFastaAA}
\alias{getLength.SeqFrag}
\alias{getLength.default}
\alias{getAttributsocket}
\title{Generic Function To obtain the length of a sequence}
\description{
  Methods for the classes of sequences: \code{SeqFastadna},  \code{SeqFastaAA},  \code{SeqAcnucWeb}, \code{SeqFrag}. 
}
\usage{
getLength(object)
}
\arguments{
   \item{object}{ an object of class \code{SeqFastadna},  \code{SeqFastaAA},  \code{SeqAcnucWeb} or \code{SeqFrag}. }
}
\value{
  a numeric vector giving the length of the sequence
}
\references{ 
To have an overview of the seqinR's functionnality, please consult this vignette: 
Charif, D., Lobry, J.R. (2005) SeqinR: a contributed package to the R project for statistical
computing devoted to biological sequences retrieval and analysis. Springer Verlag, \emph{Biological and Medical Physics/Biomedical Series}, in preparation. \cr
}
\author{ D. Charif } 
\seealso{ \code{\link{getName}}, \code{\link{getFrag}} }
\examples{
 methods(getLength)
}
\keyword{ utilities }
