\name{getName}
\alias{getName}
\alias{getName.SeqAcnucWeb}
\alias{getName.SeqFastadna}
\alias{getName.SeqFastaAA}
\alias{getName.SeqFrag}
\alias{getName.default}
\title{ Generic Function To obtain the name of a sequence }
\description{
  Method for the class of sequences: \code{SeqFastadna},  \code{SeqFastaAA},  \code{SeqAcnucWeb} or \code{SeqFrag}. 
}
\usage{
getName(object)
}
\arguments{
  \item{object}{ an object of class \code{SeqFastadna},  \code{SeqFastaAA},  \code{SeqAcnucWeb} or \code{SeqFrag}}
}
\value{
  A string of chars containing the name of the sequence.
}
\references{ 
To have an overview of the seqinR's functionnality, please consult this vignette: 
Charif, D., Lobry, J.R. (2005) SeqinR: a contributed package to the R project for statistical
computing devoted to biological sequences retrieval and analysis. Springer Verlag, \emph{Biological and Medical Physics/Biomedical Series}, in preparation. \cr
}
\author{ D. Charif } 
\seealso{ \code{\link{getFrag}} }
\examples{
  methods(getName)
}
\keyword{ utilities }
