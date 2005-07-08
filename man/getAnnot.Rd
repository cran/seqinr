\name{getAnnot}
\alias{getAnnot}
\alias{getAnnot.SeqAcnucWeb}
\alias{getAnnot.SeqFastadna}
\alias{getAnnot.SeqFastaAA}
\alias{getAnnot.default}
\alias{readAnnots.socket}
\alias{getAnnots}
\title{ Generic Function to obtain annotation for a sequence }
\description{
  Methods for the classes of sequences: \code{SeqFastadna},  \code{SeqFastaAA},  \code{SeqAcnucWeb}. 
}
\usage{
getAnnot(object, nbl)
}
\arguments{
  \item{object}{ one of this object used to select a method}
  \item{nbl}{ the number of line of annotation to read}
}
\value{
  a vector of sting of character containing the annotation. 
}
\references{ 
  \code{citation("seqinr")}
}
\author{ D. Charif } 
\examples{
  methods(getAnnot)
}
\keyword{ utilities }
