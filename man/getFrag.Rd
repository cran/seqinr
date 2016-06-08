\name{getFrag}
\alias{getFrag}
\alias{getFrag.default}
\alias{getFrag.list}
\alias{getFrag.character}
\alias{getFrag.SeqFrag}
\alias{getFrag.SeqAcnucWeb}
\alias{getFrag.SeqFastadna}
\alias{getFrag.SeqFastaAA}
\alias{getFrag.qaw}
\alias{getFrag.logical}

\title{Generic function to extract sequence fragments}
\description{
getFrag is used to extract the sequence fragment starting at the \code{begin} position
and ending at the \code{end} position.
}
\usage{
getFrag(object, begin, end, ...)
\method{getFrag}{SeqAcnucWeb}(object, begin, end, ..., socket = autosocket(), name = getName(object))
\method{getFrag}{SeqFastadna}(object, begin, end, ..., name = getName(object))
\method{getFrag}{SeqFastaAA}(object, begin, end, ..., name = getName(object))
\method{getFrag}{SeqFrag}(object, begin, end, ..., name = getName(object))

}
\arguments{
  \item{object}{ an object of the class \code{\link{SeqAcnucWeb}}
    or \code{\link{SeqFastadna}}, or \code{\link{SeqFastaAA}} or \code{\link{SeqFrag}} or a list of these objects }
  \item{begin}{ First position of the fragment to extract. This position is included. Numerotation starts at 1.}
  \item{end}{ Last position of the fragment to extract. This position is included. }
  \item{socket}{an object of class \code{sockconn} connecting to a remote ACNUC
                        database (default is a socket to the last opened database by \code{\link{choosebank}}).}
  \item{name}{the sequence name}
  \item{...}{further arguments passed to or from other methods}
}
\value{
  \code{getFrag} returns an object of class \code{\link{SeqFrag}}.
}
\references{
  \code{citation("seqinr")}
}
\author{D. Charif, J.R. Lobry, L. Palmeira} 
\seealso{
  \code{\link{SeqAcnucWeb}}, \code{\link{SeqFastadna}}, \code{\link{SeqFastaAA}}, \code{\link{SeqFrag}}
}
\examples{
#
# List all available methods for getFrag generic function:
#
   methods(getFrag)
#
# Example with a DNA sequence from a FASTA file:
#
  dnafile <- system.file("sequences/malM.fasta", package = "seqinr")
  sfdna <- read.fasta(file = dnafile)
  myfrag <- getFrag(sfdna[[1]], begin = 1, end = 10)
  stopifnot(getSequence(myfrag, as.string = TRUE) == "atgaaaatga")
}
\keyword{ utilities }
