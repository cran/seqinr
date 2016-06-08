\name{getSequence}
\alias{getSequence}
\alias{getSequence.default}
\alias{getSequence.list}
\alias{getSequence.character}
\alias{getSequence.SeqFrag}
\alias{getSequence.SeqAcnucWeb}
\alias{getSequence.SeqFastadna}
\alias{getSequence.SeqFastaAA}
\alias{getSequence.qaw}
\alias{getSequence.logical}

\title{Generic function to get sequence data}
\description{
getSequence returns the sequence either as vector of single chararacters
or as a single string of multiple characters.
}
\usage{
getSequence(object, as.string = FALSE, ...)
\method{getSequence}{SeqAcnucWeb}(object, as.string = FALSE, ..., socket = autosocket())
}
\arguments{
  \item{object}{ an object of the class \code{\link{SeqAcnucWeb}}
    or \code{\link{SeqFastadna}}, or \code{\link{SeqFastaAA}} or \code{\link{SeqFrag}} or a list of these objects, or an object of class \code{qaw} created by \code{\link{query}} }
  \item{as.string}{if TRUE sequences are returned as strings of multiple characters
    instead of a vector of single characters}
  \item{socket}{an object of class \code{sockconn} connecting to a remote ACNUC
                        database (default is a socket to the last opened database).}
  \item{...}{further arguments passed to or from other methods}
}
\value{
  For a single sequence an object of class \code{character} containing the characters
  of the sequence, either of length 1 when \code{as.string} is TRUE, or of the length
  of the sequence when \code{as.string} is FALSE. For many sequences, a list of these.}
\references{
  \code{citation("seqinr")}
}
\author{D. Charif, J.R. Lobry, L. Palmeira} 
\seealso{
  \code{\link{SeqAcnucWeb}}, \code{\link{SeqFastadna}}, 
  \code{\link{SeqFastaAA}}, \code{\link{SeqFrag}}
}
\examples{
#
# List all available methods for getSequence generic function:
#
   methods(getSequence)
#
# SeqAcnucWeb class example:
#
  \dontrun{# Need internet connection
  choosebank("emblTP")
  fc <- query("fc", "sp=felis catus et t=cds et o=mitochondrion")
  getSequence(fc$req[[1]])
  getSequence(fc$req[[1]], as.string = TRUE)
  closebank()
  }
#
# SeqFastaAA class example:
#
  aafile <- system.file("sequences/seqAA.fasta", package = "seqinr")
  sfaa <- read.fasta(aafile, seqtype = "AA")
  getSequence(sfaa[[1]])
  getSequence(sfaa[[1]], as.string = TRUE)
#
# SeqFastadna class example:
#
  dnafile <- system.file("sequences/someORF.fsa", package = "seqinr")
  sfdna <- read.fasta(file = dnafile)
  getSequence(sfdna[[1]])
  getSequence(sfdna[[1]], as.string = TRUE)
#
# SeqFrag class example:
#
  sfrag <- getFrag(object = sfdna[[1]], begin = 1, end = 10)
  getSequence(sfrag)
  getSequence(sfrag, as.string = TRUE)
}
\keyword{ utilities }
