\name{getAnnot}
\alias{getAnnot}
\alias{getAnnot.default}
\alias{getAnnot.list}
\alias{getAnnot.SeqAcnucWeb}
\alias{getAnnot.SeqFastadna}
\alias{getAnnot.SeqFastaAA}
\alias{getAnnot.qaw}
\alias{getAnnot.logical}
\alias{readAnnots.socket}

\title{Generic Function to get sequence annotations}
\description{
Annotations are taken from the \code{Annot} attribute for sequences
imported from a FASTA file and retrieved from an ACNUC server for
objects of the \code{SeqAcnucWeb} class.
}
\usage{
getAnnot(object, ...)
\method{getAnnot}{SeqAcnucWeb}(object, ..., nbl = 100, socket = autosocket())
}
\arguments{
  \item{object}{ an object of the class \code{SeqAcnucWeb}
    or \code{SeqFastadna}, or \code{SeqFastaAA} or a list of these objects }
  \item{nbl}{ the maximum number of line of annotation to read. Reading of 
     lines stops when nbl lines have been transmitted or at the last annotation 
     line of the sequence (SQ or ORIGIN line). }
  \item{socket}{an object of class \code{sockconn} connecting to a remote ACNUC
                        database (default is a socket to the last opened database).}
  \item{...}{further arguments passed to or from other methods}
}
\value{
  \code{getAnnot} returns a vector of string of characters containing the
  annotations for the sequences.
}
\references{
  \code{citation("seqinr")}
}
\author{D. Charif, J.R. Lobry, L. Palmeira} 
\seealso{
  \code{\link{query}}, \code{\link{SeqAcnucWeb}}, \code{\link{c2s}}, \code{\link{translate}} and \code{\link{prepgetannots}} to select the annotation lines.
}
\examples{
#
# List all available methods for getAnnot generic function:
#
   methods(getAnnot)
#
# SeqAcnucWeb class example:
#
  \dontrun{
  # Need internet connection
  choosebank("emblTP")
  fc<-query("fc", "sp=felis catus et t=cds et O=mitochondrion et Y>2001 et no k=partial")
  # get the first 5 lines annotating the first sequence:
  annots <- getAnnot(fc$req[[1]], nbl = 5)
  cat(annots, sep = "\n")
  # or use the list method to get them all at once:
  annots <- getAnnot(fc$req, nbl = 5)
  cat(annots, sep = "\n")
  closebank()
  }
#
# SeqFastaAA class example:
#
   aafile <- system.file("sequences/seqAA.fasta", package = "seqinr")
   sfaa <- read.fasta(aafile, seqtype = "AA")
   getAnnot(sfaa[[1]])
#
# SeqFastadna class example:
#
   dnafile <- system.file("sequences/malM.fasta", package = "seqinr")
   sfdna <- read.fasta(file = dnafile)
   getAnnot(sfdna[[1]])
#
# Example with a FASTA file with multiple entries:
#
  ff <- system.file("sequences/someORF.fsa", package = "seqinr")
  fs <- read.fasta(ff)
  getAnnot(fs) # the list method is used here to get them all at once
#
# Default getAnnot method example. An error is produced because 
# there are no annotations by default:
#
   result <- try(getAnnot(letters))
   stopifnot(!inherits("result", "try-error"))
}
\keyword{ utilities }
