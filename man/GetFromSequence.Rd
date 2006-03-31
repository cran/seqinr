\name{GetFromSequence}
\alias{getAnnot}
\alias{getAnnot.SeqAcnucWeb}
\alias{getAnnot.SeqFastadna}
\alias{getAnnot.SeqFastaAA}
\alias{getAnnot.default}
\alias{readAnnots.socket}
\alias{getAnnots}
\alias{getFrag}
\alias{getFrag.SeqAcnucWeb}
\alias{getFrag.SeqFastadna}
\alias{getFrag.SeqFastaAA}
\alias{getFrag.SeqFrag}
\alias{getFrag.default}
\alias{getKeyword}
\alias{getKeyword.SeqAcnucWeb}
\alias{getKeywordsocket}
\alias{getKeyword.default}
\alias{getLength}
\alias{getLength.SeqAcnucWeb}
\alias{getLength.SeqFastadna}
\alias{getLength.SeqFastaAA}
\alias{getLength.SeqFrag}
\alias{getLength.default}
\alias{getAttributsocket}
\alias{getLocation}
\alias{getLocation.SeqAcnucWeb}
\alias{getLocation.default}
\alias{getLocationSocket}
\alias{getExon}
\alias{getName}
\alias{getName.SeqAcnucWeb}
\alias{getName.SeqFastadna}
\alias{getName.SeqFastaAA}
\alias{getName.SeqFrag}
\alias{getName.default}
\alias{getSequence}
\alias{getSequence.SeqAcnucWeb}
\alias{getSequence.SeqFastadna}
\alias{getSequence.SeqFastaAA}
\alias{getSequence.SeqFrag}
\alias{getSequence.default}
\alias{getNumber.socket}
\alias{getSequenceSocket}
\alias{getTrans}
\alias{getTrans.SeqAcnucWeb}
\alias{getTrans.SeqFastadna}
\alias{getTrans.SeqFastaAA}
\alias{getTrans.SeqFrag}
\alias{getTrans.default}
\title{
  Generic Functions to obtain annotation, fragment, associated
  keyword(s), length, location, name, sequence, or translation for a
  sequence
}
\description{
  All methods apply on sequences of class \code{SeqAcnucWeb}.

  \code{getAnnot}, \code{getFrag}, \code{getLength}, \code{getName},
  \code{getSequence}, and \code{getTrans} can also apply on sequences of
  classes \code{SeqFastadna} and \code{SeqFastaAA}.
  
  \code{getFrag}, \code{getLength}, \code{getName}, \code{getSequence}
  and \code{getTrans} can moreover apply on sequences of classes
  \code{SeqFrag}.
}
\usage{
getAnnot(object, nbl = 10000)
getFrag(object, begin, end)
getKeyword(object)
getLength(object)
getLocation(object)
getName(object)
getSequence(object, as.string = FALSE)
getTrans(object, frame = 0, sens = "F", numcode = 1, NAstring = "X", ambiguous = FALSE)
}
\arguments{
  \item{object}{ an object of the class \code{SeqAcnucWeb}
    [ or of the classes \code{SeqFastadna}, \code{SeqFastaAA}] [ or of
    the class \code{SeqFrag}] }
  \item{nbl}{ the maximum number of line of annotation to read. Reading of 
     lines stops when nl lines have been transmitted or at the last annotation 
     line of the sequence (SQ or ORIGIN line). }
  \item{begin}{ First base }
  \item{end}{ Last base }
  \item{as.string}{if TRUE sequences are returned as a string instead of a vector of chars.}
  \item{frame}{ Frame(s) (0,1,2) to translate. By default the frame \code{0} is used. }
  \item{sens}{ Sense to translate: \code{F} for forward sense and \code{R} for revers sense. }
  \item{numcode}{ The number of the code to use. By default the standard genetic code is used. }
  \item{NAstring}{ How to translate amino-acids when there are ambiguous bases in codons. }
  \item{ambiguous}{ If TRUE, ambiguous bases are taken into account so that for instance
  GGN is translated to Gly in the standard genetic code. }
}
\value{
  \code{getAnnot} returns a vector of string of characters containing the
  annotation.
  
  \code{getFrag} returns an object of class SeqFrag, which is a vector of
  chars with many attributes (see \code{\link{SeqFrag}}).
  
  \code{getKeyword} returns a vector of strings containing the keyword(s)
  associated to a sequence.
  
  \code{getLength} returns a  numeric vector giving the length of the
  sequence.
  
  \code{getLocation} returns a list giving the positions of the sequence
  on the parent sequence. If the sequence is a subsequence (e.g. coding
  sequence), the function will return the position of each exon on the
  parent sequence.

  \code{getName} returns a string of characters containing the name of the
  sequence.
  
  \code{getSequence} returns a vector of chars containing the sequence
  (default) or a string when \code{as.string} is set to TRUE.
  
  \code{getTrans} returns a vector of chars containing the sequence.
}
\references{
  \code{citation("seqinr")}
}
\author{ D. Charif and J.R. Lobry and L. Palmeira} 
\seealso{
  \code{\link{query}}, \code{\link{SeqAcnucWeb}}, \code{\link{c2s}}, \code{\link{translate}}
}
\examples{
###
### List all available methods for getSequence generic function:
###
  methods(getSequence)
#
# SeqAcnucWeb class example:
#
  \dontrun{
  # Need internet connection for getSequence.SeqAcnucWeb()
  choosebank("emblTP")
  query("fc", "sp=felis catus et t=cds")
  getSequence(fc$req[[1]])
  getSequence(fc$req[[1]], as.string = TRUE)
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
  dnafile <- system.file("sequences/malM.fasta", package = "seqinr")
  sfdna <- read.fasta(file = dnafile)
  getSequence(sfdna[[1]])
  getSequence(sfdna[[1]], as.string = TRUE)
#
# SeqFrag class example:
#
  sfrag <- getFrag(object = sfdna[[1]], begin = 1, end = 10)
  getSequence(sfrag)
  getSequence(sfrag, as.string = TRUE)
#
# Default getSequence method example:
#
  getSequence(letters)
  getSequence(letters, as.string = TRUE)
  
###
### List all available methods for getAnnot generic function:
###
  methods(getAnnot)
#
# SeqAcnucWeb class example:
#
  \dontrun{
  # Need internet connection for getAnnot.SeqAcnucWeb()
  choosebank("emblTP")
  query("fc", "sp=felis catus et t=cds")
  annots <- getAnnot(fc$req[[1]])
  cat(annots, sep = "\n")
  }
#
# SeqFastaAA class example:
#
  getAnnot(sfaa[[1]])
#
# SeqFastadna class example:
#
  getAnnot(sfdna[[1]])
#
# Default getAnnot method example:
#
  \dontrun{
  # An error is produced because there are no annotations by default
  getAnnot(letters)
  }

###
### List all available methods for getKeyword generic function:
###
  methods(getKeyword)
#
# SeqAcnucWeb class example:
#
  \dontrun{
  # Need internet connection for getKeyword.SeqAcnucWeb()
  choosebank("emblTP")
  query("fc", "sp=felis catus et t=cds")
  getKeyword(fc$req[[1]])
  # Should be: [1] "DIVISION ORG" "RELEASE 62"   "CYTOCHROME B" "SOURCE"       "CDS"
  }
#
# Default getKeyword method example:
#
  \dontrun{
  # An error is produced because there are no keywords by default
  getKeyword(letters)
  }

###
### List all available methods for getFrag generic function:
###
  methods(getFrag)

###
### List all available methods for getLength generic function:
###
  methods(getLength)

###
### List all available methods for getLocation generic function:
###
  methods(getLocation)

###
### List all available methods for getName generic function:
###
  methods(getName)
#
# SeqAcnucWeb class example:
#
  \dontrun{
  # Need internet connection for getName.SeqAcnucWeb()
  choosebank("emblTP")
  query("fc", "sp=felis catus et t=cds")
  sapply(fc$req, getName)
  }
###
### List all available methods for getTrans generic function:
###
  methods(getTrans)
}
\keyword{ utilities }
