\name{getLength}
\alias{getLength}
\alias{getLength.default}
\alias{getLength.list}
\alias{getLength.character}
\alias{getLength.SeqFrag}
\alias{getLength.SeqAcnucWeb}
\alias{getLength.SeqFastadna}
\alias{getLength.SeqFastaAA}
\alias{getLength.qaw}
\alias{getLength.logical}

\title{Generic function to get the length of sequences}
\description{
getLength returns the total number of bases or amino-acids in a sequence.
}
\usage{
getLength(object, ...)
}
\arguments{
  \item{object}{ an object of the class \code{\link{SeqAcnucWeb}}
    or \code{\link{SeqFastadna}}, or \code{\link{SeqFastaAA}} or \code{\link{SeqFrag}} or a list of these objects }
  \item{...}{further arguments passed to or from other methods}
}
\value{
  \code{getLength} returns a numeric vector giving the length of the
    sequences.}
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
# List all available methods for getLength generic function:
#
   methods(getLength)
#
# Example with seven DNA sequences from a FASTA file:
#
  ff <- system.file("sequences/someORF.fsa", package = "seqinr")
  fs <- read.fasta(file = ff)
  stopifnot(all(getLength(fs) == c(5573, 5825, 2987, 3929, 2648, 2597, 2780)))
#
# Example with 49 sequences from an ACNUC server:
#
  \dontrun{
  # Need internet connection
  choosebank("emblTP")
  fc <- query("fc", "sp=felis catus et t=cds et o=mitochondrion")
  getLength(fc)
  closebank()  
}
}
\keyword{ utilities }
