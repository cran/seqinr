\name{getName}
\alias{getName}
\alias{getName.default}
\alias{getName.list}
\alias{getName.SeqFrag}
\alias{getName.SeqAcnucWeb}
\alias{getName.SeqFastadna}
\alias{getName.SeqFastaAA}
\alias{getName.qaw}
\alias{getName.logical}

\title{Generic function to get the names of sequences}
\description{
GetName returns the sequence names.
}
\usage{
getName(object, ...)
}
\arguments{
  \item{object}{ an object of the class \code{\link{SeqAcnucWeb}}
    or \code{\link{SeqFastadna}}, or \code{\link{SeqFastaAA}} or \code{\link{SeqFrag}} or a list of these objects }
  \item{...}{further arguments passed to or from other methods}
}
\value{
  an object of class \code{character} containing the names of the sequences}
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
# List all available methods for getName generic function:
#
   methods(getName)
#
# Example with seven DNA sequences from a FASTA file:
#
  ff <- system.file("sequences/someORF.fsa", package = "seqinr")
  fs <- read.fasta(file = ff)
  stopifnot(all(getName(fs) == c("YAL001C", "YAL002W", "YAL003W",
    "YAL005C", "YAL007C", "YAL008W", "YAL009W")))
#
# Example with 49 sequences from an ACNUC server:
#
  \dontrun{
  # Need internet connection
  choosebank("emblTP")
  fc <- query("fc", "sp=felis catus et t=cds et o=mitochondrion")
  getName(fc)
  closebank()  
}
}
\keyword{ utilities }
