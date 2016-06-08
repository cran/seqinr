\name{SeqFrag}
\alias{SeqFrag}
\alias{is.SeqFrag}
\alias{as.SeqFrag}
\title{Class for sub-sequences}
\description{
  \code{as.SeqFrag} is called by all methods of \code{\link{getFrag}}, but not directly by the users. It creates an object sequence of class \code{SeqFrag}. 
}
\usage{
 as.SeqFrag(object, begin, end, name)
 is.SeqFrag(object)
}
\arguments{
  \item{object}{ an object sequence of class \code{seqFastadna}, \code{seqFastaAA}, \code{seqAcnucWeb} or \code{seqFrag} }
  \item{begin}{ the first base of the fragment to get }
  \item{end}{ the last base of the fragment to get }
  \item{name}{ the name of the sequence }
}
\value{
  \code{as.SeqFrag} returns a biological sequence with the following attributes:
  \item{seqMother}{ the name of the sequence from which the sequence comes}
  \item{begin}{ the position of the first base of the fragment on the mother sequence}
  \item{end}{ the position of the last base of the fragment on the mother sequence }
  \item{class}{ \code{SeqFrag} which is the classfor sub-sequence } 
  \code{is.SeqFrag} returns TRUE if the object is of class Seqfrag. 
}
\author{D. Charif, J.R. Lobry}
\references{
  \code{citation("seqinr")}
}
\seealso{\code{\link{getFrag}}, \code{\link{getLength}}, \code{\link{getName}},
  \code{\link{getSequence}}, \code{\link{getTrans}} }
\examples{
 s <- read.fasta(file = system.file("sequences/malM.fasta", package = "seqinr"))
 getFrag(s[[1]], 1, 10) 	
}
\keyword{utilities}
