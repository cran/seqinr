\name{SeqFrag}
\alias{SeqFrag}
\alias{is.SeqFrag}
\alias{as.SeqFrag}
\title{Class for sub-sequences}
\description{
  \code{as.SeqFrag} is called by all methods of \code{getFrag}, but not directly by the users. It creates an object sequence of class \code{SeqFrag}. 
}
\usage{
 as.SeqFrag(object, begin, end, compl = FALSE, name = "frag")
 is.SeqFrag(object)
}
\arguments{
  \item{object}{ an object sequence of class \code{seqFastadna}, \code{seqFastaAA}, \code{seqAcnucWeb} or \code{seqFrag} }
  \item{begin}{ the first base of the fragment to get }
  \item{end}{ the last base of the fragment to get }
  \item{compl}{ if TRUE, you must give a name for the sequence}
  \item{name}{ the name of the fragment of the sequence }
}
\value{
  \code{as.SeqFrag} returns a biological sequence representing by a vector of chars with the following attributes:
  \item{seqMother}{ the name of the sequence from which the sequence comes}
  \item{begin}{ the position of the first base of the fragment on the mother sequence}
  \item{end}{ the position of the last base of the fragment on the mother sequence }
  \item{class}{ \code{SeqFrag} which is the newest class of the sequence } 
  \code{is.SeqFrag} returns TRUE if the object is of class Seqfrag. 
}
\author{ D. Charif }
\references{ 
  \code{citation("seqinr")}
}
\seealso{\code{\link{getFrag}} }
\examples{
 s = read.fasta(File=system.file("sequences/malM.fasta",package = "seqinr"))
 getFrag(s[[1]],1,10) 	
}
\keyword{utilities}
