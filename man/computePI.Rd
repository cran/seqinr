\name{computePI}
\alias{computePI}
\title{ To Compute the Theoretical Isoelectric Point }
\description{
  This function calculates the theoretical isoelectric point of a protein. Isoelectric point is the pH at which the protein has a neutral charge.
These estimate does not account for the post-translational modifications.
}
\usage{
computePI(seq)
}
\arguments{
  \item{seq}{ AA sequence as a vector of chars }
}
\value{
  It returns the pI.
}
\references{ 
The algorithm is quite the same as the one which is implemented at the following url:  
\url{http://www.expasy.org/tools/pi_tool-doc.html}
\code{citation("seqinr")}
 }
\author{ D. Charif }
\note{ 
Protein pI is calculated using pK values of amino acids described in  Bjellqvist et al. See also \code{SEQINR.UTIL} for more details.
 }
\seealso{\code{\link{SEQINR.UTIL}} }
\examples{
ss = read.fasta(File=system.file("sequences/seqAA.fasta",package = "seqinr"),seqtype="AA")
computePI(ss[[1]]) 
}
\keyword{manip}
