\name{computePI}
\alias{computePI}
\title{To Compute the Theoretical Isoelectric Point}
\description{
  This function calculates the theoretical isoelectric point of a protein. Isoelectric point is the pH at which the protein has a neutral charge.
This estimate does not account for the post-translational modifications.
}
\usage{
computePI(seq)
}
\arguments{
  \item{seq}{ Protein sequence as a vector of single chars in upper case}
}
\value{
  The theoretical isoelectric point (pI) as a numerical vector of length one.
}
\references{ 
The algorithm is the same as the one which is implemented at the following url:  
\url{https://web.expasy.org/compute_pi/pi_tool-doc.html} but with many trials
in case of convergence failure of the non-linear regression procedure.
\code{citation("seqinr")}
 }
\author{D. Charif, J.R. Lobry}
\note{ 
Protein pI is calculated using pK values of amino acids described in  Bjellqvist et al. See also \code{SEQINR.UTIL} for more details.
 }
\seealso{\code{\link{SEQINR.UTIL}} }
\examples{
#
# Simple sanity check with all 20 amino-acids in one-letter code alphabetical order:
#
prot <- s2c("ACDEFGHIKLMNPQRSTVWY")
stopifnot(all.equal(computePI(prot), 6.78454))
#
# Read a protein sequence in a FASTA file and then compute its pI :
#
myProts <- read.fasta(file = system.file("sequences/seqAA.fasta",
 package = "seqinr"), seqtype = "AA")
computePI(myProts[[1]]) # Should be 8.534902
}
\keyword{manip}
