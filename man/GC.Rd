\name{G+C Content}
\alias{GC}
\alias{GC1}
\alias{GC2}
\alias{GC3}
\title{Calculates the fractional G+C content of nucleic acid sequences.}
\description{
  Calculates the fraction of G+C bases of the input nucleic acid
  sequence(s). It reads in nucleic acid sequences, sums the number of
  'g' and 'c' bases and writes out the result as the fraction (in the
  interval 0.0 to 1.0) of the length of the whole sequence. Global G+C
  content \code{GC}, G+C in the first position of the codon bases
  \code{GC1}, G+C in the second position of the codon bases
  \code{GC2}, and G+C in the third position of the codon bases
  \code{GC3} can be computed.
}
\usage{
GC(seq, forceToLower = TRUE, exact = TRUE)
GC1(seq, ...)
GC2(seq, ...)
GC3(seq, ...)
}
\arguments{
  \item{seq}{a nucleic acid sequence as a vector of single characters in lower case}
  \item{forceToLower}{logical defaulting to \code{TRUE}: force sequence
  	characters in lower-case. Turn this to \code{FALSE} to save time
  	if your sequence is already in lower-case}
  \item{exact}{logical defaulting to \code{FALSE}: should ambiguous bases taken
  	into account when computing the G+C content (see details)}
  \item{...}{arguments passed to the function \code{GC}}
}
\details{
	When \code{exact} is set to \code{TRUE} the G+C content is estimated 
	with ambiguous bases taken into account. Note that this is time expensive.
	A first pass is made on non-ambiguous bases to estimate the probabilities
	of the four bases in the sequence. They are then used to weight the
	contributions of ambiguous bases to the G+C content. Let note nx
	the total number of base 'x' in the sequence. For instance
	suppose that there are nb bases 'b'. 'b' stands for "not a", that
	is for 'c', 'g' or 't'. The contribution of 'b' bases to the GC base
	count will be:
	
	nb*(nc + ng)/(nc + ng + nt)
	
	The contribution of 'b' bases to the AT base count will be:
	
	nb*nt/(nc + ng + nt)
	
	All ambiguous bases contributions to the AT and GC counts are weighted
	is similar way and then the G+C content is computed as ngc/(nat + ngc).
}
\value{
  \code{GC} returns the fraction of G+C as a numeric vector,
  \code{GC1} returns the fraction of G+C at first codon position as a numeric vector,
  \code{GC2} returns the fraction of G+C at second codon position  as a numeric vector,
  \code{GC3} returns the fraction of G+C at third codon position  as a numeric vector.
}
\references{
  \code{citation("seqinr")}.
  
  The program codonW used here for comparison is available at 
  \url{http://codonw.sourceforge.net/}.
}
\seealso{You can use \code{\link{s2c}} to convert a string into a vetor of single
character and \code{\link{tolower}} to convert upper-case characters into
lower-case characters.
}
\author{D. Charif and L. Palmeira and J.R. Lobry}
\examples{
   mysequence <- s2c("agtctggggggccccttttaagtagatagatagctagtcgta")
   GC(mysequence)  # 0.4761905
   GC1(mysequence) # 0.6428571
   GC2(mysequence) # 0.3571429
   GC3(mysequence) # 0.4285714
#
# With upper-case characters:
#
  myUCsequence <- s2c("GGGGGGGGGA")
  GC(myUCsequence) # 0.9
#
# With ambiguous bases:
#
  GC(s2c("acgt")) # 0.5
  GC(s2c("acgtwsws")) # 0.25
  GC(s2c("acgtwsws"), exact = TRUE) # 0.5
#
# How to reproduce the results obtained with the C program codonW
# version 1.4.4 writen by John Peden. We use here the "input.dat"
# test file from codonW (there are no ambiguous base in these
# sequences).
#
  inputdatfile <- system.file("sequences/input.dat", package = "seqinr")
  input <- read.fasta(file = inputdatfile) # read the FASTA file
  inputoutfile <- system.file("sequences/input.out", package = "seqinr")
  input.res <- read.table(inputoutfile, header = TRUE) # read codonW result file

# remove stop codon before computing G+C content (as in codonW)

  GC.codonW <- function(dnaseq, ...){
  	 GC(dnaseq[seq_len(length(dnaseq) - 3)], ...)
  }
  input.gc <- sapply(input, GC.codonW, forceToLower = FALSE)
  max(abs(input.gc - input.res$GC)) # 0.0004946237

  plot(x = input.gc, y = input.res$GC, las = 1,
  xlab = "Results with GC()", ylab = "Results from codonW",
  main = "Comparison of G+C content results")
  abline(c(0, 1), col = "red")
  legend("topleft", inset = 0.01, legend = "y = x", lty = 1, col = "red")
}
\keyword{manip}
