\name{kaks}
\alias{kaks}
\title{ to Get an Estimation of Ka and Ks }
\description{
  Ks and Ka  are respectively the number of substitutions per synonymous site and per nonsynonymous site between two protein-coding genes. The ratio of nonsynonymous (Ka) to synonymous (Ks) nucleotide substitution rates is an indicator of selective pressures on genes. A ratio significantly greater than 1 indicates positive selective pressure. A ratio around 1 indicates either neutral evolution at the protein level or an averaging of sites under positive and negative selective pressures. A ratio less than 1 indicates pressures to conserve protein sequence (i.e. purifying selection). This function estimates the Ka and Ks values for a set of aligned sequences using the method published by Li (1993) and gives the associated variance matrix. 
}
\usage{
kaks(x, debug = FALSE)
}
\arguments{
  \item{x}{ An object of class \code{alignment} }
  \item{debug}{ If TRUE turns debug mode on}
}
\value{
  \item{ ks }{ matrix of Ks values }
  \item{ ka }{ matrix of Ka values }
  \item{ vks }{ variance matrix of Ks }
  \item{ vka }{ variance matrix of Ka }	
}
\references{
Li WH. (1993) Unbiased estimation of the rates of synonymous and nonsynonymous substitution. 
\emph{J Mol Evol.}, \bold{Jan};36(1):96-9.\cr 
Hurst LD. (2002) The Ka/Ks ratio: diagnosing the form of sequence evolution.
\emph{Trends Genet.}, \bold{Sept};18(9):486.\cr  
The C programm implementing this method was provided by Manolo Gouy. More info is
needed here to trace back the original C source so as to credit correct source.\cr
\code{citation("seqinr")}
}
\note{ 
 When the alignment does not contain enough information (i.e we approach saturation), the Ka and Ks values take the value 10.
 Negative values indicate that Ka and Ks can not be computed.\cr
 Codons with ambiguous bases are treated as gaps.\cr
 Codons with gaps are not used for computations.
 }
\author{ D. Charif, J.R. Lobry }
\seealso{\code{\link{read.alignment}} }
\examples{
 #
 # Simple Toy example:
 #
 s <- read.alignment(File = system.file("sequences/test.phylip", package = "seqinr"), format = "phylip")
 kaks(s)
 #
 # Check numeric results on an simple test example:
 #
 data(AnoukResult)
 Anouk <- read.alignment(File = system.file("sequences/Anouk.fasta", package = "seqinr"), format = "fasta")	
 if( ! all.equal(kaks(Anouk), AnoukResult) ) {
   warning("Poor numeric results with Anouk test file")
 } else {
   print("Results are OK with Anouk test file")
 }
}
\keyword{ manip }
