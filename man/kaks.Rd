\name{kaks}
\alias{kaks}
\title{ to Get an Estimation of Ka and Ks }
\description{
  Ks and Ka  are respectively the number of substitutions per synonymous site and per nonsynonymous site between two protein-coding genes. The ratio of nonsynonymous (Ka) to synonymous (Ks) nucleotide substitution rates is an indicator of selective pressures on genes. A ratio significantly greater than 1 indicates positive selective pressure. A ratio around 1 indicates either neutral evolution at the protein level or an averaging of sites under positive and negative selective pressures. A ratio less than 1 indicates pressures to conserve protein sequence (i.e. purifying selection). This function estimates the Ka and Ks values for a set of aligned sequences using the method published by Li (1993) and gives the associated variance matrix. 
}
\usage{
kaks(x)
}
\arguments{
  \item{x}{ An object of class \code{alignment} }
}
\value{
  \item{ ks }{ matrix of ks values }
  \item{ ka }{ matrix of ka values }
  \item{ vks }{ variance matrix of ks }
  \item{ vka }{ variance matrix of ka }	
}
\references{
Li WH. (1993) Unbiased estimation of the rates of synonymous and nonsynonymous substitution. 
\emph{J Mol Evol.}, \bold{Jan};36(1):96-9.\cr 
Hurst LD. (2002) The Ka/Ks ratio: diagnosing the form of sequence evolution.
\emph{Trends Genet.}, \bold{Sept};18(9):486.\cr  
The C programm implementing this method was provided by M. Gouy.

To have an overview of the seqinR's functionnality, please consult this vignette: 
Charif, D., Lobry, J.R. (2005) SeqinR: a contributed package to the R project for statistical
computing devoted to biological sequences retrieval and analysis. Springer Verlag, \emph{Biological and Medical Physics/Biomedical Series}, in preparation. \cr
}
\note{ 
 When the alignment does not contain enough information (i.e we approach saturation), the Ka and Ks values take the value 10.
 Negative values indicate that Ka and Ks can not be computed.
 }
\author{ D. Charif }
\seealso{\code{\link{read.alignment}} }
\examples{

 s = read.alignment(File=system.file("sequences/test.phylip", package = "seqinr"),format="phylip")
 kaks(s)	
}
\keyword{ manip }
