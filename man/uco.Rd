\name{uco}
\alias{uco}
\title{ Create codon usage table }
\description{
  \code{uco} calculates some codon usage index: the codon count \code{eff}, the absolute frequencies \code{freq} or the Relative Synonymous Codon Usage \code{rscu}.
}
\usage{
uco( seq, frame = 0, index = c("eff","freq","rscu"), as.data.frame = FALSE) 
}
\arguments{
  \item{seq}{ a vector of chars }
  \item{frame}{ an integer (0,1,2) giving the frame }
  \item{index}{ One of this value }
  tabular{lll}{
	eff  \tab To get the codon count\cr
	freq \tab To get the absolute frequence of codon\cr
	rscu  \tab To get the RSCU indice
	}
  \item{as.data.frame}{ logical. If \code{TRUE}: The eff, freq and rscu index will be returned in a data frame.}
}
\value{
    If \code{as.data.frame} is TRUE \code{uco} returns a data frame with five columns: 
  \item{ aa }{ a vector containing the name of amino-acid }
  \item{ codon }{ a vector containing the corresponding codon }
  \item{ eff }{ a numeric vector containing effectifs of codons }
  \item{ freq }{  a numeric vector containing frequencies of codons }
  \item{ rscu }{ a numeric vector containing the relative synonymous codon usage}
  If \code{as.data.frame} is FALSE, \code{uco} returns one of this index: 
  \item{ eff }{ a factor containing effectifs of codons }
  \item{ freq }{  a factor containing frequencies of codons }	
  \item{ rscu }{ a factor containing the relative synonymous codon usage}
}
\references{ 
To have an overview of the seqinR's functionnality, please consult this vignette: 
Charif, D., Lobry, J.R. (2005) SeqinR: a contributed package to the R project for statistical
computing devoted to biological sequences retrieval and analysis. Springer Verlag, \emph{Biological and Medical Physics/Biomedical Series}, in preparation. \cr
}
\author{ D. Charif }
\examples{
s = read.fasta(File=system.file("sequences/malM.fasta",package = "seqinr"))
uco( s[[1]], index="freq")
uco( s[[1]], index="eff")
uco( s[[1]], index="rscu")
uco( s[[1]], as.data.frame = TRUE)
}
\keyword{ manip }
