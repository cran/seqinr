\name{read.fasta}
\alias{read.fasta}
\title{ Read file in fasta format }
\description{
  Read a file containing sequences in fasta format.
}
\usage{
read.fasta(File = system.file("sequences/ct.fasta", package = "seqinr"),seqtype="DNA")
}
\arguments{
  \item{File}{ The name of the file which the sequences in fasta format are to be read from. If it does not contain an absolute path, the file name is relative to the current working directory, getwd() }
  \item{seqtype}{ the nature of the sequence: \code{DNA} or \code{AA} }	
}
\details{
  By default \code{read.fasta}  
}
\value{
  return a list of vector of chars. Each element is a sequence object of the class \code{SeqFastadna} or \code{SeqFastaAA}.
}
\references{ 
To have an overview of the seqinR's functionnality, please consult this vignette: 
Charif, D., Lobry, J.R. (2005) SeqinR: a contributed package to the R project for statistical
computing devoted to biological sequences retrieval and analysis. Springer Verlag, \emph{Biological and Medical Physics/Biomedical Series}, in preparation. \cr
}
\author{ D. Charif }
\examples{
s = read.fasta(File=system.file("sequences/malM.fasta",package = "seqinr"))
ss = read.fasta(File=system.file("sequences/seqAA.fasta",package = "seqinr"),seqtype="AA")
}
\keyword{ manip }
