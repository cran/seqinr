\name{SeqFastaAA}
\alias{SeqFastaAA}
\alias{is.SeqFastaAA}
\alias{as.SeqFastaAA}
\alias{summary.SeqFastaAA}
\title{AA sequence in Fasta Format}
\description{
  \code{as.SeqFastaAA} is called by the function as \code{read.fasta}. It creates an object of class \code{SeqFastaAA}. 
  \code{is.SeqFastaAA} returns TRUE if the object is of class \code{SeqFastaAA}. 
  \code{summary.SeqFastaAA} gives the AA composition of an object of class \code{SeqFastaAA}.
}
\usage{
as.SeqFastaAA(object, name = NULL, Annot = NULL)
is.SeqFastaAA(object)
summary.SeqFastaAA(object,...)
}
\arguments{
  \item{object}{ a vector of chars representing a biological sequence }
  \item{name}{ \code{NULL} a character string specifying a name for the sequence } 
  \item{Annot}{ \code{NULL} a character string specifying some annotations for the sequence }	
  \item{...}{ additional arguments affecting the summary produced }
}
\value{
  \code{as.SeqFastaAA} returns an object sequence of class \code{SeqFastaAA}.
  \code{summary.SeqFastaAA} returns a list which the following components:
	\item{composition}{ the AA counting of the sequence} 
	\item{AA.Property}{ the percentage of each group of amino acid in the sequence. By example, the groups are small, tiny, aliphatic, aromatic ... }
}
\references{ 
To have an overview of the seqinR's functionnality, please consult this vignette: 
Charif, D., Lobry, J.R. (2005) SeqinR: a contributed package to the R project for statistical
computing devoted to biological sequences retrieval and analysis. Springer Verlag, \emph{Biological and Medical Physics/Biomedical Series}, in preparation. \cr
}
\author{ D. Charif }
\examples{
  s = read.fasta(File=system.file("sequences/seqAA.fasta",package="seqinr"),seqtype="AA")
  is.SeqFastaAA(s[[1]])
  summary(s[[1]])
  myseq = s2c("MSPTAYRRGSPAFLV*")
  as.SeqFastaAA(myseq, name = "myseq", Annot = "blablabla")
  myseq
}
\keyword{ utilities }
