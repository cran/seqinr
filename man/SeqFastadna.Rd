\name{SeqFastadna}
\alias{SeqFastadna}
\alias{is.SeqFastadna}
\alias{as.SeqFastadna}
\alias{summary.SeqFastadna}
\title{ Class for DNA sequence in Fasta Format}
\description{
  \code{as.SeqFastadna} is called by many functions as \code{read.fasta}. It creates an object of class \code{SeqFastadna}. 
  \code{is.SeqFastadna} returns TRUE if the object is of class \code{SeqFastadna}. 
  \code{summary.SeqFastadna} gives the base composition of an object of class \code{SeqFastadna}.
}
\usage{
as.SeqFastadna(object, name = NULL, Annot = NULL)
is.SeqFastadna(object)
summary.SeqFastadna(object,...)
}
\arguments{
  \item{object}{a vector of chars representing a biological sequence} 
  \item{name}{ \code{NULL} a character string specifying a name for the sequence } 
  \item{Annot}{ \code{NULL} a character string specifying some annotations for the sequence }
  \item{...}{additional arguments affecting the summary produced}
}
\value{
  \code{as.SeqFastadna} returns an object sequence of class \code{SeqFastadna}.
  \code{summary.SeqFastadna} returns a list which the following components:
	\item{length}{ the legth of the sequence}
	\item{compo}{the base counting of the sequence} 
	\item{GC}{the percentage of G+C in the sequence}
}
\references{ 
To have an overview of the seqinR's functionnality, please consult this vignette: 
Charif, D., Lobry, J.R. (2005) SeqinR: a contributed package to the R project for statistical
computing devoted to biological sequences retrieval and analysis. Springer Verlag, \emph{Biological and Medical Physics/Biomedical Series}, in preparation. \cr
}
\author{D. Charif}
\examples{
 s = read.fasta(system.file("sequences/malM.fasta",package="seqinr"))
 is.SeqFastadna(s[[1]])
 summary(s[[1]])
 myseq = s2c("acgttgatgctagctagcatcgat")
 as.SeqFastadna(myseq, name = "myseq", Annot = "blablabla")
 myseq
}
\keyword{utilities}
