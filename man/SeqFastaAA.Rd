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
\method{summary}{SeqFastaAA}(object,...)
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
  \code{citation("seqinr")}
}
\author{D. Charif}
\examples{
  s <- read.fasta(file = system.file("sequences/seqAA.fasta", package = "seqinr"), seqtype="AA")
  is.SeqFastaAA(s[[1]])
  summary(s[[1]])
  myseq <- s2c("MSPTAYRRGSPAFLV*")
  as.SeqFastaAA(myseq, name = "myseq", Annot = "blablabla")
  myseq
}
\keyword{ utilities }
