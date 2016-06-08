\name{as.alignment}
\alias{as.alignment}
\title{Constructor for class alignment}
\description{
Returns an object of (S3) class alignment. 
}
\usage{
as.alignment(nb = NULL, nam = NULL, seq = NULL, com = NULL)
}
\arguments{
  \item{nb}{integer. The number of sequences in the alignment.}
  \item{nam}{vector of \code{nb} character strings. The sequence names. }
  \item{seq}{vector of \code{nb} character strings. The aligned sequences.}
  \item{com}{vector of \code{nb} character strings. The comments about sequences.}
}

\value{
 An object of class \code{alignment} which is a list with the following components:
  \item{nb}{ the number of aligned sequences }
  \item{nam}{ a vector of strings containing the names of the aligned sequences } 
  \item{seq}{ a vector of strings containing the aligned sequences} 
  \item{com}{ a vector of strings containing the commentaries for each sequence or \code{NA} if there are no comments }
}
\references{ 
\code{citation("seqinr")}
}
\author{D. Charif, J.R. Lobry}
\seealso{
\code{\link{read.alignment}},
\code{\link{as.matrix.alignment}}, \code{\link{read.fasta}}, 
\code{\link{write.fasta}}, \code{\link{reverse.align}}, \code{\link{dist.alignment}}.

 }
\examples{
as.alignment(nb = 2, nam = c("one", "two"), 
  seq = c("-ACGT", "GACG-"), com = c("un", "deux"))
}

