\name{AAstat}
\alias{AAstat}
\title{ To Get Some Protein Statistics }
\description{
  Returns simple protein sequence information including the number of residues, 
  the percentage physico-chemical classes and the theoretical isoelectric point.
}
\usage{
AAstat(seq, plot = TRUE)
}
\arguments{
  \item{seq}{ a protein sequence as a vector of upper-case chars }
  \item{plot}{ if \code{TRUE}, plots the presence of residues splited by 
  physico-chemical classes along the sequence. }
}
\value{
  A list with the three following components: 
  \item{Compo}{A factor giving the amino acid counts.}
  \item{Prop}{A list giving the percentage of each physico-chemical classes
  (Tiny, Small, Aliphatic, Aromatic, Non-polar, Polar, Charged, Positive, Negative).}
  \item{Pi}{The theoretical isoelectric point}
}
\references{
  \code{citation("seqinr")}
}
\author{D. Charif, J.R. Lobry}
\seealso{ \code{\link{computePI}}, \code{\link{SEQINR.UTIL}}, \code{\link{SeqFastaAA}}}
\examples{
  seqAA <- read.fasta(file = system.file("sequences/seqAA.fasta", package = "seqinr"),
   seqtype = "AA")
  AAstat(seqAA[[1]])
}
\keyword{utilities}
