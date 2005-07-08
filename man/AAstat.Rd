\name{AAstat}
\alias{AAstat}
\title{ To Get Some Protein Statistics }
\description{cd
  It gives simple protein sequence information including the number of residues, the percentage of each physico-chemical class of amino acid and the theoretical isoelectric point.
}
\usage{
AAstat(seq, plot = TRUE)
}
\arguments{
  \item{seq}{ a protein sequence as a vector of chars }
  \item{plot}{ if \code{TRUE}, it plots a histogram of the presence of residues with the physico-chemical properties: Tiny, Small, Aliphatic, Aromatic, Non-polar, Polar, Charged, Positive, Negative}
}
\value{
  If plot is \code{TRUE}, it draws the histogram as described with the top. It returns a list with the 
following components: 
  \item{Compo}{A factor giving the number of each residues.}
  \item{Prop}{A list giving the percentage of each physico-chemical class of amino acid.}
  \item{Pi}{The theoretical isoelectric point}
}
\references{
  \code{citation("seqinr")}
}
\author{ D. Charif }
\seealso{ \code{\link{computePI}}, \code{\link{SEQINR.UTIL}}, \code{\link{SeqFastaAA}}}
\examples{
  ss = read.fasta(File=system.file("sequences/seqAA.fasta",package = "seqinr"),seqtype="AA")
  AAstat(ss[[1]])
}
\keyword{utilities}
