\name{splitseq}
\alias{splitseq}
\title{ split a sequence into sub-sequences }
\description{
 Split a sequence into sub-sequences of 3 (the default size) with no overlap between the sub-sequences. 
}
\usage{
splitseq(seq, frame = 0, word = 3)
}
\arguments{
  \item{seq}{ a vector of chars }
  \item{frame}{ an integer giving the starting position to split the sequence }
  \item{word}{ an integer giving the size of the sub-sequences }
}
\value{
  This function returns a vector which contains the sub-sequences.
}
\references{ 
To have an overview of the seqinR's functionnality, please consult this vignette: 
Charif, D., Lobry, J.R. (2005) SeqinR: a contributed package to the R project for statistical
computing devoted to biological sequences retrieval and analysis. Springer Verlag, \emph{Biological and Medical Physics/Biomedical Series}, in preparation. \cr
}
\author{ D. Charif }
\seealso{ \code{\link{split}} }
\examples{
a=s2c("aacgttgcaggtcgctcgctacgtagctactgttt")
## To obtain the codon sequence in frame 0:
splitseq(a)
}
\keyword{ manip }
