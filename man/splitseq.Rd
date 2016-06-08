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
  \item{frame}{ an integer (0, 1, 2) giving the starting position to split the sequence }
  \item{word}{ an integer giving the size of the sub-sequences }
}
\value{
  This function returns a vector which contains the sub-sequences.
}
\references{
\code{citation("seqinr")}
\cr
}
\author{J.R. Lobry}
\seealso{ \code{\link{split}} }
\examples{
cds <- s2c("aacgttgcaggtcgctcgctacgtagctactgttt")
#
# To obtain the codon sequence in frame 0:
#
stopifnot(identical(splitseq(cds), 
  c("aac", "gtt", "gca", "ggt", "cgc", "tcg", "cta", "cgt", "agc", "tac", "tgt")))
#
# Show the effect of frame and word with a ten char sequence:
#
(tenchar <- s2c("1234567890"))
splitseq(tenchar, frame = 0)
splitseq(tenchar, frame = 1)
splitseq(tenchar, frame = 2)
splitseq(tenchar, frame = 0, word = 2)
splitseq(tenchar, frame = 0, word = 1)
}
\keyword{ manip }
