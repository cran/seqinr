\name{count}
\alias{count}
\title{Composition of dimer/trimer/etc nucleotides}
\description{
  Counts the number of times dimer/trimer/etc nucleotides occurs in a
  sequence. Note that the oligomers are overlapping.
}
\usage{
count(seq,word,frame=0)
}
\arguments{
  \item{seq}{ a vector of chars }
  \item{word}{ an integer giving the size of word (n-mer) to count }
  \item{frame}{	an integer (0,1,2) giving the frame (starting position) }
}
\details{
 \code{count} counts the occurence of all words by moving a window of
 length \code{word}. The window step is always an unit. \code{frame}
 controls the starting position in the sequence for the count.
}
\value{
  This function returns a factor whose levels are all the possible
  oligonucleotids. All oligomers are returned, even if absent from the sequence.		
}
\author{ D. Charif }
\references{ 
To have an overview of the seqinR's functionnality, please consult this vignette: 
Charif, D., Lobry, J.R. (2005) SeqinR: a contributed package to the R project for statistical
computing devoted to biological sequences retrieval and analysis. Springer Verlag, \emph{Biological and Medical Physics/Biomedical Series}, in preparation. \cr
}
\seealso{ \code{\link{table}} }
\examples{
a=s2c("acgggtacggtcccatcgaa")
##To count the occurences of dinucleotides of a:
count(a,2)
##To count the occurences of trinucleotides of a in frame 2:
count(a,3,2)
}
\keyword{ manip }
