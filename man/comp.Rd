\name{comp}
\alias{comp}
\title{ complements a nucleic acid sequence }
\description{
    Complements a sequence, for instance if the sequence is 
    \code{"a","c","g","t"} it returns \code{"t","g","c","a"}. 
    All other values will return NA. This is not the reverse
    complementary strand.
}
\usage{
comp(seq)
}
\arguments{
  \item{seq}{ a vector of chars }
}
\value{
  a vector of characters which is the complement of the sequence.
}
\author{ D. Charif, J.R. Lobry }
\references{
  \code{citation("seqinr")}
}
\seealso{ Because ssDNA sequences are always written in the 5'->3'
direction, use rev(comp(seq)) to get the reverse complementary
strand (see \code{\link{rev}}).
}
\examples{
##
## Show that comp() does not return the reverve complementary strand:
##
c2s(comp(s2c("aaaattttggggcccc")))
##
## Show how to get the reverse complementary strand:
##
c2s(rev(comp(s2c("aaaattttggggcccc"))))
##
## Show what's happen with non allowed values:
##
c2s(rev(comp(s2c("aaaaXttttYggggZcccc"))))
}
\keyword{ manip }
