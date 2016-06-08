\name{comp}
\alias{comp}
\title{ complements a nucleic acid sequence }
\description{
    Complements a sequence, for instance if the sequence is 
    \code{"a","c","g","t"} it returns \code{"t","g","c","a"}. 
    This is not the reverse complementary strand. This function
    can handle ambiguous bases if required.
}
\usage{
comp(seq, forceToLower = TRUE, ambiguous = FALSE)
}
\arguments{
  \item{seq}{ a DNA sequence as a vector of single chars }
  \item{forceToLower}{ if TRUE characters in \code{seq} are forced to lower case}
  \item{ambiguous}{ if TRUE ambiguous bases in \code{seq} are handled} 
}
\value{
  a vector of characters which is the complement of the sequence,
  not the reverse complementary strand. Undefined values are
  returned as NA.
}
\author{D. Charif, J.R. Lobry}
\references{
  \code{citation("seqinr")}
}
\seealso{ Because ssDNA sequences are always written in the 5'->3'
direction, use rev(comp(seq)) to get the reverse complementary
strand (see \code{\link{rev}}).
}
\examples{
##
## Show that comp() does *not* return the reverve complementary strand:
##

c2s(comp(s2c("aaaattttggggcccc")))

##
## Show how to get the reverse complementary strand:
##

c2s(rev(comp(s2c("aaaattttggggcccc"))))

##
## Show what happens with non allowed values:
##

c2s(rev(comp(s2c("aaaaXttttYggggZcccc"))))

##
## Show what happens with ambiguous bases:
##

allbases <- s2c("abcdghkmstvwn")
comp(allbases) # NA are produced
comp(allbases, ambiguous = TRUE) # No more NA

##
## Routine sanity checks:
##

stopifnot(identical(comp(allbases, ambiguous = TRUE), s2c("tvghcdmksabwn")))
stopifnot(identical(comp(c("A", "C", "G", "T"), forceToLower = FALSE), c("T", "G", "C", "A")))
}
