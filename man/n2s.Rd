\name{n2s}
\alias{n2s}
\title{ function to convert the numeric encoding of a DNA sequence
into a vector of characters }
\description{
By default, if no `levels' arguments is provided, this function
will just transform your vector of integer into a DNA sequence 
according to the lexical order: \code{0 -> "a"}, \code{1 -> "c"}, \code{ 2 -> "g"},
\code{3 -> "t"}, \code{others -> NA}.
}
\usage{
n2s(nseq, levels = c("a", "c", "g", "t"), base4 = TRUE)
}
\arguments{
  \item{nseq}{A vector of integers }
  \item{levels}{the translation vector }
  \item{base4}{when this logical is true, the numerical encoding of
\code{levels} starts at 0, when it is false the numerical encoding of 
\code{levels} starts at 1.}
}
\value{
a vector of characters
}
\references{
  \code{citation("seqinr")}
}
\author{J.R. Lobry}
\seealso{ \code{\link{s2n}} }
\examples{
##example of the default behaviour:
nseq <- sample(x = 0:3, size = 100, replace = TRUE)
n2s(nseq) 
# Show what happens with out-of-range and NA values:
nseq[1] <- NA
nseq[2] <- 777
n2s(nseq)[1:10]
# How to get an RNA instead:
n2s(nseq, levels = c("a", "c", "g", "u"))
}
\keyword{ utilities }
