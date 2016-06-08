\name{words}
\alias{words}
\title{ To get all words from an alphabet.}
\description{
Generates a vectors of all the words from a given alphabet,
with right positions varying faster, for instance if the
\code{alphabet} is \code{(c("0","1")} and the \code{length}
is 2 you will obtain \code{c("00", "01", "10", "11")}
}
\usage{
words(length = 3, alphabet = s2c("acgt"))
}
\arguments{
  \item{length}{ the number of characters in the words }
  \item{alphabet}{ a vector of characters }
}
\value{
A vector of string whith \code{length} characters. 
}
\author{J.R. Lobry}
\references{
  \code{citation("seqinr")}
}
\seealso{ \code{\link{kronecker}}, \code{\link{outer}} }
\examples{
#
# Get all 64 codons:
#
stopifnot(all(words() ==
c("aaa", "aac", "aag", "aat", "aca", "acc", "acg", "act", "aga", "agc", "agg", 
   "agt", "ata", "atc", "atg", "att","caa", "cac", "cag", "cat", "cca", "ccc", 
   "ccg", "cct", "cga", "cgc", "cgg", "cgt", "cta", "ctc", "ctg", "ctt", "gaa",
   "gac", "gag", "gat", "gca", "gcc", "gcg", "gct", "gga", "ggc", "ggg", "ggt", 
   "gta", "gtc", "gtg", "gtt", "taa", "tac", "tag", "tat", "tca", "tcc", "tcg", 
   "tct", "tga", "tgc", "tgg", "tgt", "tta", "ttc", "ttg", "ttt")))
#
# Get all codons with u c a g for bases:
#
words(alphabet = s2c("ucag"))
#
# Get all tetranucleotides:
#
words(length = 4)
#
# Get all dipeptides:
#
words(length = 2, alphabet = a()[-1])
}
\keyword{ utilities }
