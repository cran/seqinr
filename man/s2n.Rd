\name{s2n}
\alias{s2n}
\title{ simple numerical encoding of a DNA sequence.
 }
\description{
By default, if no \code{levels} arguments is provided, this function will
just code your DNA sequence in integer values following the lexical
order \code{(a > c > g > t)}, that is 0 for "a", 1 for "c", 2 for "g", 3 for
"t" and NA for ambiguous bases.
}
\usage{
s2n(seq, levels = s2c("acgt"), base4 = TRUE, forceToLower = TRUE)
}
\arguments{
  \item{seq}{ the sequence as a vector of single chars }
  \item{levels}{ allowed char values, by default a, c, g and t }
  \item{base4}{if TRUE the numerical encoding will start at O, if
FALSE at 1}
  \item{forceToLower}{if TRUE the sequence is forced to lower case caracters}
}
\value{
  a vector of integers
}
\references{
  \code{citation("seqinr")}
}
\author{J.R. Lobry }
\note{
The idea of starting numbering at 0 by default is that it enforces 
a kind of isomorphism between the paste operator on DNA chars and 
the + operator on integer coding for DNA chars. By this way, you can
work either in the char set, either in the integer set, depending
on what is more convenient for your purpose, and then switch from one 
set to the other one as you like.
}
\seealso{ \code{\link{n2s}}, \code{\link{factor}}, \code{\link{unclass}} }
\examples{
##
## Example of default behaviour:
##
urndna <- s2c("acgt")
seq <- sample( urndna, 100, replace = TRUE ) ; seq
s2n(seq)
##
## How to deal with RNA:
##
urnrna <- s2c("acgt")
seq <- sample( urnrna, 100, replace = TRUE ) ; seq
s2n(seq)
##
## what happens with unknown characters:
##
urnmess <- c(urndna,"n")
seq <- sample( urnmess, 100, replace = TRUE ) ; seq
s2n(seq)
##
## How to change the encoding for unknown characters:
##
tmp <- s2n(seq) ; tmp[is.na(tmp)] <- -1; tmp
##
## Simple sanity check:
##
stopifnot(all(s2n(s2c("acgt")) == 0:3))
}
\keyword{ utilities }
