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
s2n(seq, levels, base4 = TRUE)
}
\arguments{
  \item{seq}{ a vector of chars }
  \item{levels}{ allowed char values, by default a, c, g and t }
  \item{base4}{if TRUE the numerical encoding will start at O, if
FALSE at 1}
  \item{\dots}{ further arguments to factor }
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
#example of default behaviour
urndna <- c("a","c","g","t")
seq <- sample( urndna, 100, replace = TRUE ) ; seq
s2n(seq)
#How to deal with RNA
urnrna <- c("a","c","g","t")
seq <- sample( urnrna, 100, replace = TRUE ) ; seq
s2n(seq)
#what's happen with unknown characters
urnmess <- c(urndna,"n")
seq <- sample( urnmess, 100, replace = TRUE ) ; seq
s2n(seq)
#How to change the encoding for unknown characters
tmp <- s2n(seq) ; tmp[is.na(tmp)] <- -1; tmp
}
\keyword{ utilities }
