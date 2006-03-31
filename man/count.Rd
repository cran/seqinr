\name{count}
\alias{count}
\title{Composition of dimer/trimer/etc oligomers}
\description{
  Counts the number of times dimer/trimer/etc oligomers occurs in a
  sequence. Note that the oligomers are overlapping.
}
\usage{
count(seq, word, frame = 0, freq = FALSE, alphabet = s2c("acgt"))
}
\arguments{
  \item{seq}{ a vector of single chars }
  \item{word}{ an integer giving the size of word (n-mer) to count }
  \item{frame}{ an integer (0, 1, 2,...) giving the frame (starting
    position) }
  \item{freq}{ if TRUE, word frequencies are computed instead of counts}
  \item{alphabet}{a vector of single characters}
}
\details{
 \code{count} counts the occurence of all words by moving a window of
 length \code{word}. The window step is always an unit. \code{frame}
 controls the starting position in the sequence for the count.
}
\value{
  This function returns a factor whose levels are all the possible
  oligomers. All oligomers are returned, even if absent from
  the sequence.		
}
\author{ D. Charif, J.R. Lobry }
\references{
  \code{citation("seqinr")}
}
\seealso{ \code{\link{table}} }
\examples{
a <- s2c("acgggtacggtcccatcgaa")
##
## To count dinucleotide occurrences in sequence a:
##
count(a, 2)
##
## To count trinucleotide occurrences in sequence a, in frame 2:
##
count(a, 3, 2)
##
## To count dinucleotide frequencies in sequence a:
##
count(a, 2, freq = TRUE)
##
## Simple sanity check:
##
alldinucl <- "aattgtctaggcgacca"
stopifnot(all(count(s2c(alldinucl), 2) == 1))
alldiaa <- "aaxxzxbxvxyxwxtxsxpxfxmxkxlxixhxgxexqxcxdxnxrxazzbzvzyzwztzszpzfzmzkzlzizhzgzezqzczdznzrzabbvbybwbtbsbpbfbmbkblbibhbgbebqbcbdbnbrbavvyvwvtvsvpvfvmvkvlvivhvgvevqvcvdvnvrvayywytysypyfymykylyiyhygyeyqycydynyryawwtwswpwfwmwkwlwiwhwgwewqwcwdwnwrwattstptftmtktltithtgtetqtctdtntrtasspsfsmskslsishsgsesqscsdsnsrsappfpmpkplpiphpgpepqpcpdpnprpaffmfkflfifhfgfefqfcfdfnfrfammkmlmimhmgmemqmcmdmnmrmakklkikhkgkekqkckdknkrkallilhlglelqlcldlnlrlaiihigieiqicidiniriahhghehqhchdhnhrhaggegqgcgdgngrgaeeqecedenereaqqcqdqnqrqaccdcncrcaddndrdannrnarra"
stopifnot(all(count(s2c(alldiaa), 2, alphabet = s2c("arndcqeghilkmfpstwyvbzx")) == 1))
}
\keyword{ manip }
