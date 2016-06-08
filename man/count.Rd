\name{count}
\alias{count}
\title{Composition of dimer/trimer/etc oligomers}
\description{
  Counts the number of times dimer/trimer/etc oligomers occur in a
  sequence. Note that the oligomers are overlapping by default.
}
\usage{
count(seq, wordsize, start = 0, by = 1,
 freq = FALSE, alphabet = s2c("acgt"), frame = start)
}
\arguments{
  \item{seq}{a vector of single characters.}
  \item{wordsize}{an integer giving the size of word (n-mer) to count.}
  \item{start}{an integer (0, 1, 2,...) giving the starting
    position to consider in the sequence. The default value 0 means that
    we start at the first nucleotide in the sequence.}
  \item{by}{an integer defaulting to 1 for the window step.}
  \item{freq}{if TRUE, word relative frequencies (summing to 1) are returned instead of counts}
  \item{alphabet}{a vector of single characters used to build the oligomer set.}
  \item{frame}{synonymous for start}
}
\details{
 \code{count} counts the occurence of all words by moving a window of
 length \code{word}. The window step is controlled by the argument \code{by}. 
\code{start} controls the starting position in the sequence for the count.
}
\value{
  This function returns a \code{\link{table}} whose \code{\link{dimnames}} are all the possible
  oligomers. All oligomers are returned, even if absent from
  the sequence.		
}
\author{D. Charif, J.R. Lobry with suggestions from Gabriel Valiente, Stefanie Hartmann and Christian Gautier}
\references{
  \code{citation("seqinr")}
}
\seealso{ \code{\link{table}} for the class of the returned objet. See \code{\link{rho}} and
  \code{\link{zscore}} for dinucleotide statistics.}
\examples{
a <- s2c("acgggtacggtcccatcgaa")
##
## To count dinucleotide occurrences in sequence a:
##
count(a, word = 2)
##
## To count trinucleotide occurrences in sequence a, with start = 2:
##
count(a, word = 3, start = 2)
##
## To count dinucleotide relative frequencies in sequence a:
##
count(a, word = 2, freq = TRUE)
##
## To count dinucleotides in codon positions III-I in a coding sequence:
##
alldinuclIIIpI <- s2c("NNaaNatNttNtgNgtNtcNctNtaNagNggNgcNcgNgaNacNccNcaNN")
resIIIpI <- count(alldinuclIIIpI, word = 2, start = 2, by = 3)
stopifnot(all( resIIIpI == 1))
##
## Simple sanity check:
##
#alldinucl <- "aattgtctaggcgacca"
#stopifnot(all(count(s2c(alldinucl), 2) == 1))
#alldiaa <- "aaxxzxbxvxyxwxtxsxpxfxmxkxlxixhxgxexqxcxdxnxrxazzbzvzyzwztzszpzfzmzkzlzizhzgzezqzczdznz
#rzabbvbybwbtbsbpbfbmbkblbibhbgbebqbcbdbnbrbavvyvwvtvsvpvfvmvkvlvivhvgvevqvcvdvnvrvayywytysypyfymyky
#lyiyhygyeyqycydynyryawwtwswpwfwmwkwlwiwhwgwewqwcwdwnwrwattstptftmtktltithtgtetqtctdtntrtasspsfsmsks
#lsishsgsesqscsdsnsrsappfpmpkplpiphpgpepqpcpdpnprpaffmfkflfifhfgfefqfcfdfnfrfammkmlmimhmgmemqmcmdmnm
#rmakklkikhkgkekqkckdknkrkallilhlglelqlcldlnlrlaiihigieiqicidiniriahhghehqhchdhnhrhaggegqgcgdgngrgae
#eqecedenereaqqcqdqnqrqaccdcncrcaddndrdannrnarra"
#stopifnot(all(count(s2c(alldiaa), 2, alphabet = s2c("arndcqeghilkmfpstwyvbzx")) == 1))
##
## Example with dinucleotide count in the complete Human mitochondrion genome:
##
humanMito <- read.fasta(file = system.file("sequences/humanMito.fasta", package = "seqinr"))
##
## Get the dinucleotide count:
##
dinu <- count(humanMito[[1]], 2)
##
## Put the results in a 4 X 4 array:
##
dinu2 <- dinu
dim(dinu2) <- c(4, 4)
nucl <- s2c("ACGT")
dimnames(dinu2) <- list(paste(nucl, "-3\'", sep = ""), paste("5\'-", nucl, sep = ""))
##
## Show that CpG and GpT dinucleotides are depleted:
##
mosaicplot(t(dinu2), shade = TRUE,
  main = "Dinucleotide XpY frequencies in the Human\nmitochondrion complete genome", 
  xlab = "First nucleotide: Xp", 
  ylab = "Second nucleotide: pY", las = 1, cex = 1)
mtext("Note the depletion in CpG and GpT dinucleotides", side = 1, line = 3)
}
\keyword{ manip }
