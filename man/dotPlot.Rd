\name{dotPlot}
\alias{dotPlot}
\title{ Dot Plot Comparison of two sequences }
\description{
  Dot plots are most likely the oldest visual representation used to compare 
  two sequences (see Maizel and Lenk 1981 and references therein). In its
  simplest form, a dot is produced at position (i,j) iff character number
  i in the first sequence is the same as character number j in the
  second sequence. More eleborated forms use sliding windows and a threshold
  value for two windows to be considered as matched.
}
\usage{
dotPlot(seq1, seq2, wsize = 1, wstep = 1, nmatch = 1, col = c("white", "black"), 
xlab = deparse(substitute(seq1)), ylab = deparse(substitute(seq2)), ...)
}
\arguments{
  \item{seq1}{ the first sequence (x-axis) as a vector of single chars.}
  \item{seq2}{ the second sequence (y-axis) as a vector of single char.}
  \item{wsize}{ the size in chars of the moving window.}
  \item{wstep}{ the size in chars for the steps of the moving window.
    Use \code{wstep == wsize} for non-overlapping windows.}
  \item{nmatch}{ if the number of match per window is greater than or equal
    to \code{nmatch} then a dot is produced.}
  \item{col}{ color of points passed to \code{image}.}
  \item{xlab}{ label of x-axis passed to \code{image}.}
  \item{ylab}{ label of y-axis passed to \code{image}.}
  \item{...}{ further arguments passed to \code{image}.}
}

\value{
  NULL. 
}
\references{
Maizel, J.V. and Lenk, R.P. (1981) Enhanced Graphic Matrix Analysis of 
Nucleic Acid and Protein Sequences. 
\emph{Proceedings of the National Academy of Science USA},
\bold{78}:7665-7669.\cr

\code{citation("seqinr")}
}
\author{J.R. Lobry}
\seealso{ \code{\link{image}} }
\examples{
#
# Identity is on the main diagonal:
#
dotPlot(letters, letters, main = "Direct repeat")
#
# Internal repeats are off the main diagonal:
#
dotPlot(rep(letters, 2), rep(letters, 2), main = "Internal repeats")
#
# Inversions are orthogonal to the main diagonal:
#
dotPlot(letters, rev(letters), main = "Inversion")
#
# Insertion in the second sequence yields a vertical jump:
#
dotPlot(letters, c(letters[1:10], s2c("insertion"), letters[11:26]), 
  main = "Insertion in the second sequence", asp = 1)
#
# Insertion in the first sequence yields an horizontal jump:
#
dotPlot(c(letters[1:10], s2c("insertion"), letters[11:26]), letters,
  main = "Insertion in the first sequence", asp = 1)
#
# Protein sequences have usually a good signal/noise ratio because there
# are 20 possible amino-acids:
#
aafile <- system.file("sequences/seqAA.fasta", package = "seqinr")
protein <- read.fasta(aafile)[[1]]
dotPlot(protein, protein, main = "Dot plot of a protein\nwsize = 1, wstep = 1, nmatch = 1")
#
# Nucleic acid sequences have usually a poor signal/noise ratio because
# there are only 4 different bases:
#
dnafile <- system.file("sequences/malM.fasta", package = "seqinr")
dna <- protein <- read.fasta(dnafile)[[1]]
dotPlot(dna[1:200], dna[1:200],
 main = "Dot plot of a nucleic acid sequence\nwsize = 1, wstep = 1, nmatch = 1")
#
# Play with the wsize, wstep and nmatch arguments to increase the 
# signal/noise ratio:
#
dotPlot(dna[1:200], dna[1:200], wsize = 3, wstep = 3, nmatch = 3,
main = "Dot plot of a nucleic acid sequence\nwsize = 3, wstep = 3, nmatch = 3")
}
\keyword{ utilities }
