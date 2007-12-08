\name{read.fasta}
\alias{read.fasta}
\alias{readfasta}
\alias{FASTA}
\title{ read FASTA formatted files }
\description{
  Read nucleic or amino-acid sequences from a file in FASTA format.
}
\usage{
read.fasta(file = system.file("sequences/ct.fasta", package = "seqinr"), 
  seqtype = "DNA", File = NULL, as.string = FALSE, forceDNAtolower = TRUE,
  set.attributes = TRUE, legacy.mode = TRUE, seqonly = FALSE, strip.desc = FALSE)
}
\arguments{
  \item{file}{ The name of the file which the sequences in fasta format are to be 
  read from}
  \item{seqtype}{ the nature of the sequence: \code{DNA} or \code{AA} }
  \item{File}{ Synonymous of file. As from seqinR >= 1.1-3 this argument
  is deprecated and a warning is issued}
  \item{as.string}{ if TRUE sequences are returned as a string instead of a
  vector of single characters}
  \item{forceDNAtolower}{ whether sequences with \code{seqtype == "DNA"} should be
  returned as lower case letters }
  \item{set.attributes}{ whether sequence attributes should be set}
  \item{legacy.mode}{if TRUE lines starting with a semicolon ';' are ignored}
  \item{seqonly}{if TRUE, only sequences as returned without attempt to modify
    them or to get their names and annotations (execution time is divided approximately
   by a factor 3)}
  \item{strip.desc}{if TRUE the '>' at the beginning of the description lines is removed
  in the annotations of the sequences}
}
\details{
  FASTA is a widely used format in biology, some FASTA files are distributed
  with the seqinr package, see the examples section below.
  Sequence in FASTA format begins with a single-line 
  description (distinguished by a greater-than '>' symbol), followed 
  by sequence data on the next lines. Lines starting by a semicolon ';'
  are ignored, as in the original FASTA program (Pearson and Lipman 1988).
  The sequence name is just after the '>' up to the next space ' ' character,
  trailling infos are ignored for the name but saved in the annotations.
}
\value{
  By default \code{read.fasta} return a list of vector of chars. Each element 
  is a sequence object of the class \code{SeqFastadna} or \code{SeqFastaAA}.
}
\references{

  Pearson, W.R. and Lipman, D.J. (1988) Improved tools for biological 
  sequence comparison. \emph{Proceedings of the National Academy 
  of Sciences of the United States of America}, \bold{85}:2444-2448

  \code{citation("seqinr")}
}
\author{ D. Charif, J.R. Lobry }
\seealso{
  \code{\link{write.fasta}} to write sequences in a FASTA file, 
  \code{\link{gb2fasta}} to convert a GenBank file into a FASTA file,
  \code{\link{read.alignment}} to read aligned sequences,
  \code{\link{reverse.align}} to get an alignment at the nucleic level from the
  one at the amino-acid level }
\examples{
#
# Simple sanity check with a small FASTA file:
#
  smallFastaFile <- system.file("sequences/smallAA.fasta", package = "seqinr")
  mySmallProtein <- read.fasta(file = smallFastaFile, as.string = TRUE, seqtype = "AA")[[1]]
  stopifnot(mySmallProtein == "SEQINRSEQINRSEQINRSEQINR*")
#
# Example of a DNA file in FASTA format:
#
  dnafile <- system.file("sequences/malM.fasta", package = "seqinr")
#
# Read with defaults arguments, looks like:
#
# $XYLEECOM.MALM
# [1] "a" "t" "g" "a" "a" "a" "a" "t" "g" "a" "a" "t" "a" "a" "a" "a" "g" "t"
# ...
  read.fasta(file = dnafile)
#
# The same but do not turn the sequence into a vector of single characters, looks like:
#
# $XYLEECOM.MALM
# [1] "atgaaaatgaataaaagtctcatcgtcctctgtttatcagcagggttactggcaagcgc 
# ...
  read.fasta(file = dnafile, as.string = TRUE)
#
# The same but do not force lower case letters, looks like:
#
# $XYLEECOM.MALM
# [1] "ATGAAAATGAATAAAAGTCTCATCGTCCTCTGTTTATCAGCAGGGTTACTGGCAAGC
# ...
  read.fasta(file = dnafile, as.string = TRUE, forceDNAtolower = FALSE)
#
# Example of a protein file in FASTA format:
#
  aafile <- system.file("sequences/seqAA.fasta", package = "seqinr")
#
# Read the protein sequence file, looks like:
#
# $A06852
# [1] "M" "P" "R" "L" "F" "S" "Y" "L" "L" "G" "V" "W" "L" "L" "L" "S" "Q" "L"
# ...
  read.fasta(aafile, seqtype = "AA")
#
# The same, but as string and without attributes, looks like:
#
# $A06852
# [1] "MPRLFSYLLGVWLLLSQLPREIPGQSTNDFIKACGRELVRLWVEICGSVSWGRTALSLEEP
# QLETGPPAETMPSSITKDAEILKMMLEFVPNLPQELKATLSERQPSLRELQQSASKDSNLNFEEFK
# KIILNRQNEAEDKSLLELKNLGLDKHSRKKRLFRMTLSEKCCQVGCIRKDIARLC*"
#
  read.fasta(aafile, seqtype = "AA", as.string = TRUE, set.attributes = FALSE)
#
# Example with a FASTA file that contains comment lines starting with
# a semicolon character ';'
#
  legacyfile <- system.file("sequences/legacy.fasta", package = "seqinr")
  legacyseq <- read.fasta(file = legacyfile, as.string = TRUE)
  stopifnot( nchar(legacyseq) == 921 )
}
\keyword{ manip }
