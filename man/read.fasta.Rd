\name{read.fasta}
\alias{read.fasta}
\title{ read FASTA formatted files }
\description{
  Read sequences from a file in FASTA format.
}
\usage{
read.fasta(file = system.file("sequences/ct.fasta", package = "seqinr"), 
  seqtype = "DNA", File = NULL, as.string = FALSE, forceDNAtolower = TRUE,
  set.attributes = TRUE)
}
\arguments{
  \item{file}{ The name of the file which the sequences in fasta format are to be 
  read from}
  \item{seqtype}{ the nature of the sequence: \code{DNA} or \code{AA} }
  \item{File}{ Synonymous of file. Maintained for upper compatibility with code
  based on seqinR <= 1.0-4 }
  \item{as.string}{ if TRUE sequences are returned as a string instead of a
  vector of single characters}
  \item{forceDNAtolower}{ whether sequences with \code{seqtype == "DNA"} should be
  returned as lower case letters }
  \item{set.attributes}{ whether sequence attributes should be set}
}
\details{
  FASTA is a widely used format in biology, some FASTA files are distributed
  with the seqinr package, see the examples section bellow.
}
\value{
  By default \code{read.fasta} return a list of vector of chars. Each element 
  is a sequence object of the class \code{SeqFastadna} or \code{SeqFastaAA}.
}
\references{
  \code{citation("seqinr")}
}
\author{ D. Charif, J.R. Lobry }
\seealso{ \code{\link{write.fasta}}, \code{\link{read.alignment}} }
\examples{
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
read.fasta(aafile, seqtype = "AA", as.string = TRUE, set.attributes = FALSE)
}
\keyword{ manip }
