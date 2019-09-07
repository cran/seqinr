\name{gb2fasta}
\alias{gb2fasta}
\title{Conversion of GenBank file into fasta file}
\description{
Converts a single entry in GenBank format into a fasta file.
}
\usage{
gb2fasta(source.file, destination.file)
}
\arguments{
  \item{source.file}{ GenBank file }
  \item{destination.file}{ Fasta file }
}
\details{
Multiple entries in GenBank file are not supported.
}
\value{
  none
}
\references{
  \code{citation("seqinr")}
}
\author{J.R. Lobry}
\seealso{ \code{\link{oriloc}} }
\examples{
  myGenBankFile <- system.file("sequences/ct.gbk.gz", package = "seqinr")
  #myFastaFileName <- "Acinetobacter_ADP1_uid61597.fasta"
  myFastaFileName <-tempfile(pattern = "Acinetobacter_ADP1_uid61597", 
   tmpdir = tempdir(), fileext = "fasta")
  tempdir(check = FALSE)
  gb2fasta(myGenBankFile, myFastaFileName)
  readLines(myFastaFileName)[1:5]
  #
  # Should be :
  #
  # [1] ">CHLTCG 1042519 bp"                                          
  # [2] "gcggccgcccgggaaattgctaaaagatgggagcaaagagttagagatctacaagataaa"
  # [3] "ggtgctgcacgaaaattattaaatgatcctttaggccgacgaacacctaattatcagagc"
  # [4] "aaaaatccaggtgagtatactgtagggaattccatgttttacgatggtcctcaggtagcg"
  # [5] "aatctccagaacgtcgacactggtttttggctggacatgagcaatctctcagacgttgta"
  #
}
\keyword{utilities}
