\name{write.fasta}
\alias{write.fasta}
\title{ Write file in fasta format }
\description{
  Writes sequences to a file in FASTA format.
}

\usage{write.fasta(sequences, names, nbchar = 60, file.out, open = "w")}

\arguments{
  \item{sequences}{ A DNA or protein sequence (i.e. a
    character vector) or a list of  sequences (i.e. a
    list of character vectors). }
  \item{names}{ The name(s) of the sequences. }
  \item{nbchar}{ The number of characters per line (default: 60) }
  \item{file.out}{ The name of the output file. }
  \item{open}{ Mode to open the output file, use "w" to write into
  a new file, use "a" to append at the end of an already existing file.}
}
\value{
 NULL
}
\references{
  \code{citation("seqinr")}
}
\author{ A. Necsulea }
\seealso{ \code{\link{read.fasta}} }
\examples{
## Read sequences from a FASTA file:
ortho <- read.fasta(file = system.file("sequences/ortho.fasta", package =
"seqinr"))

## Select only third codon positions:
ortho3 <- lapply(ortho, function(x) x[seq(from = 3, to = length(x), by = 3)])

## Write the modified sequences to a file:
write.fasta(sequences = ortho3, names = names(ortho3), nbchar = 80, file.out = "ortho3.fasta")

## Read it again from the same file and check that sequences are preserved:
ortho3bis <- read.fasta("ortho3.fasta", set.attributes = FALSE)
stopifnot(identical(ortho3bis, ortho3))
}
\keyword{ utilities }
