\name{write.fasta}
\alias{write.fasta}
\title{ Write sequence(s) into a file in fasta format }
\description{
  Writes one or more sequences into a file in FASTA format.
}

\usage{write.fasta(sequences, names, file.out, open = "w", nbchar = 60)}

\arguments{
  \item{sequences}{ A DNA or protein sequence (in the form of a
    vector of single characters) or a list of such sequences. }
  \item{names}{ The name(s) of the sequences. }
  \item{nbchar}{ The number of characters per line (default: 60) }
  \item{file.out}{ The name of the output file. }
  \item{open}{ Mode to open the output file, use "w" to write into
  a new file, use "a" to append at the end of an already existing file.}
}
\value{
 none.
}
\references{
  \code{citation("seqinr")}
}
\author{ A. Necsulea }
\seealso{ \code{\link{read.fasta}} }
\examples{
## Read 3 sequences from a FASTA file:
ortho <- read.fasta(file = system.file("sequences/ortho.fasta", package =
"seqinr"))

## Select only third codon positions:
ortho3 <- lapply(ortho, function(x) x[seq(from = 3, to = length(x), by = 3)])

## Write the 3 modified sequences to a file:
write.fasta(sequences = ortho3, names = names(ortho3), nbchar = 80, file.out = "ortho3.fasta")

## Read them again from the same file and check that sequences are preserved:
ortho3bis <- read.fasta("ortho3.fasta", set.attributes = FALSE)
stopifnot(identical(ortho3bis, ortho3))
}
\keyword{ utilities }
