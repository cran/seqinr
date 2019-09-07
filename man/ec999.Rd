\name{ec999}
\alias{ec999}
\docType{data}
\title{ 999 coding sequences from E. coli }
\description{
This dataset contains 999 coding sequences from the Escherichia
coli chromosome}
\usage{data(ec999)}
\format{ List of 999 vectors of characters, one for each coding
sequence.
\describe{
 \item{ECFOLE.FOLE    }{chr [1:672] "A" "T" "G" "C" ...}
 \item{ECMSBAG.MSBA   }{chr [1:1749] "A" "T" "G" "C" ...}
 \item{ECNARZYW-C.NARV}{chr [1:681] "A" "T" "G" "A" ...}
 \item{...            }{ ... TRUNCATED ... }
 \item{XYLEECOM.MALK  }{chr [1:1116] "A" "T" "G" "G" ...}
 \item{XYLEECOM.LAMB  }{chr [1:1341] "A" "T" "G" "A" ...}
 \item{XYLEECOM.MALM  }{chr [1:921] "A" "T" "G" "A" ...}
}
}
\references{
Lobry, J.R., Gautier, C. (1994) Hydrophobicity,
expressivity and aromaticity are the major trends of amino-acid usage in
999 \emph{Escherichia coli} chromosome-encode genes. \emph{Nucleic Acids
  Research},\bold{22}:3174-3180.

\code{citation("seqinr")}
}
\examples{
data(ec999)
#
# How to export sequences in a FASTA file:
#
fname <- tempfile(pattern = "ecc999", tmpdir = tempdir(), fileext = "ffn")
tempdir(check = FALSE)
write.fasta(ec999, names(ec999), file = fname)
}
\keyword{datasets}
