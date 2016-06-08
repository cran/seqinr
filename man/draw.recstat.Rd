\name{draw.recstat}
\alias{draw.recstat}
\title{Graphical representation of a recstat analysis.}
\description{This function displays the results returned by \code{recstat} with two plots. The
first one shows the factor scores of a CA computed on the codon composition of a DNA sequence.
The second one shows the locations of all Start and Stop codons in this sequence.}
\usage{draw.recstat(rec, fac = 1, direct  = TRUE, xlim = c(1, seqsize),
    col = c("red", "blue", "purple"))}
\arguments{
    \item{rec}{list of elements returned by \code{recstat} function.}
    \item{fac}{axis of the CA to use for display (4 \eqn{\ge} \code{fac}
    \eqn{\ge} 1).}
    \item{direct}{a logical for the choice of direct or reverse strand.}
    \item{xlim}{starting and ending positions in the sequence for the plot.}
    \item{col}{vector of colour codes for the three frames of the sequence.}
}
\details{The first plot shows the factor scores of the sliding windows, this for the three
possible frames of the strand selected by the user. The second shows the Start (filled grey
triangles pointing up) and Stop (solid black triangles pointing down) codons positions. Note
that the standard genetic code is used for that purpose. Visual detection of putative CDS is
performed through the simultaneous use of these two graphics. If a CDS is located within the
sequence, the factor scores for the windows located in the corresponding reading frame will be
significantly separated from the two others. Moreover, the region where this separation is seen
should be located between a Start and a Stop codon.}
\author{O. Clerc, G. Perrière}
\seealso{\code{\link{test.li.recstat}}, \code{\link{test.co.recstat}}}
\examples{
ff <- system.file("sequences/ECOUNC.fsa", package = "seqinr")
seq <- read.fasta(ff)
rec <- recstat(seq[[1]], seqname = getName(seq))
draw.recstat(rec)
}
\keyword{correspondence analysis}
\keyword{sequence}