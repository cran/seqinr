\name{draw.rearranged.oriloc}
\alias{draw.rearranged.oriloc}
\title{Graphical representation for rearranged nucleotide skews in
  prokaryotic chromosomes.}
\description{
  Graphical representation for rearranged nucleotide skews in
  prokaryotic chromosomes.}

\usage{draw.rearranged.oriloc(rearr.ori, breaks.gcfw = NA,
 breaks.gcrev = NA, breaks.atfw = NA, breaks.atrev = NA)}

\arguments{

\item{rearr.ori}{A data frame obtained with the \code{rearranged.oriloc}
  function. }

\item{breaks.gcfw}{The coordinates of the breakpoints in the GC-skew,
  for forward transcribed protein coding sequences. These coordinates
  can be obtained with the \code{extract.breakpoints} function. }
\item{breaks.gcrev}{The coordinates of the breakpoints in the GC-skew,
  for reverse transcribed protein coding sequences. These coordinates
  can be obtained with the \code{extract.breakpoints} function. }
\item{breaks.atfw}{The coordinates of the breakpoints in the AT-skew,
  for forward transcribed protein coding sequences. These coordinates
  can be obtained with the \code{extract.breakpoints} function. }
\item{breaks.atrev}{The coordinates of the breakpoints in the AT-skew,
  for reverse transcribed protein coding sequences. These coordinates
  can be obtained with the \code{extract.breakpoints} function. }

}

\references{
  Necşulea, A. and Lobry, J.R. (2007) A New Method for Assessing the 
  Effect of Replication on DNA Base Composition Asymmetry.
  \emph{Molecular Biology and Evolution}, \bold{24}:2169-2179.
}
\author{J.R. Lobry, A. Necşulea}
\seealso{ \code{\link{rearranged.oriloc}},
  \code{\link{extract.breakpoints}}  }

\examples{

\dontrun{
### Example for Chlamydia trachomatis ####

### Rearrange the chromosome and compute the nucleotide skews ###

#r.ori <- rearranged.oriloc(seq.fasta = system.file("sequences/ct.fasta.gz", package = "seqinr"),
#    g2.coord = system.file("sequences/ct.coord", package = "seqinr"))

r.ori <- rearranged.oriloc(seq.fasta = system.file("sequences/ct.fasta.gz", package = "seqinr"),
    g2.coord = system.file("sequences/ct.coord", package = "seqinr"))



### Extract the breakpoints for the rearranged nucleotide skews ###

breaks <- extract.breakpoints(r.ori, type = c("gcfw", "gcrev"),
 nbreaks = c(2, 2), gridsize = 50, it.max = 100)

### Draw the rearranged nucleotide skews and  ###
### place the position of the breakpoints on the graphics ###

draw.rearranged.oriloc(r.ori, breaks.gcfw = breaks$gcfw$breaks,
 breaks.gcrev = breaks$gcrev$breaks)}
}
