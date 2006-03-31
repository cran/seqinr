\name{draw.rearranged.oriloc}
\alias{draw.rearranged.oriloc}
\title{Graphical representation for rearranged nucleotide skews in
  prokaryotic chromosomes.}
\description{
  Graphical representation for rearranged nucleotide skews in
  prokaryotic chromosomes.}

\usage{draw.rearranged.oriloc(rearr.ori, breaks.gcfw = NA, breaks.gcrev = NA, breaks.atfw = NA, breaks.atrev = NA)}

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
  Necsulea, A. and Lobry, J.R. (in prep) A novel method for assessing
  the effect of replication on DNA base composition asymmetry.
}
\author{Jean R. Lobry and A. Necsulea }
\seealso{ \code{\link{rearranged.oriloc}},
  \code{\link{extract.breakpoints}}  }

\examples{

### Example for Chlamydia trachomatis ####

### Rearrange the chromosome and compute the nucleotide skews ###

\dontrun{r.ori <- rearranged.oriloc(seq.fasta = system.file("sequences/ct.fasta",package = "seqinr"),
    g2.coord = system.file("sequences/ct.coord",package = "seqinr"))}

### Extract the breakpoints for the rearranged nucleotide skews ###

\dontrun{breaks <- extract.breakpoints(r.ori, type = c("gcfw", "gcrev"), nbreaks = c(2, 2), gridsize = 50, it.max = 100)}



### Draw the rearranged nucleotide skews and place the position of the breakpoints on the graphics ###

\dontrun{draw.rearranged.oriloc(r.ori, breaks.gcfw = breaks$gcfw$breaks, breaks.gcrev = breaks$gcrev$breaks)}
}

\keyword{utilities}
