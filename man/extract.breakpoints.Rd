\name{extract.breakpoints}
\alias{extract.breakpoints}
\title{Extraction of breakpoint positions on the
  rearranged nucleotide skews. }
\description{
  Extraction of breakpoint positions on the
  rearranged nucleotide skews. 
  }

\usage{extract.breakpoints(rearr.ori,
type = c("atfw", "atrev", "gcfw", "gcrev"),
 nbreaks, gridsize = 100, it.max = 500)}

\arguments{
\item{rearr.ori}{A data frame obtained with the \code{rearranged.oriloc}
  function.  }
\item{type}{The type of skew for which to extract the breakpoints; must
  be a subset of \code{c("atfw","atrev","gcfw","gcrev")}.}
\item{nbreaks}{The number of breakpoints to extract for each type of
  skew. Provide a vector of the same length as \code{type}.}
\item{gridsize}{To make sure that the best breakpoints are found, and to
avoid finding only a local extremum of the likelihood and residual sum
of square functions, a grid search is performed. The search for
breakpoints is repeated \code{gridsize} times, with different starting
values for the breakpoints. }
\item{it.max}{The maximum number of iterations to be performed when
  searching for the breakpoints. This argument corresponds to the
  \code{it.max} argument in \code{segmented}.}
}
\details{
  This method uses the \code{segmented} function in the \code{segmented}
  package to extract the breakpoints positions in the rearranged
  nucleotide skews obtained with the \code{rearranged.oriloc} function.
To make sure that the best breakpoints are found, and to
avoid finding only a local extremum of the likelihood and residual sum
of square functions, a grid search is performed. The search for
breakpoints is repeated \code{gridsize} times, with different starting
values for the breakpoints. 
  
}

\value{

  This function returns a list, with as many elements as the \code{type}
  argument (for example \code{$gcfw} will contain the results for the
  rearranged GC-skew, for forward-encoded genes). Each element of this list is also a list, containing the
  following information: in  \code{$breaks} the position of the breakpoints on the
  rearranged chromosome; in  \code{$slopes.left} the slopes of the
  segments on the left side of each breakpoint; in  \code{$slopes.right} the slopes of the
  segments on the right side of each breakpoint; in \code{$real.coord},
  the coordinates of the breakpoints on the real chromosome (before rearrangement).

  }

\references{

  \code{citation("segmented")}
  
  Necşulea, A. and Lobry, J.R. (in prep) A novel method for assessing
  the effect of replication on DNA base composition asymmetry.
  \emph{Molecular Biology and Evolution},\bold{24}:2169-2179.

}
\author{A. Necşulea}
\seealso{ \code{\link{oriloc}}, \code{\link{draw.rearranged.oriloc}},
  \code{\link{rearranged.oriloc}}  }

\examples{

### Example for Chlamydia trachomatis ####

### Rearrange the chromosome and compute the nucleotide skews ###

\dontrun{r.ori <- rearranged.oriloc(seq.fasta = system.file("sequences/ct.fasta.gz", package = "seqinr"),
    g2.coord = system.file("sequences/ct.coord",package = "seqinr"))}

### Extract the breakpoints for the rearranged nucleotide skews ###


\dontrun{breaks <- extract.breakpoints(r.ori,type = c("gcfw", "gcrev"),
 nbreaks = c(2, 2), gridsize = 50, it.max = 100)}


### Draw the rearranged nucleotide skews and ###
### place the position of the breakpoints on the graphics ###

\dontrun{draw.rearranged.oriloc(r.ori, breaks.gcfw = breaks$gcfw$breaks,
 breaks.gcrev = breaks$gcrev$breaks)}
}

\keyword{utilities}
