\name{dist.alignment}
\alias{dist.alignment}
\title{ Pairwise Distances from Aligned Protein or DNA/RNA Sequences }
\description{
  These functions compute a matrix of pairwise distances from aligned sequences using similarity (Fitch matrix) or identity matrix.
}
\usage{
dist.alignment(x, matrix = "similarity")
}
\arguments{
  \item{x}{ object of class \code{alignment} }
  \item{matrix}{ the matrix distance to be used: "similarity" or "identity" }
}
\value{
  The distance matrix, object of class \code{dist}, computed by using the specified distance measure.
}
\references{ 
The reference for the similarity matrix is :\cr
Fitch, W.M. (1966) Mutation values for the interconversion of amino acid pair. 
\emph{J. Mol. Biol.}, \bold{16}:9-16.\cr
 

\code{citation("seqinr")}
}
\author{ D. Charif}
\seealso{  \code{\link{read.alignment} } }
\examples{
 s = read.alignment(File=system.file("sequences/test.mase", package = "seqinr"), format="mase")
 dist.alignment( s, matrix = "identity" )
}
\keyword{ manip }
