\name{dist.alignment}
\alias{dist.alignment}
\title{ Pairwise Distances from Aligned Protein or DNA/RNA Sequences }
\description{
  These functions compute a matrix of pairwise distances from aligned sequences 
  using similarity (Fitch matrix, for protein sequences only) or identity matrix 
  (for protein and DNA sequences).
  The resulting matrix contains the squared root of the pairwise distances.
  For example, if identity between 2 sequences is 80%, the matrix will gives
  the squared root of (1.0 - 0.8) i.e. 0.4472136.

}
\usage{
dist.alignment(x, matrix = c("similarity", "identity"),gap)
}
\arguments{
  \item{x}{an object of class \code{alignment}, as returned by 
  \code{read.alignment} for instance}
  \item{matrix}{the matrix distance to be used, partial matching allowed }
  \item{gap}{-optional- with nucleotides, if set to 1, gaps will be counted in the identity measure }
}
\value{
  The distance matrix, object of class \code{dist}, computed by using the 
  specified distance measure.
}
\references{ 
The reference for the similarity matrix is :\cr
Fitch, W.M. (1966) An improved method of testing for evolutionary homology. 
\emph{J. Mol. Biol.}, \bold{16}:9-16.\cr

\code{citation("seqinr")}
}
\author{D. Charif, J.R. Lobry}
\seealso{  \code{\link{read.alignment} } }
\examples{
 myseqs <- read.alignment(file = system.file("sequences/test.mase", 
 package = "seqinr"), format = "mase")
 dist.alignment(myseqs, matrix = "identity" )
 as.matrix(dist.alignment(myseqs, matrix = "identity" ))
}
\keyword{ manip }
