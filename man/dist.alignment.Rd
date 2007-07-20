\name{dist.alignment}
\alias{dist.alignment}
\title{ Pairwise Distances from Aligned Protein or DNA/RNA Sequences }
\description{
  These functions compute a matrix of pairwise distances from aligned sequences 
  using similarity (Fitch matrix) or identity matrix.
}
\usage{
dist.alignment(x, matrix = c("similarity", "identity"))
}
\arguments{
  \item{x}{an object of class \code{alignment}, as returned by 
  \code{read.alignment} for instance}
  \item{matrix}{the matrix distance to be used, partial matching allowed }
}
\value{
  The distance matrix, object of class \code{dist}, computed by using the 
  specified distance measure.
}
\references{ 
The reference for the similarity matrix is :\cr
Fitch, W.M. (1966) Mutation values for the interconversion of amino acid pair. 
\emph{J. Mol. Biol.}, \bold{16}:9-16.\cr

\code{citation("seqinr")}
}
\author{D. Charif and J.R. Lobry}
\seealso{  \code{\link{read.alignment} } }
\examples{
 myseqs <- read.alignment(file = system.file("sequences/test.mase", 
 package = "seqinr"), format = "mase")
 dist.alignment(myseqs, matrix = "identity" )
}
\keyword{ manip }
