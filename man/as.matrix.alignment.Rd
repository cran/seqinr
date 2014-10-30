\name{as.matrix.alignment}
\alias{as.matrix.alignment}

\title{as.matrix.alignment}
\description{
Converts an alignment into a matrix of characters
}
\usage{
\method{as.matrix}{alignment}(x, ...)
}
\arguments{
  \item{x}{ an object of the class alignment.}
  \item{...}{additional arguments to be passed to or from methods.}
}
\value{
  A matrix of characters.
}
\author{J.R. Lobry} 
\seealso{
  \code{\link{read.alignment}}
}
\examples{
  phylip <- read.alignment(file = system.file("sequences/test.phylip",
   package = "seqinr"), format = "phylip")
  as.matrix(phylip)
}
