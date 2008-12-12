\name{stresc}
\alias{stresc}
\title{Utility function to escape LaTeX special characters present in a string}
\description{
 This function returns a vector of strings in which LaTeX special characters are
 escaped, this was useful in conjunction with xtable.
}
\usage{
stresc(strings)
}
\arguments{
  \item{strings}{A vector of strings to deal with.}
}
\value{
  Returns a vector of strings with escaped characters within each string.
}
\references{
  \code{citation("seqinr")}
}
\author{J.R. Lobry}
\seealso{ \code{\link{s2c}} }
\examples{
  stresc("MISC_RNA")
  stresc(c("BB_0001","BB_0002"))
}


