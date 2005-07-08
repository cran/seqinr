\name{tablecode}
\alias{tablecode}
\title{ to plot genetic code as in textbooks }
\description{
This function plots a genetic code table as in textbooks, that is
following the order \code{u > c > a > g} so that synonymous codons
are almost always in the same boxes.
}
\usage{
tablecode(numcode = 1, urn.rna = c("u", "c", "a", "g"), dia = TRUE, latexfile = NULL)
}
\arguments{
  \item{numcode}{ The genetic code number as in \code{translate} }
  \item{urn.rna}{ The letters to display codons, use c("t","c",
"a","g") if you want the code in terms of DNA sequence}
  \item{dia}{ to produce a yellow/blue plot for slides }
  \item{latexfile}{ The name of a LaTex file if you want to redirect the output}
}
\references{ 
  \code{citation("seqinr")}
}
\author{ J.R. Lobry }
\seealso{ \code{\link{translate}} }
\examples{
 tablecode()
}
\keyword{ utilities }
