\name{tablecode}
\alias{tablecode}
\title{ to plot genetic code as in textbooks }
\description{
This function plots a genetic code table as in textbooks, that is
following the order \code{u > c > a > g} so that synonymous codons
are almost always in the same boxes.
}
\usage{
tablecode(numcode = 1, urn.rna = c("u", "c", "a", "g"), dia = TRUE)
}
\arguments{
  \item{numcode}{ The genetic code number as in \code{translate} }
  \item{urn.rna}{ The letters to display codons, use c("t","c",
"a","g") if you want the code in terms of DNA sequence}
  \item{dia}{ to produce a yellow/blue plot for slides }
}
\references{ 
To have an overview of the seqinR's functionnality, please consult this vignette: 
Charif, D., Lobry, J.R. (2005) SeqinR: a contributed package to the R project for statistical
computing devoted to biological sequences retrieval and analysis. Springer Verlag, \emph{Biological and Medical Physics/Biomedical Series}, in preparation. \cr
}
\author{J.R. Lobry }
\seealso{ \code{\link{translate}} }
\examples{
 tablecode()
}
\keyword{ utilities }
