\name{plot.SeqAcnucWeb}
\alias{plot.SeqAcnucWeb}
\title{To Plot Subsequences on the Parent Sequence}
\description{
  This function allows to plot all the type of subsequences on a parent sequence.Subsequences are represented by colored rectangle on the parent sequence.  As an example, types could be CDS, TRNA, RRNA .... In order to get all the types that are available for the selected database, please run \code{getType.socket}. 
}
\usage{
plot.SeqAcnucWeb(x, type = "all", ...)
}
\arguments{
  \item{x}{A sequence of class \code{SeqAcnucWeb}}
  \item{type}{ The type of subsequences to plot. if \code{all}, all the type of subsequences will be drawn.}
  \item{\dots}{graphical parameters can also be specified as arguments}	
}
\value{
 The plot.
 A list giving, for each subsequence, its position on the parent sequence.
}
\references{ \url{http://pbil.univ-lyon1.fr/databases/acnuc.html}

To have an overview of the seqinR's functionnality, please consult this vignette: 
Charif, D., Lobry, J.R. (2005) SeqinR: a contributed package to the R project for statistical
computing devoted to biological sequences retrieval and analysis. Springer Verlag, \emph{Biological and Medical Physics/Biomedical Series}, in preparation. \cr
}
\author{D. Charif}
\seealso{\code{\link{getType}}, \code{\link{query}} }
\examples{
\dontrun{ s=choosebank("hovernucl")}
\dontrun{ query(s$socket,"list","sp=homo sapiens et k=globin@")}	
\dontrun{ plot.SeqAcnucWeb(list$req[[22]]) }
\dontrun{ plot.SeqAcnucWeb(list$req[[22]],type=c("CDS","5'NCR")) }
}
\keyword{ hplot }
