\name{plot.SeqAcnucWeb}
\alias{plot.SeqAcnucWeb}
\title{To Plot Subsequences on the Parent Sequence}
\description{
  This function allows to plot all the type of subsequences on a parent sequence.Subsequences are represented by colored rectangle on the parent sequence.  As an example, types could be CDS, TRNA, RRNA .... In order to get all the types that are available for the selected database, please run \code{getType.socket}. 
}
\usage{
\method{plot}{SeqAcnucWeb}(x, type = "all", ...)
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

\code{citation("seqinr")}
}
\author{D. Charif}
\seealso{\code{\link{getType}}, \code{\link{query}} }
\examples{
\dontrun{ 
  ### Need internet connection
  choosebank("hovernucl")
  query("list","sp=homo sapiens et k=globin@")	
  plot.SeqAcnucWeb(list$req[[22]])
  plot.SeqAcnucWeb(list$req[[22]],type=c("CDS","5'NCR"))
  }
}
\keyword{ hplot }
