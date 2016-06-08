\name{plot.SeqAcnucWeb}
\alias{plot.SeqAcnucWeb}
\title{To Plot Subsequences on the Parent Sequence}
\description{
  This function plots all the type of subsequences on a parent sequence. Subsequences are represented by colored rectangle on the parent sequence.  For example, types could be CDS, TRNA, RRNA .... In order to get all the types that are available for the selected database, use \code{getType}. 
}
\usage{
\method{plot}{SeqAcnucWeb}(x, types = getType()$sname, socket = autosocket(), ...)
}
\arguments{
  \item{x}{A sequence of class \code{SeqAcnucWeb}}
  \item{types}{ The type of subsequences to plot. Default value is to consider
    all possible subsequence types.}
  \item{socket}{an object of class \code{sockconn} connecting to a remote ACNUC
                        database (default is a socket to the last opened database).}
  \item{\dots}{not currently used}	
}
\value{
 An invisible list giving, for each subsequence, its position on the parent sequence.
}
\references{ \url{http://doua.prabi.fr/databases/acnuc.html}

\code{citation("seqinr")}
}
\author{D. Charif, J.R. Lobry}
\seealso{\code{\link{getType}}, \code{\link{query}} }
\examples{
\dontrun{ 
  ### Need internet connection
  choosebank("emblTP")
  mylist <- query("mylist", "AC=AB078009")	
  plot(mylist$req[[1]])
  }
}
\keyword{ hplot }
