\name{print.qaw}
\alias{print.qaw}
\title{Print method for objects from class qaw}
\description{
 Print the number of elements, their type and the corresponding query.
}
\usage{
\method{print}{qaw}(x, ...)
}
\arguments{
  \item{x}{A objet of class \code{qaw}}
  \item{\dots}{not used}	
}
\value{
 None.
}
\references{ 
\code{citation("seqinr")}
}
\author{J.R. Lobry}
\seealso{\code{\link{print}} }
\examples{
\dontrun{ 
  ### Need internet connection
  choosebank("emblTP")
  list1 <- query("sp=felis catus")	
  list1
  # 4732 SQ for sp=felis catus
  }
}
