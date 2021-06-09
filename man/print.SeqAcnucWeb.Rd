\name{print.SeqAcnucWeb}
\alias{print.SeqAcnucWeb}
\title{Print method for objects from class SeqAcnucWeb}
\description{
 Print the name, length, frame and genetic code number.
}
\usage{
\method{print}{SeqAcnucWeb}(x, ...)
}
\arguments{
  \item{x}{A sequence of class \code{SeqAcnucWeb}}
  \item{\dots}{Arguments passed to \code{print}}
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
  mylist <- query("mylist", "sp=felis catus")
  mylist$req[[1]]
  #      name   length    frame   ncbigc 
  # "A06937"     "34"      "0"      "1"
  }
}
\keyword{ manip }
