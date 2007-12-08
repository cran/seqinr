\name{parser.socket}
\alias{parser.socket}

\title{Utility function to parse answers from an ACNUC server}
\description{
Answers from server looks like : "code=0&lrank=2&count=150513&type=SQ&locus=F".
}
\usage{
parser.socket(onelinefromserver)
}
\arguments{
  \item{onelinefromserver}{a string}
}
\value{
A vector of mode character or NULL if \code{onelinefromserver} is NULL.
}
\references{
\code{citation("seqinr")}
}
\author{J.R. Lobry}
\seealso{\code{\link{choosebank}}, \code{\link{query}} }
\examples{
stopifnot(all(parser.socket("code=0&lrank=2&count=150513&type=SQ&locus=F") 
                                     == c("0", "2", "150513", "SQ", "F")))
}
\keyword{ utilities }
