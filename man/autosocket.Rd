\name{autosocket}
\alias{autosocket}
\title{Returns a socket to the last opened database}
\description{
This is a low level function that is mainly used to select automatically
the last opened ACNUC database for functions using sockets.
}
\usage{
autosocket()
}

\value{
An object of class sockconn.
}
\references{ \url{http://doua.prabi.fr/databases/acnuc.html}

\code{citation("seqinr")}
}
\author{J.R. Lobry}
\seealso{\code{\link{choosebank}}, \code{\link{connections}}.}
\examples{
\dontrun{ #Need internet connection
  choosebank("emblTP")
  autosocket()
  closebank()
  }
}
\keyword{ utilities }
