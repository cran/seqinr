\name{closebank}
\alias{closebank}
\title{ To close a remote ACNUC database }
\description{
 This function tries to close a remote ACNUC database.
}
\usage{
closebank(socket = autosocket(), verbose = FALSE)
}
\arguments{
  \item{socket}{an object of class \code{sockconn} connecting to a remote ACNUC
                        database (default is a socket to the last opened database).}
  \item{verbose}{ Logical. If TRUE, verbose mode is on }
}
\references{
  \code{citation("seqinr")}
}
\author{J.R. Lobry }

\seealso{ \code{\link{choosebank}}}
\examples{
  \dontrun{# Need internet connection
   choosebank("emblTP")
   closebank()
  }
}
\keyword{ manip }
