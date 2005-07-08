\name{closebank}
\alias{closebank}
\title{ To close a remote ACNUC database }
\description{
 This function tries to close a remote ACNUC database.
}
\usage{
closebank(bank = NA, host = "pbil.univ-lyon1.fr", port = 5558, verbose = FALSE)
}
\arguments{
  \item{bank}{ String. if \code{NA} the last opened database as given by banknameSocket
    in the global environment will be closed.}
  \item{host}{ String. Host name for port }
  \item{port}{ Integer. The TCP port number }
  \item{verbose}{ Logical. If TRUE, verbose mode is on }
}
\references{
  \code{citation("seqinr")}
}
\seealso{ \code{\link{choosebank}}}
\examples{
  \dontrun{ choosebank("genbank") }
  \dontrun{ closebank() }
}
\keyword{ manip }
