\name{getType}
\alias{getType}
\title{To get available subsequence types in an opened ACNUC database}
\description{
 This function returns all subsequence types (e.g. CDS, TRNA) present 
 in an opened ACNUC database, using default database if no socket is
 provided.
}
\usage{
getType(socket = autosocket())
}
\arguments{
  \item{socket}{an object of class \code{sockconn} connecting to a remote ACNUC
                        database (default is a socket to the last opened database).}
}
\value{
  a list containing a short description for each subsequence type.
}
\references{
  \code{citation("seqinr")}
}
\author{D. Charif, J.R. Lobry}
\seealso{ \code{\link{choosebank}}, \code{\link{query}} }
\examples{
\dontrun{
# Need internet connection
  choosebank("emblTP")
  getType()}
}
\keyword{utilities}
