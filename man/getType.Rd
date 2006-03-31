\name{getType}
\alias{getType}
\title{To get available subsequence types in an opened ACNUC database}
\description{
 This function returns all subsequence types (e.g. CDS, TRNA) present 
 in an opened ACNUC database, using default database if no socket is
 provided.
}
\usage{
getType(socket = "auto")
}
\arguments{
  \item{socket}{An object of class \code{connection} returned by 
  \code{choosebank}. The default bank is used by when socket == "auto".}
}
\value{
  It returns a list containing a short description for each sequence type.
}
\references{
  \code{citation("seqinr")}
}
\author{D. Charif and J.R. Lobry}
\seealso{ \code{\link{choosebank}}, \code{\link{query}} }
\examples{
\dontrun{
# Need internet connection
  choosebank("genbank")
  geType()}
}
\keyword{utilities}
