\name{alllistranks}
\alias{alllistranks}
\alias{alr}
\title{To get the count of existing lists and all their ranks on server}
\description{
This is a low level function to get the total number of list and all their
ranks in an opened database.
}

\usage{
alllistranks(socket = "auto", verbose = FALSE)
alr(socket = "auto", verbose = FALSE)
}

\arguments{
  \item{socket}{a socket of class connection and sockconn returned by \code{choosebank}. 
   Default value (auto) means
   that the socket will be set to to the socket component of the banknameSocket variable. }
  \item{verbose}{if \code{TRUE}, verbose mode is on}
}

\details{
  This low level function is usually not used directly by the user.
}

\value{
  A list with two components:
  \item{count}{count of existing lists}	
  \item{rank}{their rank}
}
\references{ 
\code{citation("seqinr")}
}
\author{J.R. Lobry}
\seealso{
  \code{\link{choosebank}}, 
  \code{\link{query}}
}

\examples{
 \dontrun{
 # Need internet connection
 choosebank("genbank")
 query("tmp1", "sp=Borrelia burgdorferi", virtual=T)
 query("tmp2", "sp=Borrelia burgdorferi", virtual=T)
 query("tmp3", "sp=Borrelia burgdorferi", virtual=T)
 alr()
 #
 # Should be:
 #
 # $count
 # [1] 3
 #
 # $ranks
 # [1] 2 3 4
 #
 }
}
\keyword{utilities}
