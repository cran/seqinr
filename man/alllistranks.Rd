\name{alllistranks}
\alias{alllistranks}
\alias{alr}
\title{To get the count of existing lists and all their ranks on server}
\description{
This is a low level function to get the total number of list and all their
ranks in an opened database.
}

\usage{
alllistranks(socket = autosocket(), verbose = FALSE)
alr(socket = autosocket(), verbose = FALSE)
}

\arguments{
  \item{socket}{an object of class \code{sockconn} connecting to a remote ACNUC
                        database (default is a socket to the last opened database).}
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
 \dontrun{# Need internet connection
 choosebank("emblTP")
 tmp1 <- query("tmp1", "sp=Borrelia burgdorferi", virtual = TRUE)
 tmp2 <- query("tmp2", "sp=Borrelia burgdorferi", virtual = TRUE)
 tmp3 <- query("tmp3", "sp=Borrelia burgdorferi", virtual = TRUE)
 (result <- alllistranks())
 stopifnot(result$count == 3)   # Three ACNUC lists
 stopifnot(result$ranks == 2:4) # Starting at rank 2
 #
 # Summay of current lists defined on the ACNUC server:
 #
 sapply(result$ranks, getliststate)
 closebank()
 }
}
\keyword{utilities}
