\name{getlistrank}
\alias{getlistrank}
\alias{glr}
\title{To get the rank of a list from its name}
\description{
This is a low level function to get the rank of a list on server from its name.
}

\usage{
getlistrank(listname, socket = autosocket(), verbose = FALSE)
glr(listname, socket = autosocket(), verbose = FALSE)
}

\arguments{
  \item{listname}{the name of list on server}
  \item{socket}{an object of class \code{sockconn} connecting to a remote ACNUC
                        database (default is a socket to the last opened database).}
  \item{verbose}{if \code{TRUE}, verbose mode is on}
}

\details{
  This low level function is usually not used directly by the user.
}

\value{
  The rank of list named \code{listname} on server, or 0 if no list with this name exists.
}
\references{ 
\code{citation("seqinr")}
}
\author{J.R. Lobry}
\seealso{
  \code{\link{choosebank}}, \code{\link{query}}
}

\examples{
 \dontrun{
 # Need internet connection
 choosebank("emblTP")
 MyListName <- query("MyListName", "sp=Borrelia burgdorferi", virtual = TRUE)
 (result <- getlistrank("MyListName"))
 stopifnot(result == 2)
 closebank()
 }
}
\keyword{utilities}
