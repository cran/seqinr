\name{acnucopen}
\alias{acnucopen}
\alias{acnucclose}
\alias{clientid}
\alias{quitacnuc}
\title{open and close a remote access to an ACNUC database}
\description{
These are low level functions to start and stop a remote access to an ACNUC database.
}

\usage{
acnucopen(db, socket, challenge = NA)
acnucclose(socket)
clientid(id = paste("seqinr_",
 packageDescription("seqinr")$Version, sep = ""),
 socket, verbose = FALSE)
quitacnuc(socket)
}

\arguments{
  \item{db}{the remote ACNUC database name}
  \item{socket}{an object of class \code{sockconn} connecting to an ACNUC server}
  \item{challenge}{unimplemented yet}
  \item{id}{client ID definition defaulting to seqinr + package version number}
  \item{verbose}{logical, if TRUE mode verbose is on}
}

\details{
these low level functions are usually not used directly by the user. 
Use \code{\link{choosebank}} to open a remote ACNUC database
and \code{\link{closebank}} to close it.
}

\value{
For \code{openacnuc} a list with the following
components: type : the type of database that was opened.
totseqs, totspec, totkey : total number of seqs, species, keywords in opened database.
ACC\_LENGTH, L\_MNEMO, WIDTH\_KW, WIDTH\_SP, WIDTH\_SMJ, WIDTH\_AUT, 
WIDTH\_BIB, lrtxt, SUBINLNG: max lengths of record keys in database.

}
\references{ 
\code{citation("seqinr")}
}
\author{J.R. Lobry}
\seealso{
  \code{\link{choosebank}}, \code{\link{closebank}}
}

\examples{
 \dontrun{# Need internet connection
  mysocket <- socketConnection( host = "pbil.univ-lyon1.fr", 
    port = 5558, server = FALSE, blocking = TRUE)
  readLines(mysocket, n = 1) # OK acnuc socket started
  acnucopen("emblTP", socket = mysocket) -> res
  expected <- c("EMBL", "14138095", "236401", "1186228", "8", 
    "16", "40", "40", "20", "20", "40", "60", "504")
  stopifnot(all(unlist(res) == expected))
  tryalreadyopen <- try(acnucopen("emblTP", socket = mysocket))
  stopifnot(inherits(tryalreadyopen, "try-error"))
  # Need a fresh socket because acnucopen() close it if error:
  mysocket <- socketConnection( host = "pbil.univ-lyon1.fr", 
    port = 5558, server = FALSE, blocking = TRUE)
  tryoff <-  try(acnucopen("off", socket = mysocket))
  stopifnot(inherits(tryoff, "try-error"))

  mysocket <- socketConnection( host = "pbil.univ-lyon1.fr", 
    port = 5558, server = FALSE, blocking = TRUE)
  tryinexistent <-  try(acnucopen("tagadatagadatsointsoin", socket = mysocket))
  stopifnot(inherits(tryinexistent, "try-error"))

  mysocket <- socketConnection( host = "pbil.univ-lyon1.fr", 
    port = 5558, server = FALSE, blocking = TRUE)
  trycloseunopened <- try(acnucclose(mysocket))
  stopifnot(inherits(trycloseunopened, "try-error"))

 }
}
\keyword{utilities}
