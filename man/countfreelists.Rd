\name{countfreelists}
\alias{countfreelists}
\alias{cfl}
\title{The number of free lists available and annotation lines in an ACNUC server}
\description{
Returns the number of free lists available list of names of annotation lines in the 
opened ACNUC database. 
}
\usage{
countfreelists(socket = autosocket())
cfl(socket = autosocket())
}
\arguments{
  \item{socket}{an object of class \code{sockconn} connecting to a remote ACNUC
                        database (default is a socket to the last opened database).}
}

\value{
a list with the following 2 components:

  \item{free}{numeric. The number of free lists}
  \item{annotlines}{vector of strings. Names of annotation lines}
}
\references{ \url{http://doua.prabi.fr/databases/acnuc.html}

\code{citation("seqinr")}
}
\author{J.R. Lobry}
\seealso{\code{\link{choosebank}}, \code{\link{query}} }
\examples{
\dontrun{ # Need internet connection
  choosebank("emblTP")
  (rescountfreelists <- countfreelists())
  stopifnot(all(rescountfreelists$annotlines == 
   c("ALL", "AC",  "PR",  "DT",  "KW",  "OS",  "OC",
   "OG",  "RN",  "RC",  "RP",  "RX", "RG",  "RA",  "RT",  "RL",  "DR", 
   "CC",  "AH",  "AS",  "FH",  "FT",  "CO",  "SQ", "SEQ")))
  closebank()
  }
}
\keyword{ utilities }
