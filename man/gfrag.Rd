\name{gfrag}
\alias{gfrag}
\title{Extract sequence identified by name or by number from an ACNUC server}
\description{
Get \code{length} characters from sequence identified by name or by number 
starting from position \code{start} (counted from 1). 
}
\usage{
gfrag(what, start, length, idby = c("name", "number"), socket = autosocket())
}
\arguments{
  \item{what}{A sequence name or number}
  \item{start}{Start position from 1}
  \item{length}{Number of requested characters (answer may be shorter)}
  \item{idby}{Is the sequence identified by name or number? Default to name}
  \item{socket}{an object of class \code{sockconn} connecting to a remote ACNUC
                        database (default is a socket to the last opened database).}
}
\value{
A string of characters with at most \code{length} characters (may be 
shorter than asked for). NA is returned and a warning is issued in
case of problem (non existent sequence for instance).
}
\references{ \url{http://doua.prabi.fr/databases/acnuc.html}

\code{citation("seqinr")}
}
\author{J.R. Lobry}
\seealso{\code{\link{choosebank}}, \code{\link{query}} }
\examples{
\dontrun{# Need internet connection
  choosebank("emblTP")
  gfrag("LMFLCHR36", start = 1, length = 3529852) -> myseq
  stopifnot(nchar(myseq) == 3529852)
  closebank()	
  }
}
