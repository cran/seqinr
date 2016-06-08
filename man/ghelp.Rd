\name{ghelp}
\alias{ghelp}
\title{Get help from an ACNUC server}
\description{
Reads one item of information in specified help file from an ACNUC server.
The are differences between ACNUC clients so that this help could be
confusing. However, the query language is common to all clients so that
the most recent documentation is most likely here.
}
\usage{
ghelp(item = c("GENERAL", "SELECT", "SPECIES", "KEYWORD"),
 file = c("HELP", "HELP_WIN"), socket = autosocket(), catresult = TRUE)

}
\arguments{
  \item{item}{the name of the desired help item}
  \item{file}{the name of the help file on server side.}
  \item{socket}{an object of class \code{sockconn} connecting to a remote ACNUC
                        database (default is a socket to the last opened database).}
  \item{catresult}{logical. If TRUE output is redirected to the console.}
}

\value{
A vector of string which is returned invisibly and "cated" to the console
by default.
}
\references{ \url{http://doua.prabi.fr/databases/acnuc.html}

\code{citation("seqinr")}
}
\author{J.R. Lobry}
\seealso{\code{\link{choosebank}}, \code{\link{query}} }
\examples{
\dontrun{ 
  ### Need internet connection
  choosebank("emblTP")
  ghelp()
  ghelp("SELECT")
  # To get info about current database:
  ghelp("CONT")
  }
}
\keyword{ utilities }
