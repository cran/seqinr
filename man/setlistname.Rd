\name{setlistname}
\alias{setlistname}
\title{Sets the name of an ACNUC list identified by its rank}
\description{
This is a low level function to set the name of a list from an ACNUC server.
It should not be used directly by end users.
}
\usage{
setlistname(lrank, name = "list1", socket = autosocket())
}
\arguments{
  \item{lrank}{the list rank on the ACNUC server}
  \item{name}{the name to use for this list}
  \item{socket}{an object of class \code{sockconn} connecting to a remote ACNUC
                        database (default is a socket to the last opened database).}
}
\value{
A single numeric value corresponding to:

  \item{NA}{Empty answer from server.}
  \item{0}{OK.}
  \item{3}{if another list with that name already existed and was deleted.}
  \item{4}{no list of rank \code{lrank} exists.}
}

\references{ \url{http://doua.prabi.fr/databases/acnuc.html}

\code{citation("seqinr")}
}
\author{J.R. Lobry}
\seealso{\code{\link{choosebank}}, \code{\link{query}}, \code{\link{glr}} }
\examples{
\dontrun{ 
  ### Need internet connection
  choosebank("emblTP")
  mylist <- query("mylist", "sp=felis catus et t=CDS", virtual = TRUE)
  # Change list name on server:
  setlistname(lrank = glr("mylist"), name = "feliscatus") # 0, OK.
  glr("mylist") # 0, list doesn't exist no more.
  glr("feliscatus") # 2, this list exists.
  # Note the danger here: the object mylist is still present in the user workspace
  # while the corresponding list was deleted from server.
  }
}
\keyword{ utilities }
