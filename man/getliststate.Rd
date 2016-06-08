\name{getliststate}
\alias{getliststate}
\alias{gls}
\alias{gln}
\title{Asks for information about an ACNUC list of specified rank}
\description{
Reply gives the type of list, its name, the number of elements it contains,
and, for sequence lists, says whether the list contains only parent seqs (locus=T).
}
\usage{
getliststate(lrank, socket = autosocket())
gls(lrank, socket = autosocket())
gln(lrank, ...)
}
\arguments{
  \item{lrank}{the name of the ACNUC list to modify}
  \item{socket}{an object of class \code{sockconn} connecting to a remote ACNUC
                        database (default is a socket to the last opened database).}
  \item{...}{arguments passed to getliststate}
}

\value{
NA in case of problem and an warning is issued. When there is no problem
a list with the following 4 components:

  \item{type}{string. Type of ACNUC list (SQ, KW, SP)}
  \item{name}{string. ACNUC list name}
  \item{count}{numeric. Number of elements in ACNUC list}
  \item{locus}{logical. For ACNUC sequence lists TRUE means that the list 
         contains only parent sequences. NA otherwise.}

\code{gln} is a shortcut for \code{getliststate(lrank, ...)$name}
}
\references{ \url{http://doua.prabi.fr/databases/acnuc.html}

\code{citation("seqinr")}
}
\author{J.R. Lobry}
\seealso{\code{\link{choosebank}}, \code{\link{query}}, \code{\link{alr}},
\code{\link{glr}} }
\examples{
\dontrun{ 
  ### Need internet connection
  choosebank("emblTP")
  mylist <- query("mylist", "sp=felis catus et t=cds", virtual=TRUE)
  getliststate(glr("mylist")) # SQ, MYLIST, 603, FALSE
  gln(glr("mylist")) # MYLIST (upper case letters on server)
  closebank()
  }
}
\keyword{ utilities }
