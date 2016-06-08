\name{countsubseqs}
\alias{countsubseqs}
\alias{css}
\title{Number of subsequences in an ACNUC list}
\description{
Returns the number of subsequences in the ACNUC list of rank \code{lrank}. 
}
\usage{
countsubseqs(lrank, socket = autosocket())
css(lrank, socket = autosocket())
}
\arguments{
  \item{lrank}{the rank of the ACNUC list to consider.}
  \item{socket}{an object of class \code{sockconn} connecting to a remote ACNUC
                        database (default is a socket to the last opened database).}
}

\value{
Numeric.
}
\references{ \url{http://doua.prabi.fr/databases/acnuc.html}

\code{citation("seqinr")}
}
\author{J.R. Lobry}
\seealso{\code{\link{choosebank}}, \code{\link{query}}, \code{\link{glr}} to
get a list rank from its name.}
\examples{
\dontrun{ # Need internet connection
  choosebank("emblTP")
  mylist<-query("mylist", "N=@", virtual = TRUE) # select all (seqs + subseqs)
  mylist$nelem   # 14138094 seqs + subseqs
  stopifnot(mylist$nelem == 14138094)
  css(glr("mylist")) # 1604500 subsequences only
  stopifnot(css(glr("mylist")) == 1604500)
  closebank()
  }
}
\keyword{ utilities }
