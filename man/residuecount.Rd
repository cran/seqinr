\name{residuecount}
\alias{residuecount}
\title{Total number of residues in an ACNUC list}
\description{
Computes the total number of residues (nucleotides or aminoacids) in
all sequences of the list of specified rank.
}
\usage{
residuecount(lrank, socket = autosocket())
}
\arguments{
  \item{lrank}{the list rank on the ACNUC server}
  \item{socket}{an object of class \code{sockconn} connecting to a remote ACNUC
                        database (default is a socket to the last opened database).}
}
\value{
A single numeric value corresponding to  the total number of residues or
NA in case of problem.
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
  mylist <- query("mylist", "t=CDS", virtual = TRUE)
  stopifnot(residuecount(glr("mylist")) == 1611439240)
  stopifnot(is.na(residuecount(glr("unknowlist")))) # A warning is issued
  }
}
\keyword{ utilities }
