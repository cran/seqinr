\name{SeqAcnucWeb}
\alias{SeqAcnucWeb}
\alias{as.SeqAcnucWeb}
\alias{is.SeqAcnucWeb}

\title{Sequence coming from a remote ACNUC data base}
\description{
    \code{as.SeqAcnucWeb} is called by many functions, for instance by \code{query}, 
  and should not be directly called by the user. It creates an object of class \code{SeqAcnucWeb}. 
  \code{is.SeqAcnucWeb} returns TRUE if the object is of class \code{SeqAcnucWeb}. 
}
\usage{
as.SeqAcnucWeb(object, length, frame, ncbigc)
is.SeqAcnucWeb(object)
}
\arguments{
  \item{object}{ a string giving the name of a sequence present in the data base}
  \item{length}{ a string giving the length of the sequence present in the data base}
  \item{frame}{ a string giving the frame of the sequence present in the data base}
  \item{ncbigc}{ a string giving the ncbi genetic code of the sequence present in the data base}
}
\value{
  \code{as.SeqAcnucWeb} returns an object sequence of class \code{SeqAcnucWeb}. Note
  that as from seqinR 1.1-3 the slot socket has been deleted to save space for long lists.
}
\references{
  \code{citation("seqinr")}
}
\author{D. Charif, J.R. Lobry}
\examples{
  \dontrun{# Need internet connection
  choosebank("emblTP")
  mylist <- query("mylist", "sp=felis catus et t=cds et o=mitochondrion")
  stopifnot(is.SeqAcnucWeb(mylist$req[[1]]))
  closebank()
}
}
\keyword{utilities}
