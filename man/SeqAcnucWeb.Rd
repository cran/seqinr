\name{SeqAcnucWeb}
\alias{SeqAcnucWeb}
\alias{as.SeqAcnucWeb}
\alias{is.SeqAcnucWeb}
\alias{parser.socket}
\title{Sequence coming from an ACNUC data bases located on the web}
\description{
    \code{as.SeqAcnucWeb} is called by many functions, for instance by \code{query.SeqAcnucWeb}, and should not directly be called by the user. It creates an object of class \code{SeqAcnucWeb}. 
  \code{is.SeqAcnucWeb} returns TRUE if the object is of class \code{SeqAcnucWeb}. 
}
\usage{
as.SeqAcnucWeb(object,length, frame, ncbigc,socket=F)
is.SeqAcnucWeb(object)
}
\arguments{
  \item{object}{ a string giving the name of a sequence presents in the data base}
  \item{length}{ a string giving the length of the sequence presents in the data base}
  \item{frame}{ a string giving the frame of the sequence presents in the data base}
  \item{ncbigc}{ a string giving the ncbi genetic code of the sequence presents in the data base}
  \item{socket}{ an object of class \code{socket}}
}
\value{
  \code{as.SeqAcnucWeb} returns an object sequence of class \code{SeqAcnucWeb}
}
\references{ 
  \code{citation("seqinr")}
}
\author{ D. Charif }
\examples{
  \dontrun{s = choosebank.socket("genbank")}
  \dontrun{query.socket(s$socket,"felis","sp=felis catus et t=cds et o=mitochondrion")}
  \dontrun{is.SeqAcnucWeb(felis$req[[1]])}
}
\keyword{utilities}
