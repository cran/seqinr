\name{getKeyword}
\alias{getKeyword}
\alias{getKeyword.default}
\alias{getKeyword.list}
\alias{getKeyword.SeqAcnucWeb}
\alias{getKeyword.qaw}
\alias{getKeyword.logical}

\title{Generic function to get keywords associated to sequences}
\description{
Get keywords from an ACNUC server.
}
\usage{
getKeyword(object, ...)
\method{getKeyword}{SeqAcnucWeb}(object, ..., socket = autosocket())
}
\arguments{
  \item{object}{ an object of the class \code{\link{SeqAcnucWeb}}, or
                        a list of them, or the object resulting from \code{\link{query}}}
  \item{socket}{an object of class \code{sockconn} connecting to a remote ACNUC
                        database (default is a socket to the last opened database by \code{\link{choosebank}}).}
  \item{...}{further arguments passed to or from other methods}
}
\value{
  \code{getKeyword} returns a vector of strings containing the keyword(s)
  associated to a sequence.
}
\references{
  \code{citation("seqinr")}
}
\author{D. Charif, J.R. Lobry, L. Palmeira} 
\seealso{
  \code{\link{SeqAcnucWeb}}
}
\examples{
#
# List all available methods for getKeyword generic function:
#
   methods(getKeyword)
#
# Example of keyword extraction from an ACNUC server:
#
  \dontrun{
  # Need internet connection
  choosebank("emblTP")
  fc<-query("fc", "sp=felis catus et t=cds et o=mitochondrion")
  getKeyword(fc$req[[1]])
  # Should be: 
  # [1] "DIVISION ORG" "RELEASE 62"   "CYTOCHROME B" "SOURCE"       "CDS"
  closebank()  
}
}
\keyword{ utilities }
