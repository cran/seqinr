\name{getLocation}
\alias{getLocation}
\alias{getLocation.default}
\alias{getLocation.list}
\alias{getLocation.SeqAcnucWeb}
\alias{getLocation.qaw}
\alias{getLocation.logical}

\title{Generic function to get the location of subsequences on the parent sequence}
\description{
This function works only with subsequences from an ACNUC server.
}
\usage{
getLocation(object, ...)
\method{getLocation}{SeqAcnucWeb}(object, ..., socket = autosocket())
}
\arguments{
  \item{object}{ an object of the class \code{\link{SeqAcnucWeb}}, or a list of them,
    or an object created by \code{\link{query}} }
  \item{socket}{an object of class \code{sockconn} connecting to a remote ACNUC
                        database (default is a socket to the last opened database by \code{\link{choosebank}}).}
  \item{...}{further arguments passed to or from other methods}
}
\value{
  A list giving the positions of the sequence
  on the parent sequence. If the sequence is a subsequence (\emph{e.g.} coding
  sequence), the function returns the position of each exon on the
  parent sequence. NA is returned for parent sequences and a warning
  is isued.}

\references{
  \code{citation("seqinr")}
}
\author{D. Charif, J.R. Lobry, L. Palmeira} 
\seealso{
  \code{\link{SeqAcnucWeb}}
}
\examples{
#
# List all available methods for getLocation generic function:
#
   methods(getLocation)
#
# Example with a subsequence from an ACNUC server:
#
  \dontrun{
  # Need internet connection
  choosebank("emblTP")
  fc <- query("fc", "sp=felis catus et t=cds et o=mitochondrion")
  getLocation(fc$req[[5]])
  closebank()  
}
}
\keyword{ utilities }
