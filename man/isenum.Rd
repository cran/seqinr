\name{isenum}
\alias{isenum}
\alias{isn}
\alias{getNumber.socket}
\alias{getAttributsocket}
\title{Get the ACNUC number of a sequence from its name or accession number}
\description{
Gives the ACNUC number of a sequence in the \code{number} element of the returned list.
More informations are returned for subsequences corresponding to coding sequences.
}
\usage{
isenum(what, idby = c("name", "access"), socket = autosocket())
isn(what, ...)
getNumber.socket(socket, name)
getAttributsocket(socket, name)
}
\arguments{
  \item{what}{a sequence name or a sequence accession number}
  \item{idby}{is the sequence identified by name or by accession number? Default to name}
  \item{socket}{an object of class \code{sockconn} connecting to a remote ACNUC
                        database (default is a socket to the last opened database).}
  \item{...}{arguments passed to \code{isenum}.}
  \item{name}{a sequence name.}
}
\value{
A list whith the following 6 components:

  \item{number}{numeric. The ACNUC number of the sequence.}
  \item{length}{numeric. The length of the sequence.}
  \item{frame}{numeric. The reading frame (0, 1, or 2) of the sequence for CDS.} 
  \item{gencode}{numeric. ACNUC's genetic code (0 means universal) of the sequence for CDS.}
  \item{ncbigc}{numeric. NCBI's genetic code (0 means universal) of the sequence for CDS.}
  \item{otheraccessmatches}{logical. If TRUE it means that several sequences are attached 
    to the given accession nunmber, and that only the ACNUC number of the first attached 
   sequence is returned in the \code{number} component of the list.}

\code{isn(what, ...)} is a shortcut for \code{isenum(what, ...)$number}.\cr

As from seqinR 1.1-3 \code{getNumber.socket} and
\code{getAttributsocket} are deprecated (a warning is issued). 
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
  isenum("LMFLCHR36")
  isn("LMFLCHR36")
  stopifnot(isn("LMFLCHR36") == 13682678)
  # Example with CDS:
  isenum("AB004237")
  }
}
\keyword{ utilities }
