\name{prettyseq}
\alias{prettyseq}
\title{Text representation of a sequence from an ACNUC server}
\description{
To get a text representation of sequence of rank \code{num} and of its subsequences,
with \code{bpl} bases per line (default = 60), and with optional translation of 
protein-coding subsequences 
}
\usage{
prettyseq(num, bpl = 60, translate = TRUE, socket = autosocket())
}
\arguments{
  \item{num}{rank of the sequence in the ACNUC database}
  \item{bpl}{number of base per line}
  \item{translate}{should coding sequences be translated?}
  \item{socket}{an object of class \code{sockconn} connecting to a remote ACNUC
                        database (default is a socket to the last opened database).}
}
\value{
An invisible vector of string. The output is redirected to the console.
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
  prettyseq(111)
  }
}
\keyword{ utilities }
