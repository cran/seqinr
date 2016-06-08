\name{readsmj}
\alias{readsmj}
\title{Low level function to read ACNUC SMJYT index files}
\description{
 Extract informations from the SMJYT index file for status, molecule, journal, 
 year, type, organelle, division, and db structure information.
}
\usage{
readsmj(socket = autosocket(), num = 2, nl = 10, recnum.add = FALSE, nature.add = TRUE,
plong.add = FALSE, libel.add = FALSE, sname.add = FALSE, all.add = FALSE)
}
\arguments{
  \item{socket}{an object of class \code{sockconn} connecting to a remote ACNUC
                        database (default is a socket to the last opened database).}
  \item{num}{rank number of first record.}
  \item{nl}{number of records to read.}
  \item{recnum.add}{to extract record numbers.}
  \item{nature.add}{to extract as a factor with human understandable levels 
      the nature of the name. Unordered levels are: status, molecule, journal,
      year, type, organelle, division and dbstrucinfo.}
  \item{plong.add}{to extract the plong.}
  \item{libel.add}{to extract the label of the name.}
  \item{sname.add}{to extract the short version of the name, that is without
                   the first two characters.}
  \item{all.add}{to extract all (all flags set to TRUE).}
}

\value{
  A data.frame with requested columns.
}
\references{
  See ACNUC physical structure at:
  \url{http://doua.prabi.fr/databases/acnuc/structure.html}.\cr
  
  \code{citation("seqinr")}
}
\author{J.R. Lobry}
\seealso{ \code{\link{choosebank}} to start a session and 
\code{\link{readfirstrec}} to get the total number of records.}
\keyword{utilities}
