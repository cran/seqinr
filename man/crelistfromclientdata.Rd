\name{crelistfromclientdata}
\alias{crelistfromclientdata}
\alias{clfcd}
\title{To create on server an ACNUC list from data lines sent by client}
\description{
This function is usefull if you have a local file with sequence names
(sequence ID), or sequence accession numbers, or species names, or
keywords. This allows you to create on the server a list with the
corresponding items.
}
\usage{
crelistfromclientdata(listname, file, type,
 socket = autosocket(), invisible = TRUE,
 verbose = FALSE, virtual = FALSE)
clfcd(listname, file, type, socket = autosocket(),
 invisible = TRUE, verbose = FALSE, virtual = FALSE)
}

\arguments{
  \item{listname}{The name of the list as a quoted string of chars}
  \item{file}{The local file name}
  \item{type}{Could be one of "SQ", "AC", "SP", "KW", see examples}
  \item{socket}{an object of class \code{sockconn} connecting to a remote ACNUC
                        database (default is a socket to the last opened database).}
  \item{invisible}{if \code{FALSE}, the result is returned visibly.}
  \item{verbose}{if \code{TRUE}, verbose mode is on}
  \item{virtual}{if \code{TRUE}, no attempt is made to retrieve the information about
    all the elements of the list. In this case, the \code{req} component of the list is set to 
    \code{NA}.}
}
\details{
\code{clfcd} is a shortcut for \code{crelistfromclientdata}.
}

\value{
The result is directly assigned to the object \code{listname} in the user workspace.
This is an objet of class \code{qaw}, a list with the following 6 components:

  \item{call}{the original call}
  \item{name}{the ACNUC list name}
  \item{nelem}{the number of elements (for instance sequences) in the ACNUC list}
  \item{typelist}{the type of the elements of the list. Could be SQ for a list of
    sequence names, KW for a list of keywords, SP for a list of species names.}
  \item{req}{a list of sequence names that fit the required criteria or \code{NA} when
    called with parameter \code{virtual} is \code{TRUE}}
  \item{socket}{the socket connection that was used}
}
\references{ 

\code{citation("seqinr")}
}
\author{J.R. Lobry}
\seealso{
  \code{\link{choosebank}}, 
  \code{\link{query}}, \code{\link{savelist}} for the reverse operation with
an ACNUC list of sequences.
}

\examples{
 \dontrun{ # Need internet connection
 choosebank("emblTP")
 #
 # Example with a file that contains sequence names:
 #
 fileSQ <- system.file("sequences/bb.mne", package = "seqinr")
 listSQ <- crelistfromclientdata("listSQ", file = fileSQ, type = "SQ")
 sapply(listSQ$req, getName)
 #
 # Example with a file that contains sequence accession numbers:
 #
 fileAC <- system.file("sequences/bb.acc", package = "seqinr")
 listAC <- crelistfromclientdata("listAC", file = fileAC, type = "AC")
 sapply(listAC$req, getName) 
 #
 # Example with a file that contains species names:
 #
 fileSP <- system.file("sequences/bb.sp", package = "seqinr")
 listSP <- crelistfromclientdata("listSP", file = fileSP, type = "SP")
 sapply(listSP$req, getName) 
 #
 # Example with a file that contains keywords:
 #
 fileKW <- system.file("sequences/bb.kwd", package = "seqinr")
 listKW <- crelistfromclientdata("listKW", file = fileKW, type = "KW")
 sapply(listKW$req, getName)
 #
 # Summary of ACNUC lists:
 #
 sapply(alr()$rank, getliststate)
 closebank() 
 }
}
\keyword{utilities}
