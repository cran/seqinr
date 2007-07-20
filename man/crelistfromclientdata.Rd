\name{crelistfromclientdata}
\alias{crelistfromclientdata}
\alias{clfcd}
\title{To create on server a bitlist from data lines sent by client}
\description{
This function is usefull if you have a local file with sequence names
(sequence ID), or sequence accession numbers, or species names, or
keywords. This allows you to create on the server a list with the
corresponding items.
}
\usage{
crelistfromclientdata(listname, file, type, socket = "auto", invisible = TRUE, verbose = FALSE, virtual = FALSE)
clfcd(listname, file, type, socket = "auto", invisible = TRUE, verbose = FALSE, virtual = FALSE)
}

\arguments{
  \item{listname}{The name of the list as a quoted string of chars}
  \item{file}{The local file name}
  \item{type}{Could be one of "SQ", "AC", "SP", "KW", see examples}
  \item{socket}{a socket of class connection and sockconn returned by \code{choosebank}.Default value (auto) means
   that the socket will be set to to the socket component of the banknameSocket variable. }
  \item{invisible}{if \code{FALSE}, the result is returned visibly.}
  \item{verbose}{if \code{TRUE}, verbose mode is on}
  \item{virtual}{if \code{TRUE}, no attempt is made to retrieve the information about
    all the elements of the list. In this case, the \code{req} component of the list is set to 
    \code{NA}.}
}
\details{

}

\value{
  A list with the following components:
  \item{bank}{the name of the bank that has been choosen by \code{choosebank.socket}}	
  \item{call}{original call}
  \item{name}{list name}
  \item{nelem}{number of elements in the list on the server}
  \item{typelist}{the type of the elemnts of the list. Could be SQ for a list of
    sequence names, KW for a list of keywords, SP for a list of species names.}
  \item{req}{a list of sequence names that fit the required criteria or \code{NA} when
    called with parameter \code{virtual} is \code{TRUE}}
}
\references{ 

\code{citation("seqinr")}
}
\author{J.R. Lobry}
\seealso{
  \code{\link{choosebank}}, 
  \code{\link{query}}
}

\examples{
 \dontrun{
 # Need internet connection
 # Need internet connection
 choosebank("emblTP")
 #
 # Example with a file that contains sequence names:
 #
 fileSQ <- system.file("sequences/bb.mne", package = "seqinr")
 crelistfromclientdata("listSQ", file = fileSQ, type = "SQ")
 sapply(listSQ$req, getName)
 #
 # Example with a file that contains sequence accession numbers:
 #
 fileAC <- system.file("sequences/bb.acc", package = "seqinr")
 crelistfromclientdata("listAC", file = fileAC, type = "AC")
 sapply(listAC$req, getName) 
 #
 # Example with a file that contains species names:
 #
 fileSP <- system.file("sequences/bb.sp", package = "seqinr")
 crelistfromclientdata("listSP", file = fileSP, type = "SP")
 sapply(listSP$req, getName) 
 #
 # Example with a file that contains keywords:
 #
 fileKW <- system.file("sequences/bb.kwd", package = "seqinr")
 crelistfromclientdata("listKW", file = fileKW, type = "KW")
 sapply(listKW$req, getName) 
 }
}
\keyword{utilities}
