\name{choosebank}
\alias{choosebank}
\alias{removeTrailingSpaces}
\title{To select a database structured under ACNUC and located on the web}
\description{
  This function allows to select one of the databases structured under ACNUC and located on the web. 
  Called without arguments, \code{choosebank()}, will return the list of available databases.
  Then, you can use \code{\link{query}} to make your query and get a list of sequence names. 
  Remote access to ACNUC databases works by opening a socket connection on a port (for example 
  on port number 5558 at pbil.univ-lyon1.fr) and by communicating on this socket following the protocol 
  described in the section \code{references}.   
}
\usage{
choosebank(bank = NA, host = "pbil.univ-lyon1.fr", port = 5558, verbose = FALSE,
              timeout = 5, infobank = FALSE, tagbank = NA)
}
\arguments{
  \item{bank}{ String. The name of the bank. If NA, \code{choosebank} will return the names of all database known by the server.}
  \item{host}{ String. Host name for port}
  \item{port}{ Integer. The TCP port number}
  \item{verbose}{ Logical. If TRUE, verbose mode is on}
  \item{timeout}{ Integer. The timeout in seconds for \code{socketConnection}. Default 5 seconds.}
  \item{infobank}{ Logical. If \code{infobank} is TRUE and \code{bank} is \code{NA}, a data.frame 
                   with all database informations will be returned}
  \item{tagbank}{ String. If \code{bank} is \code{NA} and \code{tagbank} is documented, the names
                  of special purposes databases are returned. Current allowed values are TP
                  for frozen databases and TEST for test databases. }
}
\details{
  When called without arguments, \code{choosebank()} returns a list of all the databases names known 
  by the server, as a vector of string. When called with \code{choosebank(infobank = TRUE)}, a data.frame
  with more information is returned.
}
\value{
\item{When called with a regular bank name, an (invisible) list with five components}
  \describe{
  \item{socket}{ an object of class \code{socket} }
  \item{bankname}{ the name of the bank }
  \item{totseqs}{ the total number of sequences present in the opened database }
  \item{totspecs}{ the total number of species present in the opened database }
  \item{totkeys}{ the total number of keywords present in the opened database }	
  }
\item{When called with bank = NA}
  \describe{\item{ }{ A vector of all available bank names.}}
\item{When called with bank = NA and infobank = TRUE, a data.frame with three columns}
  \describe{
  \item{bank}{ The name of the bank. }
  \item{status}{ The bank status (on/of). }
  \item{info}{ Short description of bank with last release date. } 
  }
}
\references{ 
For more information about the socket communication protocol with ACNUC please get at \url{http://pbil.univ-lyon1.fr/databases/acnuc/remote_acnuc.html}.
To get the release date and content of all the databases located at the pbil, please look at the following url: \url{http://pbil.univ-lyon1.fr/search/releases.php}\cr
Gouy, M., Milleret, F., Mugnier, C., Jacobzone, M., Gautier,C. (1984) ACNUC: a nucleic acid sequence data base and analysis system. 
\emph{Nucl. Acids Res.}, \bold{12}:121-127.\cr
Gouy, M., Gautier, C., Attimonelli, M., Lanave, C., Di Paola, G. (1985) 
ACNUC - a portable retrieval system for nucleic acid sequence databases:
logical and physical designs and usage.
\emph{Comput. Appl. Biosci.}, \bold{3}:167-172.\cr
Gouy, M., Gautier, C., Milleret, F. (1985) System analysis and nucleic acid sequence banks.
\emph{Biochimie}, \bold{67}:433-436.\cr

\code{citation("seqinr")}
}
\note{ The object of class \code{socket} will be the first argument of the \code{query} function.}
\author{ D. Charif }
\seealso{ \code{\link{query}}, \code{\link{connection}}, \code{\link{socketConnection}} }
\examples{
  \dontrun{ mybank <- choosebank() }
  \dontrun{ choosebank(mybank[1]) }
}
\keyword{ manip }
