\name{choosebank}
\alias{choosebank}
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
choosebank(bank = NA, host = "pbil.univ-lyon1.fr", port = 5558, server = FALSE,
                    blocking = TRUE, open = "a+", encoding = "", verbose = FALSE,
                    timeout = 5, infobank = FALSE, tagbank = NA)
}
\arguments{
  \item{bank}{string. The name of the bank. If NA, \code{choosebank} will return the names of all database known by the server.}
  \item{host}{string. Host name for port (see \code{\link{socketConnection}})}
  \item{port}{integer. The TCP port number (see \code{\link{socketConnection}})}
  \item{server}{logical. Should the socket be a client or a server? (see \code{\link{socketConnection}})}
  \item{blocking}{logical. (see \code{\link{socketConnection}})}
  \item{open}{string. A description of how to open the connection (see \code{\link{socketConnection}})}
  \item{encoding}{string. The name of the encoding to be used. (see \code{\link{socketConnection}})}
  \item{verbose}{logical. If TRUE, verbose mode is on}
  \item{timeout}{integer. The timeout in seconds for \code{socketConnection}. Default 5 seconds.}
  \item{infobank}{logical. If \code{infobank} is TRUE and \code{bank} is \code{NA}, a data.frame 
                   with all database informations will be returned}
  \item{tagbank}{string. If \code{bank} is \code{NA} and \code{tagbank} is documented, the names
                  of special purposes databases are returned. Current allowed values are TP
                  for frozen databases (TP is an acronym for "travaux pratiques" which means practicals
                  in french, these databases are useful mainly for teaching so as to have stable results),
                  TEST for test databases, and DEV for databases under development (unstable).}
}
\details{
  When called without arguments, \code{choosebank()} returns a list of all the databases names known 
  by the server, as a vector of string. When called with \code{choosebank(infobank = TRUE)}, a data.frame
  with more information is returned.
}
\value{
When called with a regular bank name, an (invisible) list with 6 components:

  \item{socket}{ an object of class \code{socket} }
  \item{bankname}{ the name of the bank }
  \item{banktype}{ the type of the bank (GENBANK, EMBL, SWISSPROT, NBRF)}
  \item{totseqs}{ the total number of sequences present in the opened database }
  \item{totspecs}{ the total number of species present in the opened database }
  \item{totkeys}{ the total number of keywords present in the opened database }	
 
When called with bank = NA:
  
  \item{ }{ A vector of all available bank names.}

When called with bank = NA and infobank = TRUE, a data.frame with three columns:

  \item{bank}{ The name of the bank. }
  \item{status}{ The bank status (on/of). }
  \item{info}{ Short description of bank with last release date. } 

}
\references{ 
For more information about the socket communication protocol with ACNUC please get at \url{http://doua.prabi.fr/databases/acnuc/remote_acnuc.html}.
To get the release date and content of all the databases located at the pbil, please look at the following url: \url{http://doua.prabi.fr/search/releases}\cr
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
\note{
The invisible list returned when a database is opened is stored in the variable
\code{banknameSocket} in the global environment. 
}
\author{D. Charif, J.R. Lobry}
\seealso{\code{\link{where.is.this.acc}} if you have a sequence accession number but you
don't know which database to open, \code{\link{query}} to make a query when a database
is opened, \code{\link{connection}}, \code{\link{socketConnection}} }
\examples{
  \dontrun{# Need internet connection
  # Show available databases:  
  choosebank()
  # Show frozen databases:
  choosebank(tag = "TP")
  # Select a database:
  choosebank("emblTP", tag = "TP") 
  # Do something with the database:
  myseq <- gfrag("LMFLCHR36", start = 1, length = 30)
  stopifnot(myseq == "cgcgtgctggcggcaatgaagcgttcgatg")
  # Close the database:
  closebank()}
}
\keyword{ manip }
