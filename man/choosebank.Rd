\name{choosebank}
\alias{choosebank}
\title{To select a database structured under ACNUC and located on the web}
\description{
  This function allows to select one of the databases structured under ACNUC and located on the web. Then, you can use \code{query} to compose your query and get a list of sequence names. Remote access to ACNUC databases work
by opening a socket on a port (as an example on port number 5558 of pbil.univ-lyon1.fr) and by communicating on this socket following the protocol described in the section \code{references}.   
}
\usage{
choosebank(bank = NA, host = "pbil.univ-lyon1.fr", port = 5558))
}
\arguments{
  \item{bank}{ The name of the bank. If NA, it returns the name of all database known by the server.}
  \item{host}{ Host name for port}
  \item{port}{ integer. The TCP port number}
}
\details{
  \code{choosebank()} returns a list of all the databases located at the Pbil. See the reference section for more details. 
  ACNUC databases are made of a series of flat text files (called divisions) containing annotations and sequences and a series of index files allowing efficient access to sequence data. 
}
\value{
A list with five components
  \item{socket}{ an object of class \code{socket} }
  \item{bankname}{ the name of the bank }
  \item{totseqs}{ the total number of sequences present in the opened database }
  \item{totspecs}{ the total number of species present in the opened database }
  \item{totkeys}{ the total number of keywords present in the opened database }	
}
\references{ 
For further information about socket, please see \code{connection} function. For more information about the socket communication protocol with ACNUC please get at \url{http://pbil.univ-lyon1.fr/databases/acnuc/remote_acnuc.html}.
To get the release date and content of all the databases located at the pbil, please look at the following url: \url{http://pbil.univ-lyon1.fr/search/releases.php}\cr
Gouy, M., Milleret, F., Mugnier, C., Jacobzone, M., Gautier,C. (1984) ACNUC: a nucleic acid sequence data base and analysis system. 
\emph{Nucl. Acids Res.}, \bold{12}:121-127.\cr
Gouy, M., Gautier, C., Attimonelli, M., Lanave, C., Di Paola, G. (1985) 
ACNUC - a portable retrieval system for nucleic acid sequence databases:
logical and physical designs and usage.
\emph{Comput. Appl. Biosci.}, \bold{3}:167-172.\cr
Gouy, M., Gautier, C., Milleret, F. (1985) System analysis and nucleic acid sequence banks.
\emph{Biochimie}, \bold{67}:433-436.\cr

To have an overview of the seqinR's functionnality, please consult this vignette: 
Charif, D., Lobry, J.R. (2005) SeqinR: a contributed package to the R project for statistical
computing devoted to biological sequences retrieval and analysis. Springer Verlag, \emph{Biological and Medical Physics/Biomedical Series}, in preparation. \cr
}
\note{ The object of class \code{socket} will be the first argument of the \code{query} function.}
\author{ D. Charif }
\seealso{ \code{\link{query}}, \code{\link{connection}} }
\examples{
  \dontrun{ mybank = choosebank() }
  \dontrun{ choosebank(mybank[1]) }
}
\keyword{ manip }
