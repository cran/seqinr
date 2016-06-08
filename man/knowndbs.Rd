\name{knowndbs}
\alias{knowndbs}
\alias{kdb}
\title{Description of databases known by an ACNUC server}
\description{
Returns, for each database known by the server, its name (a valid value for the \code{bank} 
argument of \code{\link{choosebank}}), availability (off means temporarily unavailable), 
and description.
}
\usage{
knowndbs(tag = c(NA, "TP", "TEST", "DEV"), socket = autosocket())
kdb(tag = c(NA, "TP", "TEST", "DEV"), socket = autosocket())
}
\arguments{
  \item{tag}{default to NA, see details}
  \item{socket}{an object of class \code{sockconn} connecting to a remote ACNUC
                        database (default is a socket to the last opened database).}
}

\details{
When the optional \code{tag}  argument is used, only databases tagged with the given 
string are listed;
when this argument is NA (by default), only untagged databases are listed.
The \code{tag} argument thus allows to identify series of special purpose (tagged) databases, 
in addition to default (untagged) ones.
}

\value{
A dataframe with 3 columns:
\item{bank}{string. Valid bank values known by the ACNUC server}
\item{status}{string. "on" means available, "off" means temporarily unavailable}
\item{info}{string. short description of the database}
}
\references{ \url{http://doua.prabi.fr/databases/acnuc.html}

\code{citation("seqinr")}

The full list of untagged and tagged databases is here : 
\url{http://doua.prabi.fr/databases/acnuc/banques_raa.php}.

}
\author{J.R. Lobry}
\seealso{\code{\link{choosebank}} when called without arguments. }
\examples{
\dontrun{ 
  ### Need internet connection
  choosebank("emblTP")
  kdb()
  closebank()
  }
}
\keyword{ utilities }
