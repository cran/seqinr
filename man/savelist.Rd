\name{savelist}
\alias{savelist}
\title{Save sequence names or accession numbers into a file}
\description{
This function retrieves all sequence names or all accession number from
an ACNUC list and saves them into a file. 
}
\usage{
savelist(lrank, type = c("N", "A"),
                     filename = paste(gln(lrank), ifelse(type == "N", "mne", "acc"),
		     sep = "."),socket = autosocket(), warnme = TRUE)
}
\arguments{
  \item{lrank}{the rank of the ACNUC list to consider.}
  \item{type}{use "N" for sequence names (mnemonics) and "A" for accession numbers.
     Default is "N".}
  \item{filename}{a string of character giving the name of the file to save results.}
  \item{socket}{an object of class \code{sockconn} connecting to a remote ACNUC
                        database (default is a socket to the last opened database).}
  \item{warnme}{if TRUE a message is issued on the console when complete.}
}

\value{
none.
}
\references{ \url{http://doua.prabi.fr/databases/acnuc.html}

\code{citation("seqinr")}
}
\author{J.R. Lobry}
\seealso{\code{\link{choosebank}}, \code{\link{query}}, \code{\link{glr}} to
get a list rank from its name, \code{\link{clfcd}} for the inverse operation
of \code{savelist}}
\examples{
\dontrun{ 
  ### Need internet connection
  choosebank("emblTP")
  mylist <- query("mylist", "sp=felis catus et t=cds", virtual=TRUE)
  savelist(glr("mylist"))
  # 603 sequence mnemonics written into file: MYLIST.mne 
  savelist(glr("mylist"), type = "A")
  # 603 sequence accession numbers written into file: MYLIST.acc
  }
}
\keyword{ utilities }
