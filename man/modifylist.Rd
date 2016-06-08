\name{modifylist}
\alias{modifylist}
\title{Modification of an ACNUC list}
\description{
This function modifies a previously existing ACNUC list by selecting sequences
either by length, either by date, either for the presence of a given string in annotations. 
}
\usage{
modifylist(listname, modlistname = listname, operation, 
 type = c("length", "date", "scan"), socket = autosocket(), 
 virtual = FALSE, verbose = FALSE)
}
\arguments{
  \item{listname}{the name of the ACNUC list to modify}
  \item{modlistname}{the name of the modified ACNUC list. Default is to use the
       same list name so that previous list is lost.}
  \item{operation}{a string of character describing the operation to be done, see details.}
  \item{type}{the type of operation, could be one of \code{ "length", "date", "scan"}.
        Default is \code{"length"}}
  \item{socket}{an object of class \code{sockconn} connecting to a remote ACNUC
                        database (default is a socket to the last opened database).}
  \item{virtual}{if TRUE, no attempt is made to retrieve the information about all the elements of the list. In this case, the \code{req} component of the list is set to NA.}
  \item{verbose}{logical, if TRUE mode verbose is on}
}

\details{
Example of possible values for the argument \code{operation}: 

  \describe{
  \item{length}{as in  "> 10000"    or    "< 500"}
  \item{date}{as in   "> 1/jul/2001"   or   "< 30/AUG/98"}
  \item{scan}{specify the string to be searched for}
  }
Character < is to be understood as <= and > likewise.
}

\value{
The result is directly assigned to the object \code{modlistname} in the user workspace.
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
\references{ \url{http://doua.prabi.fr/databases/acnuc.html}

\code{citation("seqinr")}
}
\author{J.R. Lobry}
\seealso{\code{\link{choosebank}}, \code{\link{query}} and
  \code{\link{prepgetannots}} to select the annotation lines for scan.}
\examples{
\dontrun{ # Need internet connection
  choosebank("emblTP")
  mylist <- query("mylist", "sp=felis catus et t=cds", virtual=TRUE)
  mylist$nelem # 603 sequences
  stopifnot(mylist$nelem == 603)

  # select sequences with at least 1000 bp:
  mylist <- modifylist("mylist", operation = ">1000", virtual = TRUE)
  mylist$nelem # now, only 132 sequences
  stopifnot(mylist$nelem == 132)

  # scan for "felis" in annotations:
  mylist <- modifylist("mylist", op = "felis", type = "scan", virtual = TRUE)
  mylist$nelem # now, only 33 sequences
  stopifnot(mylist$nelem == 33)

  # modify by date:
  mylist <-  modifylist("mylist", op = "> 1/jul/2001", type = "date", virtual = TRUE)
  mylist$nelem # now, only 15 sequences
  stopifnot(mylist$nelem == 15)

  # Summary of current ACNUC lists, one list called MYLIST on sever:
  sapply(alr()$rank, getliststate)
  closebank()
  }
}
\keyword{ utilities }
