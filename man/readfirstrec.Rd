\name{readfirstrec}
\alias{readfirstrec}
\title{Low level function to get the record count of the specified ACNUC 
index file}
\description{
 Called without arguments, the list of available values for argument type is
 returned.
}
\usage{
readfirstrec(socket = autosocket(), type)
}
\arguments{
  \item{socket}{an object of class \code{sockconn} connecting to a remote ACNUC
                        database (default is a socket to the last opened database).}
  \item{type}{the ACNUC index file}
}
\details{
Available index files are:

  \describe{
	\item{AUT}{AUTHOR one record for each author name (last name only, no initials)}
	\item{BIB}{BIBLIO one record for each reference}
	\item{ACC}{ACCESS one record for each accession number}
	\item{SMJ}{SMJYT one record for each status, molecule, journal, year, type, 
			   organelle, division, and db structure information}
	\item{SUB}{SUBSEQ one record for each parent or sub-sequence}
	\item{LOC}{LOCUS one record for each parent sequence}
	\item{KEY}{KEYWORDS one record for each keyword}
	\item{SPEC}{SPECIES one record for each taxon}
	\item{SHRT}{SHORTL mostly, one record for each element of a short list}
	\item{LNG}{LONGL one record for each group of SUBINLNG elements of a long list}
	\item{EXT}{EXTRACT (for nucleotide databases only) one record for each exon of each subsequence}
	\item{TXT}{TEXT one lrtxt-character record for each label of a species, keyword, or SMJYT}
  }
}
\value{
  The record count of ACNUC index file, or NA if missing (typically when 
  asking for type = EXT on a protein database).
}
\references{
  See ACNUC physical structure at 
  \url{http://doua.prabi.fr/databases/acnuc/structure.html}.\cr
  
  \code{citation("seqinr")}
}
\author{J.R. Lobry}
\seealso{ \code{\link{choosebank}} }
\examples{
\dontrun{
# Need internet connection
  choosebank("genbank")
  allowedtype <- readfirstrec()
  sapply(allowedtype, function(x) readfirstrec(type = x))
  }
}
\keyword{utilities}
