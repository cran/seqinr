\name{where.is.this.acc}
\alias{where.is.this.acc}
\title{Scans databases for a given sequence accession number}
\description{
This function loops over all availabale ACNUC databases to look for
a given sequence accession number. This is useful when you have
a sequence accession number and you don't know in which database
it is present. 
}
\usage{
where.is.this.acc(acc,  stopAtFirst = TRUE, ...)
}
\arguments{
  \item{acc}{An accession number as a string of characters such as \code{"NC_001416"}.}
  \item{stopAtFirst}{Logical. If TRUE, the default, the function stops at the first 
     database where the accession number is found.}
  \item{\dots}{Arguments passed to the function \code{\link{choosebank}}.}
}
\value{
The function resturns invisibly a vector of strings of characters for the names of the ACNUC 
databases in which the accession number was found.
}
\references{\code{citation("seqinr")}}
\author{J.R. Lobry}

\seealso{\code{\link{choosebank}} to open a given ACNUC database.}
\examples{
  \dontrun{# Need internet connection
  where.is.this.acc("NC_001416") # first found in phever2dna bank (2016-06-01)
  }
}
