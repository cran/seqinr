\name{bma}
\alias{bma}
\title{Computing an IUPAC nucleotide symbol}
\description{
  This function returns the IUPAC symbol for a nucleotide sequence, for instance
 \code{c("c", "c", "g")} is coded by \code{"s"}.
}
\usage{
bma(nucl, warn.non.IUPAC = TRUE, type = c("DNA", "RNA"))
}
\arguments{
  \item{nucl}{a nucleotide sequence as a vector of single chars}
  \item{warn.non.IUPAC}{if TRUE warns when no IUPAC symbol is possible}
  \item{type}{whether this is a DNA or a RNA sequence}
}
\details{
  The sequence is forced in lower case letters and ambiguous bases
  are expanded before trying to find an IUPAC symbol.
}
\value{
  A single IUPAC symbol in lower case, or NA when this is not possible.
}
\references{ 

The nomenclature for incompletely specified bases in nucleic acid sequences
at: \url{https://www.ncbi.nlm.nih.gov/pmc/articles/PMC341218/}

\code{citation("seqinr")}
}
\author{J.R. Lobry}

\seealso{See \code{\link{amb}} for the reverse operation. 
Use \code{\link{toupper}} to change lower case letters into
upper case letters.}

\examples{

stopifnot(bma(s2c("atatattttata")) == "w")
stopifnot(bma(s2c("gcggcgcgcggc")) == "s")
stopifnot(bma(s2c("ACGT")) == "n")
stopifnot(is.na(bma(s2c("atatttt---tatat")))) # a warning is issued
}
