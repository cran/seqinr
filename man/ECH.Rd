\name{ECH}
\alias{ECH}
\docType{data}
\title{Forensic Genetic Profile Allelic Ladder Raw Data}
\description{
  This is an example of allelic ladder raw data for a human STR genetic profile at 16 loci
  (\emph{viz.} D8S1179, D21S11, D7S820, CSF1PO, D3S1358, TH01, D13S317, D16S539,
  D2S1338, D19S433, vWA, TPOX, D18S51, Amelogenin, D5S818, FGA) which
  are commonly used in forensic sciences for individual identifications.
}
\usage{data(ECH)}
\format{
  A list with 3 components as in \code{\link{JLO}}
}
\source{
  Data were kindly provided by the INPS (Institut National de Police Scientifique)
  which is the national forensic sciences institute in France. Experiments
  were done at the LPS (Laboratoire de Police Scientifique de Lyon) in 2008.
}
\references{
  \code{citation("seqinr")}

  Anonymous (2006) Applied Biosystem Genetic Analysis Data File Format.
  Available at \url{https://www.thermofisher.com/at/en/home/brands/applied-biosystems.html}.
  Last visited on 03-NOV-2008.
}
\author{J.R. Lobry}
\seealso{
  function \code{\link{read.abif}} to import files in ABIF format, 
  data \code{\link{gs500liz}} for internal size standards,
  data \code{\link{identifiler}} for allele names in the allelic ladder,
  data \code{\link{JLO}} for an example of an individual sample file.
}
\examples{
data(JLO)
}

