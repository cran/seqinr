\name{dinucl}
\alias{dinucl}
\docType{data}
\title{ Mean zscore on 242 complete bacterial chromosomes }
\description{
This dataset contains the mean zscores as computed on all intergenic
sequences (intergenic) and on all CDS (coding) from 242
complete bacterial chromosomes (as retrieved from Genome Reviews database on
June 16, 2005).
}
\usage{
data(dinucl)
}
\format{
  List of two dataframes of 242 chromosomes and 16 dinucleotides: one
  for intergenic, one for coding sequences.
\describe{
  \item{intergenic}{the mean of \code{zscore} computed with the \code{base}
   model on each intergenic sequence}
   \item{coding}{the mean of \code{zscore} computed with the \code{codon}
  model on each coding sequence}
}
}
\references{
  Palmeira, L., Guéguen, L. and Lobry JR. (2006) UV-targeted dinucleotides
  are not depleted in light-exposed Prokaryotic genomes.
  \emph{Molecular Biology and Evolution},
  \bold{23}:2214-2219.\cr
  \url{http://mbe.oxfordjournals.org/cgi/reprint/23/11/2214}\cr\cr

  \code{citation("seqinr")}
}
\seealso{ \code{\link{zscore}} }
\examples{
data(dinucl)
}
\keyword{ datasets }
