\name{gb2fasta}
\alias{gb2fasta}
\title{ conversion of GenBank file into fasta file }
\description{
Converts a single entry in GenBank format into a fasta file.
}
\usage{
gb2fasta(source.file =
"ftp://ftp.ncbi.nih.gov/genomes/Bacteria/Agrobacterium_tumefaciens_C58_Cereon/NC_003065.gbk",
destination.file = "Agrobacterium_tumefaciens_C58_Cereon.fasta")
}
\arguments{
  \item{source.file}{ GenBank file }
  \item{destination.file}{ Fasta file }
}
\details{
Multiple entries in GenBank file are not supported.
}
\value{
  none
}
\references{
  \code{citation("seqinr")}
}
\author{J.R. Lobry }
\seealso{ \code{\link{oriloc}} }
\examples{
\dontrun{gb2fasta()}
}
\keyword{utilities}
