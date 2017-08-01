\name{kaksTorture}
\alias{kaksTorture}
\docType{data}
\title{Expected numeric results for Ka and Ks in extreme cases}
\description{
This data set is what should be obtained when runing \code{kaks()}
on the test file kaks-torture.fasta in the sequences directory of the
seqinR package.
}
\usage{data(kaksTorture)}
\format{
  A list with 4 components of class dist.
  \describe{
    \item{ka}{Ka}
    \item{ks}{Ks}
    \item{vka}{variance for Ka}
    \item{vks}{variance for Ks}
  }
}
\source{
See comments in kaks-torture.fasta for R code used to produce it.
}
\references{
\code{citation("seqinr")}
}
\examples{
data(kaksTorture)
kaks.torture <- read.alignment(file = system.file("sequences/kaks-torture.fasta", 
  package = "seqinr"), format = "fasta")
#
# Failed on windows :
#
# stopifnot(identical(kaksTorture, kaks(kaks.torture)))
# stopifnot(identical(kaksTorture, kaks(kaks.torture, rmgap = FALSE)))
}
\keyword{datasets}
