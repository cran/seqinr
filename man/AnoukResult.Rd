\name{AnoukResult}
\alias{AnoukResult}
\docType{data}
\title{Expected numeric results for Ka and Ks computation}
\description{
This data set is what should be obtained when runing \code{kaks()}
on the test file Anouk.fasta in the sequences directory of the
seqinR package.
}
\usage{data(AnoukResult)}
\format{
  A list with 4 components of class dist.
  \describe{
    \item{ka}{Ka}
    \item{ks}{Ks}
    \item{vka}{variance for Ka}
    \item{vks}{variance for Ks}
  }
}
\details{
See the example in \code{\link{kaks}}.
}
\source{
The fasta test file was provided by Anamaria Necşulea.
}
\references{
\code{citation("seqinr")}
}
\keyword{datasets}
