\name{get.ncbi}
\alias{get.ncbi}
\alias{ncbi.fna.url}
\alias{ncbi.ptt.url}
\alias{ncbi.stats}
\alias{ncbi.gbk.url}
\title{ Bacterial complete genome data from ncbi ftp site }
\description{
Try to connect to ncbi ftp site to get a list of complete bacterial genomes.
}
\usage{
get.ncbi(repository = "ftp://ftp.ncbi.nih.gov/genomes/Bacteria/")
}
\arguments{
  \item{repository}{ Where to look for data. The default value is the location of the complete bacterial genome sequences at ncbi ftp repository. }
}
\value{
Returns a data frame which contains the following columns:
    \item{species}{The species name as given by the corresponding folder
    name in the repository (\emph{e.g.} Yersinia\_pestis\_KIM).}
    \item{accession}{The accession number as given by the common prefix
    of file names in the repository (\emph{e.g.} NC\_004088).}
    \item{size.bp}{The size of the sequence in bp (\emph{e.g.} 4600755).}
    \item{type}{A factor with two levels (plasmid or chromosome) temptatively
    deduced from the description of the sequence.}
}
\references{
  \code{citation("seqinr")}
}
\author{J.R. Lobry}
\section{WARNING }{
This function is highly dependant on ncbi ftp site conventions
for which we have no control.
The ftp connection apparently does not work when there is a proxy,
this problem is circumvented here in a rather crude way.
}
\examples{
\dontrun{bacteria <- get.ncbi()}
\dontrun{summary(bacteria)}
}
\keyword{utilities}
