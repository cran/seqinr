\name{get.db.growth}
\alias{get.db.growth}
\alias{dia.db.growth}
\title{ Get the exponential growth of nucleic acid database content }
\description{
  Connects to the embl database to read the last release note about
the number of nucleotides in the DDBJ/EMBL/Genbank database content.
A log-linear fit is represented by dia.bd.gowth() with an estimate of
the doubling time in months.
}
\usage{
get.db.growth(
where = "ftp://ftp.ebi.ac.uk/pub/databases/embl/doc/relnotes.txt")
dia.db.growth( get.db.growth.out = get.db.growth(), Moore = TRUE, ... )
}
\arguments{
  \item{where}{ the file containig the database growth table.}
  \item{get.db.growth.out}{ the output from get.db.growth() }
  \item{Moore}{ logical, if TRUE add lines corresponding to an
exponential growth rate with a doubling time of 18 months, that
is Moore's law.}
  \item{...}{further arguments to plot}
}
\value{
  A dataframe with the statistics from the embl site.
}
\references{\url{http://www.ebi.ac.uk/embl/Documentation/Release_notes/current/relnotes.txt}

\code{citation("seqinr")}
}
\author{J.R. Lobry}
\examples{
\dontrun{data <- get.db.growth()}
\dontrun{dia.db.growth(data)}
}
\keyword{utilities}
