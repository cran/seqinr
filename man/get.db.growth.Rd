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
get.db.growth(where = "http://www.ebi.ac.uk/embl/Documentation/Release_notes/current/relnotes.txt" )
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
To have an overview of the seqinR's functionnality, please consult this vignette: 
Charif, D., Lobry, J.R. (2005) SeqinR: a contributed package to the R project for statistical
computing devoted to biological sequences retrieval and analysis. Springer Verlag, \emph{Biological and Medical Physics/Biomedical Series}, in preparation. \cr
 }
\author{ J.R. Lobry }
\examples{
\dontrun{data <- get.db.growth()}
\dontrun{dia.db.growth(data)}
}
\keyword{utilities}
