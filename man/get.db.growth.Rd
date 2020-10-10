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

\details{
This is a screenshot from fig. 1 in Lobry (2004):

\if{html}{\figure{lncs2004.pdf}{options: width=400}}
\if{latex}{\figure{lncs2004.pdf}{options: width=12cm}}

At that time the doubling time was 16.9 months. This is an
update in 2016 from release 3.1-5 of the seqinr tutorial
\url{https://seqinr.r-forge.r-project.org/seqinr_3_1-5.pdf}:

\if{html}{\figure{introduction-dbg.pdf}{options: width=400}}
\if{latex}{\figure{introduction-dbg.pdf}{options: width=12cm}}

The doubling time was 18.8 monts in this update. The fit to
Moore's law is still striking over such a long period.

}
\references{
\url{https://www.ebi.ac.uk/ena/browser}

Lobry, J.R. (2004) Life History Traits and Genome Structure:
Aerobiosis and G+C Content in Bacteria. \emph{Lectures Notes in Computer Sciences},
\bold{3039}:679-686.

\code{citation("seqinr")}
}
\author{J.R. Lobry}
\examples{
\dontrun{
  ### Need internet connection
  data <- get.db.growth()
  dia.db.growth(data)
}}
\keyword{utilities}
