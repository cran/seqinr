\name{dia.bactgensize}
\alias{dia.bactgensize}
\title{ Distribution of bacterial genome size from GOLD }
\description{
This function tries to download the last update of the GOLD
(Genomes OnLine Database) to extract bacterial genomes sizes
when available. The histogram and the default density()
output is produced. Optionally, a maximum likelihood estimate
of a superposition of two or three normal distributions is
also represented.
}
\usage{
dia.bactgensize(fit = 2, p = 0.5, m1 = 2000, sd1 = 600, m2 = 4500,
       sd2 = 1000, p3 = 0.05, m3 = 9000, sd3 = 1000, maxgensize = 20000,
       source = c("ftp://pbil.univ-lyon1.fr/pub/seqinr/data/goldtable15Dec07.txt",
    "http://www.genomesonline.org/DBs/goldtable.txt"))
}
\arguments{
  \item{fit}{ integer value. If \code{fit == O} no normal fit
is produced, if \code{fit == 2} try to fit a superposition of
two normal distributions, if \code{fit == 3} try to fit a
superposition of three normal distributions.
             }
  \item{p}{ initial guess for the proportion of the first population. }
  \item{m1}{ initial guess for the mean of the first population. }
  \item{sd1}{ initial guess for the standard deviation of the first population. }
  \item{m2}{ initial guess for the mean of the second population. }
  \item{sd2}{initial guess for the standard deviation of the second population. }
  \item{p3}{ initial guess for the proportion of the third population. }
  \item{m3}{ initial guess for the mean of the third population. }
  \item{sd3}{initial guess for the standard deviation of the third population. }
  \item{maxgensize}{maximum admissive value in bp for a bacterial genome size:
     only value less or equal to this threshold are considrered.}
  \item{source}{ the file with raw data. By default a local (outdated) copy is used.}
}
\value{
  An invisible dataframe with three components:

  \item{genus }{genus name}
  \item{species }{species names}
  \item{gs }{genome size in Kb}
  
}
\references{
Please cite the following references when using data from GOLD:

Kyrpides, N.C. (1999) Genomes OnLine Database (GOLD 1.0): a monitor 
of complete and ongoing genome projects world-wide.
\emph{Bioinformatics}, \bold{15}:773-774.\cr

Bernal, A., Ear, U., Kyrpides, N. (2001) Genomes OnLine Database (GOLD): 
a monitor of genome projects world-wide.
\emph{Nucleic Acids Research}, \bold{29}:126-127.\cr

Liolios, K., Tavernarakis, N., Hugenholtz, P., Kyrpides, N.C. (2006)
The Genomes On Line Database (GOLD) v.2: a monitor of genome
projects worldwide.
\emph{Nucleic Acids Research}, \bold{34}:D332-D334.\cr

Liolios, K., Mavrommatis, K., Tavernarakis, N., Kyrpides, N.C. (2008)
The Genomes On Line Database (GOLD) in 2007: status of genomic 
and metagenomic projects and their associated metadata.
\emph{Nucleic Acids Research},
\bold{in press}:D000-D000.\cr

  \code{citation("seqinr")}
}
\author{J.R. Lobry}
\seealso{ \code{\link{density}} }
\examples{
  \dontrun{# Need internet connection
#
# With a local outdated copy from GOLD:
#
   dia.bactgensize()
#
# With last GOLD data:
#
  # The URL is no more accessible.
  # dia.bactgensize(source = "http://www.genomesonline.org/DBs/goldtable.txt")
  }
}
\keyword{ utilities }
