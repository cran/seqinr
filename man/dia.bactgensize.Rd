\name{dia.bactgensize}
\alias{dia.bactgensize}
\title{ Distribution of bacterial genome size from GOLD }
\description{
This function tries to download the last update of the GOLD
(Genomes OnLine Database) to extract bacterial genomes sizes
when available. The histogram and the default density()
output is produced. Optionally, a maximum likelihood estimate
of a superposition of two or three normal distributions is
also represented
}
\usage{
dia.bactgensize(fit = 2, p = 0.5, m1 = 2000, sd1 = 600, m2 = 4500,
       sd2 = 1000, p3 = 0.05, m3 = 9000, sd3 = 1000)
}
\arguments{
  \item{fit}{ integer value. If \code{fit == O} no normal fit
is produced, if \code{fit == 2} try to fit a superposition of
two normal distributions, if \code{fit == 3} try to fit a
superposition of three normal distribution.
             }
  \item{p}{ initial guess for the proportion of the first population. }
  \item{m1}{ initial guess for the mean of the first population. }
  \item{sd1}{ initial guess for the standard deviation of the first population. }
  \item{m2}{ initial guess for the mean of the second population. }
  \item{sd2}{initial guess for the standard deviation of the second population. }
  \item{p3}{ initial guess for the proportion of the third population. }
  \item{m3}{ initial guess for the mean of the third population. }
  \item{sd3}{initial guess for the standard deviation of the second population. }
}
\value{
  An invisible dataframe with three components:
  \item{comp1 }{genus name}
  \item{comp2 }{species names}
  \item{comp3 }{genome size in Kb}
}
\references{ 
To have an overview of the seqinR's functionnality, please consult this vignette: 
Charif, D., Lobry, J.R. (2005) SeqinR: a contributed package to the R project for statistical
computing devoted to biological sequences retrieval and analysis. Springer Verlag, \emph{Biological and Medical Physics/Biomedical Series}, in preparation. \cr
}
\author{ J.R. Lobry }
\seealso{ \code{\link{density}} }
\examples{
  \dontrun{dia.bactgensize()}
}
\keyword{ utilities }
