\name{stutterabif}
\Rdversion{1.1}
\alias{stutterabif}
\title{Stutter ratio estimation}
\description{
This function tries to estimate the stutter ratio, either in terms of peak heigth ratios or peak
surface ratio.
}
\usage{
stutterabif(abifdata, chanel, poswild, datapointbefore = 70,
 datapointafter = 20, datapointsigma = 3.5,
 chanel.names = c(1:4, 105), DATA = paste("DATA", chanel.names[chanel], sep = "."),
 maxrfu = 1000, method = "monoH.FC", pms = 6, fig = FALSE)
}
\arguments{
  \item{abifdata}{the result returned by \code{\link{read.abif}}}
  \item{chanel}{the dye number}
  \item{poswild}{the position in datapoint units of the allele at 
    the origin of the stutter product, typically obtained after a call to \code{\link{peakabif}}}
  \item{datapointbefore}{how many datapoints before \code{poswild} to be include in analysis}
  \item{datapointafter}{how many datapoints after \code{poswild} to be include in analysis}
  \item{datapointsigma}{initial guess for the standard deviation of a peak}
  \item{chanel.names}{numbers extensions used for the DATA}
  \item{DATA}{names of the DATA components}
  \item{maxrfu}{argument passed to \code{\link{baselineabif}}}
  \item{method}{method to be used by \code{\link{splinefun}}}
  \item{pms}{how many standard deviations (after gaussian fit) before and after the mean 
  peak values should be considered for spline function interpolation}
  \item{fig}{should a summary plot be produced?}
}

\details{FIXME, See R code for now}

\value{
A list with the following components:
\item{rh}{Stutter ratio computed as the height of the stutter divided by 
 the height of its corresponding allele}
\item{rs}{Stutter ratio computed as the surface of the stutter divided by
 the surface of its corresponding allele}
\item{h1}{The height of the stutter with baseline at 0}
\item{h2}{The height of the allele with baseline at 0}
\item{s1}{The surface of the stutter}
\item{s2}{The surface of the allele}
\item{p}{A list of additional parameter that could be usesfull, see example}
}

\author{J.R. Lobry}

\seealso{\code{\link{JLO}} for a dataset example, 
\code{\link{peakabif}} to get an estimate of peak location.}
\examples{
  #
  # Load pre-defined dataset, same as what would be obtained with read.abif:
  #

data(JLO)

  #
  # Get peak locations in the blue channel:
  #

maxis <- peakabif(JLO, 1, npeak = 6, tmin = 3, fig = FALSE)$maxis

  #
  # Compute stutter ratio for first peak and ask for a figure:
  #

tmp <- stutterabif(JLO, 1, maxis[1], fig = TRUE)

  #
  # Show in addition the normal approximation used at the stutter peak:
  #

xx <- seq(tmp$p$mu1 - 6*tmp$p$sd1, tmp$p$mu1 + 6*tmp$p$sd1, le = 100)
lines(xx, tmp$p$p1*dnorm(xx, tmp$p$mu1, tmp$p$sd1), col = "darkgreen")

  #
  # Show in addition the normal approximation used at allele peak:
  #

xx <- seq(tmp$p$mu2 - 6*tmp$p$sd2, tmp$p$mu2 + 6*tmp$p$sd2, le = 100)
lines(xx, tmp$p$p2*dnorm(xx, tmp$p$mu2, tmp$p$sd2), col = "darkgreen")
}
