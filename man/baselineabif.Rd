\name{baselineabif}
\Rdversion{1.1}
\alias{baselineabif}
\title{Estimation of baseline value}
\description{
This function tries to estimate the baseline value for RFU data from
capillary electrophoresis whith the heuristic that the most common
value is the baseline.
}
\usage{
baselineabif(rfu, maxrfu = 1000)
}
\arguments{
  \item{rfu}{a numeric vector of signal value}
  \item{maxrfu}{signal values greater or equal to maxrfu are forced to NA}
}
\value{A single numeric value for the estimated baseline.}

\author{J.R. Lobry}

\seealso{
\code{\link{JLO}} for a dataset example, \code{\link{plotabif}} to plot this kind of data, 
\code{\link{peakabif}} to estimate peak parameters.
}

\examples{
data(JLO)
rfu <- JLO$Data$DATA.1
bl <- baselineabif(rfu)
plot(1:length(rfu), rfu, type = "l", 
  xlab = "Time [datapoint units]",
  ylab = "Signal [RFU]",
  main = "Example of baseline estimates")
abline(h = bl, col="red", lty = 2)
legend("topright", inset = 0.02, "Baseline estimate", lty = 2, col = "red")
}
