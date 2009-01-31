\name{plotladder}
\alias{plotladder}
\title{Simple plot of an allelic ladder from ABIF data}
\description{
  Simple representation of an observed allelic ladder.
}
\usage{
plotladder(abifdata, chanel, calibr, allele.names = "identifiler", npeak = NULL, ...)
}
\arguments{
  \item{abifdata}{the result returned by \code{\link{read.abif}}}
  \item{chanel}{the dye number}
  \item{calibr}{a mandatory calibration function to convert time into bp}
  \item{allele.names}{name of the dataset which contains allele names as in \code{link{identifiler}}}
  \item{npeak}{expected number of peaks, deduced from \code{allele.names} by default}
  \item{...}{arguments forwarded to \code{\link{peakabif}}}
}
\value{
  Returns invisibly the location of peaks in bp.
}
\references{
  \code{citation("seqinr")}
}
\author{J.R. Lobry}
\seealso{
  function \code{\link{read.abif}} to import files in ABIF format, 
  \code{\link{plotabif}} to plot them,
  data \code{\link{gs500liz}} for internal size standards,
  data \code{\link{identifiler}} for allele names in the allelic ladder,
  data \code{\link{JLO}} for an example of an individual sample file,
  data \code{\link{ECH}} for an example of an allelic lader.
}
\examples{
  #
  # load an example of allelic ladder results from an ABIF (*.fsa) file:
  #
data(ECH)
  #
  # Extract from internal size standard chanel number 5 the location 
  # of 14 peaks:
  #
ECH.maxis <- peakabif(ECH, 5, npeak = 14, tmin = 2.7, thres = 0.1, fig = FALSE)$maxis
  #
  # Load data about the expected size of peaks in bp for calibration:
  #
data(gs500liz)
lizbp <- gs500liz$liz # All peaks size in bp
lizbp[!gs500liz$mask1 | !gs500liz$mask2] <- NA # Mark useless peaks
lizbp <- lizbp[-c(1,2)] # The first two peaks are not extracted from ECH
ECH.calibr <- splinefun(ECH.maxis[!is.na(lizbp)], lizbp[!is.na(lizbp)])
  #
  # Show the allelic ladder for the 4 dyes:
  #
plotladder(ECH, 1, ECH.calibr, tmin = 3.1, thres = 0.3, fig = FALSE)
plotladder(ECH, 2, ECH.calibr, tmin = 3.1, thres = 0.35, fig = FALSE)
plotladder(ECH, 3, ECH.calibr, tmin = 3.1, thres = 0.2, fig = FALSE)
plotladder(ECH, 4, ECH.calibr, tmin = 3.1, thres = 0.2, fig = FALSE)
}

