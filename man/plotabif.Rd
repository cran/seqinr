\name{plotabif}
\alias{plotabif}
\title{Electrophoregram plot for ABIF data}
\description{
  Simple chromatogram plot for data imported with the \code{\link{read.abif}} function.
}
\usage{
plotabif(abifdata, 
  chanel = 1, 
  tmin = 1/tscale, 
  tmax = abifdata$Data[["SCAN.1"]]/tscale, 
  tscale = 1000, 
  yscale = 1000, type = "l", las = 1, 
  xlab = paste("Time", tscale, sep = "/"),
  ylab = paste("RFU", yscale, sep = "/"), 
  irange = (tmin*tscale):(tmax*tscale),
  x = irange/tscale,
  xlim = c(tmin, tmax),
  chanel.names = c(1:4,105),
  DATA = paste("DATA", chanel.names[chanel], sep = "."),
  y = abifdata$Data[[DATA]][irange]/yscale,
  ylim = c(min(y), max(y)),
  dyn = abifdata$Data[[paste("DyeN", chanel, sep = ".")]],
  main = paste(deparse(substitute(abifdata)), chanel, dyn, sep = " ; "),
  calibr = NULL,
  ladder.bp = NULL,
  allele.names = "identifiler",
  ladder.lab = TRUE,
  ...)
}
\arguments{
  \item{abifdata}{the result returned by \code{\link{read.abif}}}
  \item{chanel}{the dye number}
  \item{tmin}{scaled starting time for the time axis}
  \item{tmax}{scaled ending time for the time axis}
  \item{tscale}{scale factor for the time axis}
  \item{yscale}{scale factor for the y-axis (RFU)}
  \item{type}{type of line drawing forwarded to \code{\link{plot}}}
  \item{las}{orientation of axis labels forwarded to \code{\link{plot}}}
  \item{xlab}{x-axis label forwarded to \code{\link{plot}}}
  \item{ylab}{y-axis label forwarded to \code{\link{plot}}}
  \item{irange}{indices of data to be plotted}
  \item{x}{values used for the x-axis}
  \item{xlim}{limits for the x-axis forwarded to \code{\link{plot}}}
  \item{chanel.names}{numbers extensions used for the DATA}
  \item{DATA}{names of the DATA components}
  \item{y}{values used for the y-axis}
  \item{ylim}{limits for the y-axis forwarded to \code{\link{plot}}}
  \item{dyn}{dye name}
  \item{main}{title for the plot forwarded to \code{\link{plot}}}
  \item{calibr}{an optional calibration function to convert time into bp}
  \item{ladder.bp}{an optional ladder scale in bp (calibr must be provided)}
  \item{allele.names}{name of the dataset with allele names}
  \item{ladder.lab}{logical: should allele names be added on plot}
  \item{...}{arguments forwarded to \code{\link{plot}}}
}
\value{
 Returns invisibly its local graphical parameter settings.
}
\references{
  \code{citation("seqinr")}
}
\author{J.R. Lobry}
\seealso{
  function \code{\link{read.abif}} to import files in ABIF format, 
  data \code{\link{gs500liz}} for internal size standards,
  data \code{\link{identifiler}} for allele names in the allelic ladder,
  data \code{\link{JLO}} for an example of an individual sample file,
  data \code{\link{ECH}} for an example of an allelic lader.
}
\examples{
data(ECH)
plotabif(ECH,chanel = 1, tmin = 3.2, tmax = 6.1)
}

