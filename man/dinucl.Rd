\name{dinucl}
\alias{dinucl}
\docType{data}
\title{ Mean zscore on 242 complete bacterial chromosomes }
\encoding{latin1}
\description{
This dataset contains the mean zscores as computed on all intergenic
sequences (intergenic) and on all CDS (coding) from 242
complete bacterial chromosomes (as retrieved from Genome Reviews database on
June 16, 2005).
}
\usage{
data(dinucl)
}
\format{
  List of two dataframes of 242 chromosomes and 16 dinucleotides: one
  for intergenic, one for coding sequences.
\describe{
  \item{intergenic}{the mean of \code{zscore} computed with the \code{base}
   model on each intergenic sequence}
   \item{coding}{the mean of \code{zscore} computed with the \code{codon}
  model on each coding sequence}
}
}
\references{
  Palmeira, L., Guéguen, L. and Lobry JR. (2006) UV-targeted dinucleotides
  are not depleted in light-exposed Prokaryotic genomes.
  \emph{Molecular Biology and Evolution},
  \bold{23}:2214-2219.\cr
  \url{https://academic.oup.com/mbe/article/23/11/2214/1335460}\cr\cr

  \code{citation("seqinr")}
}
\seealso{ \code{\link{zscore}} }
\examples{
data(dinucl)
par(mfrow = c(2, 2), mar = c(4,4,0.5,0.5)+0.1)
myplot <- function(x){
  plot(dinucl$intergenic[, x], dinucl$coding[, x],
  xlab = "intergenic", ylab = "coding", 
  las = 1, ylim = c(-6, 4), 
  xlim = c(-3, 3), cex = 0)
  rect(-10,-10,-1.96,10,col="yellow", border = "yellow")
  rect(1.96,-10,10,10,col="yellow", border = "yellow")
  rect(-10,-10,10,-1.96,col="yellow", border = "yellow")
  rect(-10,1.96,10,10,col="yellow", border = "yellow")
  abline(v=0,lty=3)
  abline(h=0,lty=3)
  abline(h=-1.96,lty=2)
  abline(h=+1.96,lty=2)
  abline(v=-1.96,lty=2)
  abline(v=+1.96,lty=2)
  points(dinucl$intergenic[, x], dinucl$coding[, x], pch = 21,
  col = rgb(.1,.1,.1,.5), bg = rgb(.5,.5,.5,.5))
  legend("bottomright", inset = 0.02,
   legend = paste(substr(x,1,1), "p",
    substr(x,2,2), " bias", sep = ""), cex = 1.25, bg = "white")
  box()
}
myplot("CT")
myplot("TC")
myplot("CC")
myplot("TT")
}
\keyword{ datasets }
