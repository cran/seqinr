\name{gs500liz}
\alias{gs500liz}
\docType{data}
\title{GS500LIZ size standards}
\description{
GS500LIZ is an internal size standard often used in capillary electrophoresis.
It contains 16 fragments ranging in size from 35 to 500 bp. Note that they are not
all used for calibration : fragments at 250 and 340 bp may migrate anomalously 
(most likey because of secondary structure formation).
}
\usage{data(gs500liz)}
\format{
  A list with 3 components.
  \describe{
    \item{liz}{a vector of 16 values for the fragment sizes in bp.}
    \item{mask1}{a vector of 16 logicals to remove fragments whose
  migration may be anomalous (250 and 340 bp).}
    \item{mask2}{a vector of 16 logicals to remove extreme fragments (35, 50,
  490, 500 bp) so that the resulting fragments are in the 75-450 bp range.}
  }
}

\examples{
data(gs500liz)
op <- par(no.readonly = TRUE)
par(lend = "butt", mar = c(5,0,4,0)+0.1)
x <- gs500liz$liz
n <- length(x)
y <- rep(1, n)
plot(x, y, type = "h", yaxt = "n", xlab = "Fragment size [bp]",
  main = "GS500LIZ size standard", lwd = 2)
x1 <- x[!gs500liz$mask1]
segments(x1, 0, x1, 1, col = "red", lwd = 2)
x2 <- x[!gs500liz$mask2]
segments(x2, 0, x2, 1, col = "blue", lwd = 2)
col <- rep("black", n)
col[!gs500liz$mask1] <- "red"
col[!gs500liz$mask2] <- "blue"
text(x,1.05,paste(x, "bp"), srt = 90, col = col)
legend("top", inset = 0.1, legend = c("regular", "imprecise (mask1)", "extreme (mask2)"),
  lwd = 2, col = c("black","red","blue"))
par(op)
}
