\name{circle}
\alias{circle}
\title{Draws a circle}
\description{Draws a circle or an arc-circle on the current graphic device}
\usage{
circle(x = 0, y = 0, r = 1, theta = c(0, 360), n = 100, ...)
}
\arguments{
  \item{x}{x coordinate for the center of the circle}
  \item{y}{y coordinate for the center of the circle}
  \item{r}{radius of the circle}
  \item{theta}{start and stop angle}
  \item{n}{number of points for polygon object}
  \item{\dots}{arguments passed to \code{\link{polygon}}}
}
\value{none}
\author{J.R. Lobry}

\seealso{\code{\link{polygon}}}
\examples{
par(mfrow = c(2, 2), mar = c(0,0,2,0))
setup <- function(){
  plot.new()
  plot.window(xlim = c(-1,1), ylim = c(-1,1), asp = 1)
}

setup()
circle(col = "lightblue")
title(main = "theta = c(0, 360)")

setup()
circle(col = "lightblue", theta = c(0, 270))
title(main = "theta = c(0, 270)")

setup()
circle(col = "lightblue", theta = c(-90, 180))
title(main = "theta = c(-90, 180)")

setup()
n <- 20
for(i in seq(0, 360, length = n)){
  circle(col = "lightblue", theta = c(i, i+360/(2*n)))
}
title(main = "many thetas")
}
