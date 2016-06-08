\name{swap}
\alias{swap}
\title{Exchange two R objects}
\description{
Exchange object \code{x} with object \code{y}.
}
\usage{
swap(x, y)
}
\arguments{
  \item{x}{an R object}
  \item{y}{an R object}
}
\value{
none.
}
\references{ 
\code{citation("seqinr")}
}
\author{J.R. Lobry}
\seealso{\code{\link{move}} }

\examples{
#
# Example in a new empty environment:
#
local({
  x <- 0:9
  y <- 10:19
  print(x)
  print(y)
  swap(x[1], y[2])
  print(x)
  print(y)
})
#
# Sanity check with a bubble sort:
#
bubble.sort <- function(tab, n = length(tab)){
  i <- 1
  while(i < n){
    if(tab[i + 1] < tab[i]){
      swap(tab[i], tab[i+1])
      i <- 1
    } else {
      i <- i+1
    }
  }
  return(tab)
}
set.seed(1)
x <- rnorm(10)
stopifnot(identical(sort(x), bubble.sort(x)))
}
