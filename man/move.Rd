\name{move}
\alias{move}
\alias{mv}
\title{Rename an R object}
\description{
Rename object \code{from} into \code{to}.
}
\usage{
move(from, to)
mv(from, to)
}
\arguments{
  \item{from}{an R object name}
  \item{to}{the new R object name}
}
\value{
none.
}
\references{ 
\code{citation("seqinr")}
}
\author{J.R. Lobry}
\seealso{\code{\link{swap}} }

\examples{
#
# Example in a new empty environment:
#
local({
  zefplock <- pi
  print(ls())
  print(zefplock)
  mv(zefplock, toto)
  print(ls())
  print(toto)
  stopifnot(identical(toto, pi)) # Sanity check
})
#
# Check that self-affectation is possible:
#
mv(mv, mv) # force self-affectation for the function itself
mv(mv, mv) # OK, function mv() still exists
}
