\name{rot13}
\alias{rot13}
\title{Ergheaf gur EBG-13 pvcurevat bs n fgevat}
\description{
    rot13 applied to the above title returns the string
    "Returns the ROT-13 ciphering of a string".
}
\usage{
rot13(string)
}
\arguments{
  \item{string}{a string of characters.}
}
\value{
  a string of characters.
}
\author{J.R. Lobry}
\references{
  \code{citation("seqinr")}
}
\seealso{\code{\link{chartr}}
}
\examples{
##
## Simple ciphering of a string:
##
message <- "Hello, world!"
rot13(message) # "Uryyb, jbeyq!"
##
## Routine sanity check:
##
stopifnot(identical(rot13(rot13(message)), message))
}
\keyword{ manip }
