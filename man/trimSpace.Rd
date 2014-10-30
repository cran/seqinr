\name{trimSpace}
\alias{trimSpace}
\title{Trim leading and/or trailing spaces in strings}
\description{
  This function removes from a character vector the longest successive run
  of space characters starting at the begining of the strings (leading space),
  or the longest successive run of space characters at the end of the strings
  (trailing space), or both (and this is the default behaviour).
}
\usage{
trimSpace(x, leading = TRUE, trailing = TRUE, space = "[:space:]")
}
\arguments{
  \item{x}{a character vector}
  \item{leading}{logical defaulting to \code{TRUE}: should leading spaces be trimed off?}
  \item{trailing}{logical defaulting to \code{TRUE}: should trailing spaces be trimed off?}
  \item{space}{an extended regular expression defining space characters}
}
\details{
  The default value for the space character definition is large: in addition to the
  usual space, other character such as the tabulation and newline character are
  considered as space characters. See extended regular expression for a complete list.
}
\value{
  a character vector with the same length as x.
}
\references{
  \code{citation("seqinr")}.
}
\seealso{Extended regular expressionsare described in \link{regular expression} 
 (aka \code{\link{regexp}}).
}
\author{J.R. Lobry}
\examples{
  #
  # Simple use:
  #
stopifnot( trimSpace("   seqinR   ") == "seqinR" )

  #
  # Basic use, remove space at both ends:
  #
testspace <- c("   with leading space", "with trailing space   ", "   with both   ")
stopifnot(all( trimSpace(testspace) == c("with leading space", 
                                         "with trailing space", 
					 "with both")))

  #
  # Remove only leading space:
  #
stopifnot(all( trimSpace(testspace, trailing = FALSE) == c("with leading space",
                                                           "with trailing space   ",  
							   "with both   ")))

  #
  # Remove only trailing space:
  #
stopifnot(all( trimSpace(testspace, leading = FALSE) == c("   with leading space", 
                                                          "with trailing space",  
							  "   with both")))

  #
  # This should do nothing:
  #
stopifnot(all( trimSpace(testspace, leading = FALSE, trailing = FALSE) == testspace))

  #
  # How to use alternative space characters:
  #
allspaces <- "\t\n\f\r seqinR \t\n\f\r"
stopifnot(trimSpace(allspaces) == "seqinR")
stopifnot(trimSpace(allspaces, space = "\t\n") == "\f\r seqinR \t\n\f\r")
}
\keyword{manip}
