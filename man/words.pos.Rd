\name{words.pos}
\alias{words.pos}
\title{ Positions of possibly degenerated motifs within sequences }
\description{
\code{word.pos} searches all the occurences of the motif \code{pattern}
within the sequence \code{text} and returns their positions. This
function is based on \code{regexp} allowing thus for complex motif searches.
The main difference with \code{\link{gregexpr}} is that non disjoint matches
are reported here.
}
\usage{
words.pos(pattern, text, ignore.case = FALSE,
                      perl = TRUE, fixed = FALSE, useBytes = TRUE, ...)
}
\arguments{
  \item{pattern}{character string containing a \link{regular expression} (or character string for \code{fixed = TRUE}) to be matched in the given character vector.}
  \item{text}{ a character vector where matches are sought. }
  \item{ignore.case}{if \code{FALSE}, the pattern matching is case sensitive and if \code{TRUE}, case is ignored during matching.}
  \item{perl}{logical. Should perl-compatible regexps be used if available?
Has priority over \code{extended}.}
  \item{fixed}{logical. If \code{TRUE}, pattern is a string to be matched as is. Overrides all conflicting arguments.}
  \item{useBytes}{logical. If \code{TRUE} the matching is done byte-by-byte rather than character-by-character.}
  \item{...}{arguments passed to \code{\link{regexpr}}.}
}
\details{
Default parameter values have been tuned for speed when working biological sequences.
}
\value{
a vector of positions for which the motif \code{pattern} was
found in the sequence \code{text}.
}
\references{
  \code{citation("seqinr")}
}
\author{J.R. Lobry}
\seealso{ \code{\link{regexpr}} }
\examples{
myseq <- "tatagaga"
words.pos("t", myseq)   # Should be 1 3
words.pos("tag", myseq) # Should be 3
words.pos("ga", myseq)  # Should be 5 7
# How to specify ambiguous base ? Look for YpR motifs by
words.pos("[ct][ag]", myseq) # Should be 1 3
#
# Show the difference with gregexpr:
#
words.pos("toto", "totototo")           # 1 3 5 (three overlapping matches)
unlist(gregexpr("toto",  "totototo")) # 1 5    (two disjoint matches)
}
