\name{words.pos}
\alias{words.pos}
\title{ Positions of possibly degenerated motifs within sequences }
\description{
\code{word.pos} searches all the occurences of the motif \code{pattern}
within the sequence \code{text} and returns their positions. This
function is based on \code{regexp} allowing thus for complex motif searches.
}
\usage{
words.pos(pattern, text, extended = TRUE, perl = FALSE)
}
\arguments{
  \item{pattern}{character string containing a regular expression to be
matched in the given character vector.}
  \item{text}{ a character vector where matches are sought. }
  \item{extended}{if `TRUE', extended regular expression matching is used,
and if `FALSE' basic regular expressions are used.
 }
  \item{perl}{logical. Should perl-compatible regexps be used if available?
Has priority over `extended' }
}
\details{
The regular expressions used are those specified by POSIX 1003.2,
either extended or basic, depending on the value of the `extended'
argument, unless `perl = TRUE' when they are those of PCRE, \url{
ftp://ftp.csx.cam.ac.uk/pub/software/programming/pcre/}.\cr
`perl=TRUE' will only be available if R was compiled against 
 PCRE: this is detected at configure time.  All Unix and Windows
system should have it.
}
\value{
a vector of positions for which the motif \code{pattern} was
found in the sequence \code{text}.
}
\references{ 
To have an overview of the seqinR's functionnality, please consult this vignette: 
Charif, D., Lobry, J.R. (2005) SeqinR: a contributed package to the R project for statistical
computing devoted to biological sequences retrieval and analysis. Springer Verlag, \emph{Biological and Medical Physics/Biomedical Series}, in preparation. \cr
}
\author{ J.R. Lobry }
\seealso{ \code{\link{regexpr}} }
\examples{
myseq <- "tatagaga"
words.pos("t", myseq)   # Should be 1 3
words.pos("tag", myseq) # Should be 3
words.pos("ga", myseq)  # Should be 5 7
# How to specify ambiguous base ? Look for YpR motifs by
words.pos("[ct][ag]", myseq) # Should be 1 3
}
\keyword{ utilities }
