\name{consensus}
\alias{consensus}
\alias{con}
\title{Consensus and profiles for sequence alignments}
\description{
  This function returns a consensus using variuous methods (see details)
  or a profile from a sequence alignment.
}
\usage{
consensus(matali, method = c( "majority", "threshold", "IUPAC", "profile"), 
  threshold = 0.60, warn.non.IUPAC = FALSE, type = c("DNA", "RNA"))
con(matali, method = c( "majority", "threshold", "IUPAC", "profile"), 
  threshold = 0.60, warn.non.IUPAC = FALSE, type = c("DNA", "RNA"))
}
\arguments{
  \item{matali}{an object of class \code{alignment} as returned by 
    \code{\link{read.alignment}}, or a matrix of characters.}
  \item{method}{select the method to use, see details.}
  \item{threshold}{for the \code{threshold} method, a numeric value beteen 0 and 1
    indicating the minimum relative frequency for a character to be returned
    as the consensus character. If none, NA is returned.}
  \item{warn.non.IUPAC}{for the \code{IUPAC} method this argument is passed 
   to \code{\link{bma}} with a default value set to FALSE to avoid warnings due 
   to gap characters in the alignment.}
  \item{type}{for the \code{IUPAC} method this argument is passed 
   to \code{\link{bma}}.}
}
\details{
  \describe{
  \item{"majority"}{The character with the higher frequency is returned as the
  consensus character.}

  \item{"threshold"}{As above but in addition the character relative frequency
  must be higher than the value controled by the \code{threshold} argument.
  If none, NA id returned.}

  \item{"IUPAC"}{Make sense only for nucleic acid sequences (DNA or RNA).
  The consensus character is defined if possible by an IUPAC symbol by
  function \code{\link{bma}}. If this is not possible, when there is a gap
  character for instance, NA is returned.}

  \item{"profile"}{With this method a matrix with the count of each possible
  character at each position is returned.}
  }
\code{con} is a short form for \code{consensus}.

}
\value{
  Either a vector of single characters with possible NA or a matrix with
  the method \code{profile}.
}
\references{ 
\code{citation("seqinr")}
}
\author{J.R. Lobry}

\seealso{See \code{\link{read.alignment}} to import alignment from files.}

\examples{
#
# Read 5 aligned DNA sequences at 42 sites:
#
  phylip <- read.alignment(file = system.file("sequences/test.phylip", 
    package = "seqinr"), format = "phylip")
#
# Show data in a matrix form:
#
  (matali <- as.matrix(phylip))
#
# With the majority rule:
#
  res <- consensus(phylip)
  stopifnot(c2s(res) == "aaaccctggccgttcagggtaaaccgtggccgggcagggtat")
#
# With a threshold:
#
  res.thr <- consensus(phylip, method = "threshold")
  res.thr[is.na(res.thr)] <- "." # change NA into dots
# stopifnot(c2s(res.thr) == "aa.c..t.gc.gtt..g..t.a.cc..ggccg.......ta.")
  stopifnot(c2s(res.thr) == "aa.cc.tggccgttcagggtaaacc.tggccgg.cagggtat")
#
# With an IUPAC summary:
#
  res.iup <- consensus(phylip, method = "IUPAC")
  stopifnot(c2s(res.iup) == "amvsbnkkgcmkkkmmgsktrmrssndkgcmrkdmmvskyaw")
  # replace 3 and 4-fold symbols by dots:
  res.iup[match(res.iup, s2c("bdhvn"), nomatch = 0) > 0] <- "."
  stopifnot(c2s(res.iup) == "am.s..kkgcmkkkmmgsktrmrss..kgcmrk.mm.skyaw")
#
# With a profile method:
#
  (res <- consensus(phylip, method = "profile"))
#
# Show the connection between the profile and some consensus:
#
  bxc <- barplot(res, col = c("green", "blue", "orange", "white", "red"), border = NA,
  space = 0, las = 2, ylab = "Base count",
  main = "Profile of a DNA sequence alignment",
  xlab = "sequence position", xaxs = "i")
  
  text(x = bxc, y = par("usr")[4],lab = res.thr, pos = 3, xpd = NA)
  text(x = bxc, y = par("usr")[1],lab = res.iup, pos = 1, xpd = NA)
}
