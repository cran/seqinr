\name{m16j}
\alias{m16j}
\docType{data}
\title{Fragment of the E. coli chromosome}
\description{
 A fragment of the \emph{E. coli} chromosome that was used in Lobry (1996) to show the
change in GC skew at the origin of replication (\emph{i.e.} the chirochore structure
of bacterial chromosomes)
}
\usage{data(m16j)}
\format{
  A string of 1,616,539 characters
}
\details{
The sequence used in Lobry (1996) was a 1,616,174 bp fragment obtained from the concatenation
of nine overlapping sequences (U18997, U00039, L10328, M87049, L19201, U00006, U14003,
D10483, D26562. Ambiguities have been resolved since then and its was
a chimeric sequence from K-12 strains MG1655 and W3110, the sequence used here is
from strain MG1655 only (Blattner \emph{et al.} 1997).

The chirochore structure of bacterial genomes is illustrated below by a screenshot
of a part of figure 1 from Lobry (1996). See the example section to reproduce this
figure.

\if{html}{\figure{gcskewmbe96.pdf}{options: width=400}}
\if{latex}{\figure{gcskewmbe96.pdf}{options: width=12cm}}


}
\source{
\emph{Escherichia coli} K-12 strain MG1655. Fragment from U00096 from the
EBI Genome Reviews. Acnuc Release 7. Last Updated: Feb 26, 2007.
XX
DT   18-FEB-2004 (Rel. .1, Created)
DT   09-JAN-2007 (Rel. 65, Last updated, Version 70)
XX
}
\references{
Lobry, J.R. (1996) Asymmetric substitution patterns in the two DNA strands of
bacteria. \emph{Molecular Biology and Evolution}, \bold{13}:660-665.\cr

F.R. Blattner, G. Plunkett III, C.A. Bloch, N.T. Perna, V. Burland, M. Rilley,
J. Collado-Vides, J.D. Glasner, C.K. Rode, G.F. Mayhew, J. Gregor,
N.W. Davis, H.A. Kirkpatrick, M.A. Goeden, D.J. Rose, B. Mau, and
Y. Shao. (1997) The complete genome sequence of \emph{Escherichia coli} K-12. 
\emph{Science}, \bold{277}:1453-1462\cr

\code{citation("seqinr")}
}
\examples{
#
# Load data:
#
data(m16j)
#
# Define a function to compute the GC skew:
#
gcskew <- function(x) {
  if (!is.character(x) || length(x) > 1)
  stop("single string expected")
  tmp <- tolower(s2c(x))
  nC <- sum(tmp == "c")
  nG <- sum(tmp == "g")
  if (nC + nG == 0)
  return(NA)
  return(100 * (nC - nG)/(nC + nG))
}
#
# Moving window along the sequence:
#
step <- 10000
wsize <- 10000
starts <- seq(from = 1, to = nchar(m16j), by = step)
starts <- starts[-length(starts)]
n <- length(starts)
result <- numeric(n)
for (i in seq_len(n)) {
  result[i] <- gcskew(substr(m16j, starts[i], starts[i] + wsize - 1))
}
#
# Plot the result:
#
xx <- starts/1000
yy <- result
n <- length(result)
hline <- 0
plot(yy ~ xx, type = "n", axes = FALSE, ann = FALSE, ylim = c(-10, 10))
polygon(c(xx[1], xx, xx[n]), c(min(yy), yy, min(yy)), col = "black", border = NA)
usr <- par("usr")
rect(usr[1], usr[3], usr[2], hline, col = "white", border = NA)
lines(xx, yy)
abline(h = hline)
box()
axis(1, at = seq(0, 1600, by = 200))
axis(2, las = 1)
title(xlab = "position (Kbp)", ylab = "(C-G)/(C+G) [percent]",
 main = expression(paste("GC skew in ", italic(Escherichia~coli))))
arrows(860, 5.5, 720, 0.5, length = 0.1, lwd = 2)
text(860, 5.5, "origin of replication", pos = 4)
}
\keyword{datasets}
