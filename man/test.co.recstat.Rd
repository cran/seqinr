\name{test.co.recstat}
\alias{test.co.recstat}
\title{Tests if regions located between Stop codons contain putative CDSs.}
\description{This test uses columns (codons) factor scores computed by \code{recstat} in order
to determine if the regions located between two Stop codons correspond to putative CDSs.}
\usage{test.co.recstat(rec, fac = 1, length.min = 150, stop.max = 0.2, win.lim = 0.8,
    direct = TRUE, level = 0.01)}
\arguments{
    \item{rec}{list of elements returned by \code{recstat} function.}
    \item{fac}{axis of the CA to use for test (4 \eqn{\ge} \code{fac}
    \eqn{\ge} 1).}
	\item{length.min}{minimal length between two Stop codons.}
	\item{stop.max}{threshold for Stop codons relative position in a window to determine if this
	    window can be used for test computation.}
	\item{win.lim}{minimum proportion of windows inside a region showing a p-value below the
	    threshold for Kruskal-Wallis test.}
    \item{direct}{a logical for the choice of direct or reverse strand.}
	\item{level}{p-value threshold for Kruskal-Wallis test.}
}
\details{The test is computed for all windows located between two Stop codons separated by at least
\code{length.min} nucleotides. For each window inside a region considered, a Kruskal-Wallis test
is computed on the factor scores of the codons found in this window, this for the three possible
reading frames. If a proportion of at least \code{win.lim} windows in the region reject the null
hypothesis of means equality between the reading frames, then, there is a good probability that
a CDS is located in the region.\cr

Inside the first and the last windows of a region submitted to the test, the relative position of
the two Stop codons is used to determine if those windows can be used in the analysis. If the
first Stop is located within the \code{stop.max} fraction of the 5' end of the window, then this
window is kept in the analysis. In the same way, if the second Stop is located within the
\code{stop.max} fraction of the 3' end of the window, this window is also kept in the analysis.
}
\value{The result is returned as a list containing three matrices (one for each reading frame).
All matrices have the same structure, with rows corresponding to the regions between
two Stop codons. Columns \code{Start} and \code{End} give the location of starting and ending
positions of the region; and \code{CDS} is a binary indicator equal to 1 if a putative CDS is
predicted, and to 0 if not.
}
\author{O. Clerc, G. Perrière}
\seealso{\code{\link{test.li.recstat}}}
\examples{
\dontrun{# CPU time  is too long with windows
ff <- system.file("sequences/ECOUNC.fsa", package = "seqinr")
seq <- read.fasta(ff)
rec <- recstat(seq[[1]], seqname = getName(seq))
test.co.recstat(rec)
}
}
\keyword{sequence}
\keyword{correspondence analysis}
