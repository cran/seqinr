\name{test.li.recstat}
\alias{test.li.recstat}
\title{Tests if regions located between Stop codons contain putative CDSs.}
\description{This test uses rows (windows) factor scores computed by \code{recstat} in order to
determine if the regions located between two Stop codons correspond to putative CDSs.}
\usage{test.li.recstat(rec, fac = 1, length.min = 150, stop.max = 0.2,
    direct = TRUE, level = 0.05)}
\arguments{
    \item{rec}{list of elements returned by \code{recstat} function.}
    \item{fac}{axis of the CA to use for test (4 \eqn{\ge} \code{fac}
    \eqn{\ge} 1).}
	\item{length.min}{minimal length between two Stop codons.}
	\item{stop.max}{threshold for Stop codons relative position in a window to determine if this
	    window can be used for test computation.}
    \item{direct}{a logical for the choice of direct or reverse strand.}
	\item{level}{p-value threshold for t-test.}
}
\details{The test is computed for all regions between two Stop codons separated by at least
\code{length.min} nucleotides, this for the three possible reading frames of a DNA strand. For
each region considered, two t-tests are computed for comparing the mean of the factor scores of
the windows from the reading frame in which the region is located with the means of the factor
scores from the corresponding windows in the two other reading frames. If both t-tests reject
the null hypothesis of means equality, then there is a good probability that a CDS is located in
the region.\cr

Inside the first and the last windows of a region submitted to the test, the relative position of
the two Stop codons is used to determine if those windows can be used in the analysis. If the
first Stop is located within the \code{stop.max} fraction of the 5' end of the window, then this
window is kept in the analysis. In the same way, if the second Stop is located within the
\code{stop.max} fraction of the 3' end of the window, this window is also kept in the analysis.
}
\value{The result is returned as a list containing three matrices (one for each reading frame).
All matrices have the same structure, with rows corresponding to the regions between two Stop
codons. Columns \code{Start} and \code{End}  give the location of starting and ending positions
of the region; \code{Mean i} gives the mean of the factor scores for the windows located in the
region, this for reading frame \code{i}; \code{t(i,j)} gives the p-value of the t-test computed
between the means from reading frames \code{i} and \code{j}; and \code{CDS} is a binary
indicator equal to 1 if a putative CDS is predicted, and to 0 if not.
}
\author{O. Clerc, G. Perrière}
\seealso{\code{\link{test.co.recstat}}}
\examples{
ff <- system.file("sequences/ECOUNC.fsa", package = "seqinr")
seq <- read.fasta(ff)
rec <- recstat(seq[[1]], seqname = getName(seq))
test.li.recstat(rec)
}
\keyword{sequence}
\keyword{correspondence analysis}
