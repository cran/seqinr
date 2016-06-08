\name{draw.oriloc}
\alias{draw.oriloc}
\title{Graphical representation for nucleotide skews in
  prokaryotic chromosomes.}
\description{
  Graphical representation for nucleotide skews in
  prokaryotic chromosomes.}

\usage{draw.oriloc(ori, main = "Title",
  xlab = "Map position in Kb",
  ylab = "Cumulated combined skew in Kb", las = 1, las.right = 3,
  ta.mtext = "Cumul. T-A skew", ta.col = "pink", ta.lwd = 1,
  cg.mtext = "Cumul. C-G skew", cg.col = "lightblue", cg.lwd = 1,
  cds.mtext = "Cumul. CDS skew", cds.col = "lightgreen", cds.lwd = 1,
  sk.col = "black", sk.lwd = 2,
  add.grid = TRUE, ...)}

\arguments{

\item{ori}{A data frame obtained with the \code{oriloc} function.}
\item{main}{The main title of the plot.}
\item{xlab}{The x-axis title.}
\item{ylab}{The y-axis title.}
\item{las}{The style of axis labels for the bottom and left axes.}
\item{las.right}{The style of axis labels for the right axis.}
\item{ta.mtext}{The marginal legend for the TA skew.}
\item{ta.col}{The color for the TA skew.}
\item{ta.lwd}{The line width for the TA skew.}
\item{cg.mtext}{The marginal legend for the CG skew.}
\item{cg.col}{The color for the CG skew.}
\item{cg.lwd}{The line width for the CG skew.}
\item{cds.mtext}{The marginal legend for the CDS skew.}
\item{cds.col}{The color for the CDS skew.}
\item{cds.lwd}{The line width for the CDS skew.}
\item{sk.col}{The color for the cumulated combined skew.}
\item{sk.lwd}{The line width for the cumulated combined skew.}
\item{add.grid}{Logical, if \code{TRUE} a vertical grid is added to the plot.}
\item{...}{Further arguments are passed to the function \code{plot}.}
}

\references{
\code{citation("seqinr")}
}

\author{J.R. Lobry}

\seealso{ \code{\link{oriloc}}, \code{\link{rearranged.oriloc}},
  \code{\link{extract.breakpoints}}  }

\examples{
\dontrun{ # need internet connection
#
# Example with Chlamydia trachomatis complete genome
#
  ori <- oriloc()
  draw.oriloc(ori)
#
# The same, using more options from function draw.oriloc()
#
draw.oriloc(ori, 
  main = expression(italic(Chlamydia~~trachomatis)~~complete~~genome),
  ta.mtext = "TA skew", ta.col = "red",
  cg.mtext = "CG skew", cg.col = "blue",
  cds.mtext = "CDS skew", cds.col = "seagreen",
  add.grid = FALSE)
}
}

\keyword{hplot}
