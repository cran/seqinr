\name{tablecode}
\alias{tablecode}
\title{ to plot genetic code as in textbooks }
\description{
This function plots a genetic code table as in textbooks, that is
following the order \code{T > C > A > G} so that synonymous codons
are almost always in the same boxes.
}
\usage{
tablecode(numcode = 1, urn.rna = s2c("TCAG"), dia = FALSE, latexfile = NULL,
label = latexfile, size = "normalsize", caption = NULL,
preaa = rep("", 64), postaa = rep("", 64), 
precodon = preaa, postcodon = postaa)
}
\arguments{
  \item{numcode}{The genetic code number as in \code{translate} }
  \item{urn.rna}{The letters to display codons, use s2c("UCAG")
  if you want the code in terms of RNA sequence}
  \item{latexfile}{The name of a LaTex file if you want to redirect the output}
  \item{label}{The label for the LaTeX table}
  \item{size}{The LaTex size of characters for the LaTeX table}
  \item{preaa}{A string to insert before the amino-acid in the LaTeX table}
  \item{postaa}{A string to insert after the amino-acid in the LaTeX table}
  \item{precodon}{A string to insert before the codon in the LaTeX table}
  \item{postcodon}{A string to insert after the codon in the LaTeX table}
  \item{caption}{The caption of the LaTeX table}
  \item{dia}{to produce a yellow/blue plot for slides}
}
\details{
The codon order for \code{preaa}, \code{postaa}, \code{precodon}, and
\code{postcodon} should be the same as in
\code{paste(paste(rep(s2c("tcag"), each =16), s2c("tcag"), sep = ""), rep(s2c("tcag"), each = 4), sep = "")}
}
\references{
  \code{citation("seqinr")}
}
\author{J.R. Lobry}
\seealso{ \code{\link{translate}}, \code{\link{syncodons}} }
\examples{
#
# Show me the standard genetic code:
#

 tablecode()
}
\keyword{ utilities }
