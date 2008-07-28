\name{caitab}
\alias{caitab}
\docType{data}
\title{Codon Adaptation Index (CAI) w tables}
\description{
Information about a preferred set of codons for highly expressed genes
in three species.
}
\usage{data(caitab)}
\format{
  A data frame with 64 rows for the codons and the following 3 columns:
  \describe{
    \item{ec}{\emph{Escherichia coli}}
    \item{bs}{\emph{Bacillus subtilis}}
    \item{sc}{\emph{Saccharomyces cerevisiae}}  
  }
}
\details{
  Codons are given by \code{row.names(caitab)}.
}
\source{
The data were hard-encoded in 
the C program codonW version 1.4.4 writen by John Peden
available at \url{http://codonw.sourceforge.net/}. The data
are from the file \code{codonW.h}. 
According to this source file, there were no reference for
\emph{Escherichia coli} and \emph{Bacillus subtilis} and the
reference for \emph{Saccharomyces cerevisiae} was Sharp
and Cowe (1991).

It turns out that the data for \emph{Escherichia coli} and
\emph{Saccharomyces cerevisiae} are identical to table 1
in Sharp and Li (1987) where the missing values for the stop
codons are represented here by zeros. All codons were documented
by at least one count in both datasets.

The data for \emph{Bacillus subtilis} are from table 2 in Shields
and Sharp (1987). Missing values for stops codons are represented
as previously by zeros, missing values for single-box amino-acids
are represented by 1 here. Note that some codons were undocumented
in this dataset and that a 0.5 value in absolute frequencies was
already forced to avoid zeros. It is therefore impossible to use
directly these data to obtain the exact expected CAI values as documented
in \code{\link{cai}} because of overlapping with documented codons.

}
\references{
  Sharp, P.M., Li, W.-H. (1987) The codon adaptation index - a
  measure of directional synonymous codon usage bias, and its
  potential applications.
  \emph{Nucleic Acids Research}, \bold{15}:1281-1295.

  Shields, D.C., Sharp, P.M. (1987) Synonymous codon usage in \emph{Bacillus subtilis}
  reflects both traditional selection and mutational biases. 
  \emph{Nucleic Acids Research}, \bold{15}:8023-8040.

  Sharp, P. M., Cowe, E. (1991). 
  Synonymous codon usage in \emph{Saccharomyces cerevisiae}. 
  \emph{Yeast}, \bold{7}:657-678.

  Peden, J.F. (1999)
  Analysis of codon usage.
  PhD Thesis, University of Nottingham, UK.

  \code{citation("seqinr")}
}
\seealso{
  \code{\link{cai}} for an example using this dataset to compute CAI values.
}
\examples{
  data(caitab)
}
\keyword{datasets}
