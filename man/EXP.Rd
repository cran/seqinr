\name{EXP}
\alias{EXP}
\docType{data}
\title{Vectors of coefficients to compute linear forms.}
\description{
This dataset is used to compute linear forms on codon frequencies:
if \code{codfreq} is a vector of codon frequencies then
\code{drop(freq \%*\% EXP$CG3)} will return for instance the G+C content
in third codon positions. Base order is the lexical order: a,
c, g, t (or u).
}
\usage{data(EXP)}
\format{List of 24 vectors of coefficients
\describe{
  \item{A}{num [1:4] 1 0 0 0}
  \item{A3}{num [1:64] 1 0 0 0 1 0 0 0 1 0 ...}
  \item{AGZ}{num [1:64] 0 0 0 0 0 0 0 0 1 0 ...}
  \item{ARG}{num [1:64] 0 0 0 0 0 0 0 0 1 0 ...}
  \item{AU3}{num [1:64] 1 0 0 1 1 0 0 1 1 0 ...}
  \item{BC}{num [1:64] 0 1 0 0 0 0 0 0 0 0 ...}
  \item{C}{num [1:4] 0 1 0 0}
  \item{C3}{num [1:64] 0 1 0 0 0 1 0 0 0 1 ...}
  \item{CAI}{num [1:64]  0.00  0.00 -1.37 -2.98 -2.58 ...}
  \item{CG}{num [1:4] 0 1 1 0}
  \item{CG1}{num [1:64] 0 0 0 0 0 0 0 0 0 0 ...}
  \item{CG12}{num [1:64] 0 0 0 0 0.5 0.5 0.5 0.5 0.5 0.5 ...}
  \item{CG2}{num [1:64] 0 0 0 0 1 1 1 1 1 1 ...}
  \item{CG3}{num [1:64] 0 1 1 0 0 1 1 0 0 1 ...}
  \item{CGN}{num [1:64] 0 0 0 0 0 0 0 0 0 0 ...}
  \item{F1}{num [1:64]  1.026  0.239  1.026  0.239 -0.097 ...}
  \item{G}{num [1:4] 0 0 1 0}
  \item{G3}{num [1:64] 0 0 1 0 0 0 1 0 0 0 ...}
  \item{KD}{num [1:64] -3.9 -3.5 -3.9 -3.5 -0.7 -0.7 -0.7 -0.7 -4.5 -0.8 ...}
  \item{Q}{num [1:64] 0 0 0 0 1 1 1 1 0 0 ...}
  \item{QA3}{num [1:64] 0 0 0 0 1 0 0 0 0 0 ...}
  \item{QC3}{num [1:64] 0 0 0 0 0 1 0 0 0 0 ...}
  \item{U}{num [1:4] 0 0 0 1}
  \item{U3}{num [1:64] 0 0 0 1 0 0 0 1 0 0 ...}
}
}
\details{
It's better to work directly at the amino-acid level
when computing linear forms on amino-acid frequencies so as to have
a single coefficient vector. For instance \code{EXP$KD} to compute the Kyte
and Doolittle hydrophaty index from codon frequencies is valid only
for the standard genetic code.\cr
\cr
An alternative for \code{drop(freq \%*\% EXP$CG3)} is \code{
sum( freq * EXP$CG3 )}, but this is less efficient in terms of CPU
time. The advantage of the latter, however, is that thanks to
recycling rules you can use either \code{sum( freq * EXP$A )}
or \code{sum( freq * EXP$A3 )}. To do the same with the \%*\%
operator you have to explicit the recycling rule as in \code{
drop( freq \%*\% rep(EXP$A, 16))}.
}
\source{
ANALSEQ EXPFILEs for command EXP.\cr
\url{http://pbil.univ-lyon1.fr/software/doclogi/docanals/manuel.html}
}
\references{
  \code{citation("seqinr")}
  \describe{
  \item{A}{content in A nucleotide}
  \item{A3}{content in A nucleotide in third position of codon}	
  \item{AGZ}{Arg content (aga and agg codons)}
  \item{ARG}{Arg content}
  \item{AU3}{content in A and U nucleotides in third position of codon}
  \item{BC}{Good choice (Bon choix). Gouy M., Gautier C. (1982)
 codon usage in bacteria : Correlation with gene expressivity. \emph{Nucleic Acids Research},\bold{10(22)}:7055-7074.}
  \item{C}{content in C nucleotides}
  \item{C3}{content in A nucleotides in third position of codon}
  \item{CAI}{Codon adaptation index for E. coli. Sharp, P.M., Li, W.-H. (1987) The codon adaptation index -
a measure of directionam synonymous codon usage bias, and its potential
applications. \emph{Nucleic Acids Research},\bold{15}:1281-1295.}
  \item{CG}{content in G + C nucleotides}
  \item{CG1}{content in G + C nucleotides in first position of codon}
  \item{CG12}{content in G + C nucleotides in first and second position of codon}
  \item{CG2}{content in G + C nucleotides in second position of codon}
  \item{CG3}{content in G + C nucleotides in third position of codon}
  \item{CGN}{content in CGA + CGU + CGA + CGG}
  \item{F1}{From Table 2 in Lobry, J.R., Gautier, C. (1994) Hydrophobicity,
expressivity and aromaticity are the major trends of amino-acid usage in
999 \emph{Escherichia coli} chromosome-encode genes. \emph{Nucleic Acids
Research},\bold{22}:3174-3180.}
  \item{G3}{content in G nucleotides in third position of codon}
  \item{KD}{Kyte, J., Doolittle, R.F. (1982) A simple method for displaying
the hydropathic character of a protein. \emph{J. Mol. Biol.},\bold{157}
:105-132.}
  \item{Q}{content in quartet}
  \item{QA3}{content in quartet with the A nucleotide in third position}
  \item{QC3}{content in quartet with the A nucleotide in third position}
  \item{U}{content in U nucleotide}
  \item{U3}{content in U nucleotides in third position of codon}
}
}
\examples{
data(EXP)
}
\keyword{datasets}
