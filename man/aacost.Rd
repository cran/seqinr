\name{aacost}
\alias{aacost}
\docType{data}
\title{Aerobic cost of amino-acids in Escherichia coli and G+C classes}
\description{
The metabolic cost of amino-acid biosynthesis in \emph{E. coli} under aerobic conditions
from table 1 in Akashi and Gojobori (2002). The G+C classes are from Lobry (1997).
}
\usage{data(aacost)}
\format{
  A data frame with 20 rows for the amino-acids and the following 7 columns:
  \describe{
    \item{aaa}{amino-acid (three-letters code).}
    \item{a}{amino-acid (one-letter code).}
    \item{prec}{precursor metabolites (see details).}
    \item{p}{number of high-energy phosphate bonds contained in ATP and GTP molecules.}
    \item{h}{number of available hydrogen atoms carried in NADH, NADPH,
    and FADH2 molcules.}
    \item{tot}{total metabolic cost assuming 2 high-energy phosphate bonds
    per hydrogen atom.}
    \item{gc}{an ordered factor (l<m<h) for the G+C class of the amino-acid (see details)}
  }
}
\details{Precursor metabolites are: penP, ribose 5-phosphate; PRPP,
5-phosphoribosyl pyrophosphate; eryP, erythrose 4-phosphate; 3pg,
3-phosphoglycerate; pep, phospho\emph{enol}pyruvate; pyr, pyruvate;
acCoA, acetyl-CoA; akg, alpha-ketoglutarate; oaa, oxaloacetate. Negative
signs on precursor metabolites indicate chemicals \emph{gained} through
biosynthetic pathways. Costs of precursors reflect averages for growth on
glucose, acetate, and malate (see Table 6 in the supporting information
from Akashi and Gojobori 2002).\cr

The levels l<m<h for the \code{gc} ordered factor stand for Low G+C, Middle G+C,
High G+C amino-acid, respectively. The frequencies of Low G+C amino-acids
monotonously decrease with G+C content. The frequencies of High G+C amino-
acids monotonously increase with G+C content. The frequencies of Middle G+C
amino-acids first increase and then decrease with G+C content. These G+C classes 
are from Lobry (1997).\cr

\code{example(aacost)} reproduces figure 2 from Lobry (2004).

\if{html}{\figure{aka.pdf}{options: width=400}}
\if{latex}{\figure{aka.pdf}{options: width=12cm}}

}
\source{
Akashi, H, Gojobori, T. (2002) Metabolic efficiency and amino acid composition
in the proteomes of \emph{Escherichia coli} and \emph{Bacillus subtilis}.
\emph{Proceedings of the National Academy of Sciences of the United States of America}, 
\bold{99}:3695-3700.\cr

Lobry, J.R. (1997) Influence of genomic G+C content on average amino-acid composition 
of proteins from 59 bacterial species. \emph{Gene}, \bold{205}:309-316.\cr

Lobry, J.R. (2004) Life history traits and genome structure: aerobiosis and G+C content in bacteria. 
\emph{Lecture Notes in Computer Sciences}, \bold{3039}:679-686.
}
\references{
\code{citation("seqinr")}
}
\examples{
data(aacost)
levels(aacost$gc) <- c("low G+C", "mid G+C", "high G+C")
stripchart(aacost$tot~aacost$gc, pch = 19, ylim = c(0.5,3.5),
     xlim = c(0, max(aacost$tot)),
     xlab = "Metabolic cost (high-energy phosphate bonds equivalent)",
     main = "Metabolic cost of the 20 amino-acids\nas function of their G+C class" )
boxplot(aacost$tot~aacost$gc, horizontal = TRUE, add = TRUE)   
}
\keyword{datasets}
