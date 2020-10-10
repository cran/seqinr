\name{dinucleotides}
\alias{rho}
\alias{zscore}
\title{Statistical over- and under- representation of dinucleotides in a
  sequence}
\encoding{UTF-8}
\description{
  These two functions compute two different types of statistics for the
  measure of statistical dinculeotide over- and under-representation :
  the rho statistic, and the z-score, each computed for all 16 dinucleotides.
}
\usage{
rho(sequence, wordsize = 2, alphabet = s2c("acgt"))
zscore(sequence, simulations = NULL, modele, exact = FALSE, alphabet = s2c("acgt"), ... )
}
\arguments{
  \item{sequence}{a vector of single characters.}
  \item{wordsize}{an integer giving the size of word (n-mer) to consider.}
  \item{simulations}{ If \code{NULL}, analytical solution is computed
    when available (models \code{base} and \code{codon}). Otherwise, it
    should be the number of permutations for the z-score computation }
  \item{modele}{ A string of characters describing the model chosen for
    the random generation }
  \item{exact}{ Whether exact analytical calculation or an
  approximation should be used }
  \item{alphabet}{ A vector of single characters. }
  \item{...}{ Optional parameters for specific model permutations are
    passed on to \code{\link{permutation}} function. }
}
\details{
  The \code{rho} statistic, as presented in Karlin S., Cardon LR. (1994), can
  be computed on each of the 16 dinucleotides. It is the frequence of
  dinucleotide \emph{xy} divided by the product of frequencies of
  nucleotide \emph{x} and nucleotide \emph{y}. It is equal to 1.00 when
  dinucleotide \emph{xy} is formed by pure chance, and it is superior
  (respectively inferior) to 1.00 when dinucleotide \emph{xy} is over-
  (respectively under-) represented. Note that if you want to reproduce
  Karlin's results you have to compute the statistic from the sequence 
  concatenated with its inverted complement that is with something 
  like \code{rho(c(myseq, rev(comp(mysed))))}.

  The \code{zscore} statistic, as presented in Palmeira, L., Guéguen, L.
  and Lobry JR. (2006). The statistic is the normalization of the
  \code{rho} statistic by its expectation and variance according to a
  given random sequence generation model, and follows the
  standard normal distribution. This statistic can be computed
  with several models (cf. \code{\link{permutation}} for the description
  of each of the models). We provide analytical calculus for two of
  them: the \code{base} permutations model and the  \code{codon}
  permutations model.
  
  The \code{base} model allows for random sequence generation by
  shuffling (with/without replacement) of all bases in the sequence.
  Analytical computations are available for this model: either as an 
  approximation for large sequences (cf. Palmeira, L., Guéguen, L.
  and Lobry JR. (2006)), either as the exact analytical formulae
  (cf. Schbath, S. (1995)).

  The \code{position} model allows for random sequence generation
  by shuffling (with/without replacement) of bases within their
  position in the codon (bases in position I, II or III stay in
  position I, II or III in the new sequence.

  The \code{codon} model allows for random sequence generation by
  shuffling (with/without replacement) of codons. Analytical
  computation is available for this model (Gautier, C., Gouy, M. and
  Louail, S. (1985)).

  The \code{syncodon} model allows for random sequence generation
  by shuffling (with/without replacement) of synonymous codons.
}
\value{
  a table containing the computed statistic for each dinucleotide
}
\references{
  Gautier, C., Gouy, M. and Louail, S. (1985) Non-parametric statistics
  for nucleic acid sequence study. \emph{Biochimie}, \bold{67}:449-453.
  
  Karlin S. and Cardon LR. (1994) Computational DNA sequence analysis.
  \emph{Annu Rev Microbiol}, \bold{48}:619-654.

  Schbath, S. (1995) Étude asymptotique du nombre d'occurrences d'un
  mot dans une chaîne de Markov et application à la recherche de mots
  de fréquence exceptionnelle dans les séquences d'ADN.
  \emph{Thèse de l'Université René Descartes, Paris V}

  Palmeira, L., Guéguen, L. and Lobry, J.R. (2006) UV-targeted dinucleotides
  are not depleted in light-exposed Prokaryotic genomes.
  \emph{Molecular Biology and Evolution},
  \bold{23}:2214-2219.
  \url{https://academic.oup.com/mbe/article/23/11/2214/1335460}

  \code{citation("seqinr")}
}
\author{L. Palmeira, J.R. Lobry with suggestions from A. Coghlan.}
\seealso{ \code{\link{permutation}} }
\examples{
\dontrun{
sequence <- sample(x = s2c("acgt"), size = 6000, replace = TRUE)
rho(sequence)
zscore(sequence, modele = "base")
zscore(sequence, modele = "base", exact = TRUE)
zscore(sequence, modele = "codon")
zscore(sequence, simulations = 1000, modele = "syncodon")
}
}
