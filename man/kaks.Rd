\name{kaks}
\alias{kaks}
\title{Ka and Ks, also known as dn and ds, computation}
\description{ Ks and Ka  are, respectively, the number of substitutions per synonymous site and per non-synonymous site between two protein-coding genes. They are also denoted as ds and dn in the literature. The ratio of nonsynonymous (Ka) to synonymous (Ks) nucleotide substitution rates is an indicator of selective pressures on genes. A ratio significantly greater than 1 indicates positive selective pressure. A ratio around 1 indicates either neutral evolution at the protein level or an averaging of sites under positive and negative selective pressures. A ratio less than 1 indicates pressures to conserve protein sequence (\emph{i.e.} purifying selection). This function estimates the Ka and Ks values for a set of aligned sequences using the method published by Li (1993) and gives the associated variance matrix. 
}
\usage{
kaks(x, verbose = FALSE, debug = FALSE, forceUpperCase = TRUE, rmgap = TRUE)
}
\arguments{
  \item{x}{ An object of class \code{alignment}, obtained for instance by importing into R the data from an alignment file with the \code{\link{read.alignment}} function. This is typically a set of coding sequences aligned at the protein level, see \code{\link{reverse.align}}.}
  \item{verbose}{ If TRUE add to the results  the value of L0, L2, L4 (respectively the frequency of non-synonymous sites, of 2-fold synonymous sites, of 4-fold synonymous sites), A0, A2, A4 (respectively the number of transitional changes at non-synonymous, 2-fold, and 4-fold synonymous sites ) and B0, B2, B4 (respectively the number of transversional changes at non-synonymous, 2-fold, and 4-fold synonymous sites).}
  \item{debug}{ If TRUE turns debug mode on.}
  \item{forceUpperCase}{ If TRUE, the default value, all character in sequences are forced to the upper case
  if at least one 'a', 'c', 'g', or 't' is found in the sequences. 
  Turning it to FALSE if the sequences are already in upper case will save time.}
  \item{rmgap}{ If TRUE all positions with at least one gap are removed. If FALSE only positions with nothing else than gaps are removed.}
}
\value{
  \item{ ks }{ matrix of Ks values }
  \item{ ka }{ matrix of Ka values }
  \item{ vks }{ variance matrix of Ks }
  \item{ vka }{ variance matrix of Ka }	
}
\references{
Li, W.-H., Wu, C.-I., Luo, C.-C. (1985) A new method for estimating synonymous and nonsynonymous rates of nucleotide substitution considering the relative likelihood of nucleotide and codon changes. \emph{Mol. Biol. Evol}, \bold{2}:150-174\cr

Li, W.-H. (1993) Unbiased estimation of the rates of synonymous and nonsynonymous substitution. \emph{J. Mol. Evol.}, \bold{36}:96-99.\cr 

Pamilo, P., Bianchi, N.O. (1993) Evolution of the \emph{Zfx} and \emph{Zfy} genes: Rates and interdependence between genes. \emph{Mol. Biol. Evol}, \bold{10}:271-281\cr

Hurst, L.D. (2002) The Ka/Ks ratio: diagnosing the form of sequence evolution.
\emph{Trends Genet.}, \bold{18}:486-486.\cr  

The C programm implementing this method was provided by Manolo Gouy. More info is
needed here to trace back the original C source so as to credit correct source.
The original FORTRAN-77 code by Chung-I Wu modified by Ken Wolfe was available
here \url{http://wolfe.gen.tcd.ie/lab/pub/li93/} but this is no more true as 2017-07-01.\cr

For a more recent discussion about the estimation of Ka and Ks see:\cr

Tzeng, Y.H., Pan, R., Li, W.-H. (2004) Comparison of three methods for estimating 
rates of synonymous and nonsynonymous nucleotide substitutions.
\emph{Mol. Biol. Evol}, \bold{21}:2290-2298.\cr

The method implemented here is noted LWL85 in the above paper.\cr

The cite this package in a publication, as any R package, try something as \code{citation("seqinr")} 
at your R prompt.
}
\note{ 
Computing Ka and Ks makes sense for coding sequences that have been aligned at the amino-acid level before retro-translating the alignement at the nucleic acid level to ensure that sequences are compared on a codon-by-codon basis. Function \code{\link{reverse.align}} may help for this.

As from seqinR 2.0-3, when there is at least one non ACGT base in a codon, this codon is considered as a gap-codon (\code{---}). This makes the computation more robust with respect to alignments with out-of-frame gaps, see example section.

Gap-codons (\code{---}) are not used for computations. 

When the alignment does not contain enough information (\emph{i.e.} close to saturation), the Ka and Ks values are forced to 10 (more exactly to 9.999999).

Negative values indicate that Ka and Ks can not be computed.

According to Li (1993) and Pamilo and Bianchi (1993),
the rate of synonymous substitutions Ks is computed as:
Ks = (L2.A2 + L4.A4) / (L2 + L4)  +  B4

and the rate of non-synonymous substitutions Ka is computed as:
Ka =  A0 + (L0.B0 + L2.B2) / (L0 + L2) 

}
\author{D. Charif, J.R. Lobry}
\seealso{\code{\link{read.alignment}} to import alignments from files, \code{\link{reverse.align}} to align CDS at the aa level,
\code{\link{kaksTorture}} for test on one-codon CDS.}
\examples{
 #
 # Simple Toy example:
 #
 s <- read.alignment(file = system.file("sequences/test.phylip", package = "seqinr"),
  format = "phylip")
 kaks(s)
 #
 # Check numeric results on an simple test example:
 #
 data(AnoukResult)
 Anouk <- read.alignment(file = system.file("sequences/Anouk.fasta", package = "seqinr"),
  format = "fasta")	
 if( ! all.equal(kaks(Anouk), AnoukResult) ) {
   warning("Poor numeric results with respect to AnoukResult standard")
 } else {
   print("Results are consistent with AnoukResult standard")
 }
#
# As from seqinR 2.0-3 the following alignment with out-of-frame gaps
# should return a zero Ka value.
#
# >Reference
# ATGTGGTCGAGATATCGAAAGCTAGGGATATCGATTATATATAGCAAGATCGATAGAGGA
# TCGATGATCGATCGGGATCGACAGCTG
# >With out-of-frame gaps
# AT-TGGTCCAGGTATCGTAAGCTAGGGATATCGATTATATATAGCAAGATCGATAGGGGA
# TCGATGATCGATCGGGA--GACAGCTG
#
# This test example provided by Darren Obbard is now used as a routine check: 
#
 Darren <- read.alignment(file = system.file("sequences/DarrenObbard.fasta", package = "seqinr"),
  format = "fasta")
 stopifnot( all.equal(kaks(Darren)$ka[1], 0) )
#
# As from seqinR 3.4-0, non-finite values should never be returned for
# Ka and Ks even for small sequences. The following test checks that this
# is true for an alignement of the 64 codons, so that we compute Ka and
# Ks for all possible pairs of codons.
#
wrd <- as.alignment(nb = 64, nam = words(), seq = words())
res <- kaks(wrd)
if(any(!is.finite(res$ka))) stop("Non finite value returned for Ka")
if(any(!is.finite(res$ks))) stop("Non finite value returned for Ks")

}
