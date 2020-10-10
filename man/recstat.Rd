\name{recstat}
\alias{recstat}
\title{Prediction of Coding DNA Sequences.}
\description{This function aims at predicting the position of Coding DNA Sequences (CDS) through
the use of a Correspondence Analysis (CA) computed on codon composition, this for the three
reading frames of a DNA strand.
}
\usage{recstat(seq, sizewin = 90, shift = 30, seqname = "no name")}
\arguments{
    \item{seq}{a nucleic acid sequence as a vector of characters}
    \item{sizewin}{an integer, multiple of 3,  giving the length of the sliding window}
    \item{shift}{an integer, multiple of 3, giving the length of the steps between two windows}
    \item{seqname}{the name of the sequence}
}
\details{The method is built on the hypothesis that the codon composition of a CDS is biased
while it is not the case outside these regions. In order to detect such bias, a CA on codon
frequencies is computed on the six possible reading frames of a DNA sequence (three from the
direct strand and three from the reverse strand). When there is a CDS in one of the reading
frame, it is expected that the CA factor scores observed in this frame (fot both rows and
columns) will be significantly different from those in the two others.}
\value{This function returns a list containing the following components:\cr
    \item{seq}{a single DNA sequence as a vector of characters}
    \item{sizewin}{length of the sliding window}
    \item{shift}{length of the steps between windows}
    \item{seqsize}{length of the sequence}
    \item{seqname}{name of the sequence}
    \item{vdep}{a vector containing the positions of windows starts}
    \item{vind}{a vector containing the reading frame of each window}
    \item{vstopd}{a vector of stop codons positions in direct strand}
    \item{vstopr}{a vector of stop codons positions in reverse strand}
    \item{vinitd}{a vector of start codons positions in direct strand}
    \item{vinitr}{a vector of start codons positions in reverse strand}
    \item{resd}{a matrix containing codons frequencies for all the windows in the three frames
        of the direct strand}
    \item{resr}{a matrix containing codons frequencies for all the windows in the three frames
        of the reverse strand}
    \item{resd.coa}{list of class \code{coa} and \code{dudi} containing the result of the
        CA computed on the codons frequencies in the direct strand}
    \item{resr.coa}{list of class \code{coa} and \code{dudi} containing the result of the
        CA computed on the codons frequencies in the reverse strand}
}
\note{This method works only with DNA sequences long enough to obtain a sufficient number
of windows. As the optimal windows length has been estimated to be 90 bp by Fichant and
Gautier (1987), the minimal sequence length is around 500 bp. The method can be used on
prokaryotic and eukaryotic sequences. Also, only the four first factors of the CA are kept.
Indeed, most of the time, only the first factor is relevant in order to detect CDS.
}
\author{O. Clerc, G. Perrière}
\references{The original paper describing recstat is:\cr

Fichant, G., Gautier, C. (1987) Statistical method for predicting protein coding
regions in nucleic acid sequences. \emph{Comput. Appl. Biosci.}, \bold{3}, 287--295.\cr
\url{https://academic.oup.com/bioinformatics/article-abstract/3/4/287/218186}\cr
}
\seealso{\code{\link{draw.recstat}}, \code{\link{test.li.recstat}}, \code{\link{test.co.recstat}}}
\examples{
ff <- system.file("sequences/ECOUNC.fsa", package = "seqinr")
seq <- read.fasta(ff)
rec <- recstat(seq[[1]], seqname = getName(seq))
}
\keyword{correspondence analysis}
\keyword{sequence}
