\name{uco}
\alias{uco}
\title{ Codon usage indices }
\description{
  \code{uco} calculates some codon usage indices: the codon counts \code{eff}, the relative frequencies \code{freq} or the Relative Synonymous Codon Usage \code{rscu}.
}
\usage{
uco(seq, frame = 0, index = c("eff", "freq", "rscu"), as.data.frame = FALSE,
NA.rscu = NA) 
}
\arguments{
  \item{seq}{ a coding sequence as a vector of chars }
  \item{frame}{ an integer (0, 1, 2) giving the frame of the coding sequence }
  \item{index}{ codon usage index choice, partial matching is allowed. 
                \code{eff} for codon counts, 
                \code{freq} for codon relative frequencies, 
                and \code{rscu} the RSCU index}
  \item{as.data.frame}{ logical. If \code{TRUE}: all indices are returned into a data frame.}
  \item{NA.rscu}{ when an amino-acid is missing, RSCU are no more defined and repported
  as missing values (\code{NA}). You can force them to another value (typically 0 or
  1) with this argument.}
}
\details{
  Codons with ambiguous bases are ignored.\cr
  
  RSCU is a simple measure of non-uniform usage of synonymous codons in a coding sequence
  (Sharp \emph{et al.} 1986).
  RSCU values are the number of times a particular codon is observed, relative to the number 
  of times that the codon would be observed for a uniform synonymous codon usage (i.e. all the
  codons for a given amino-acid have the same probability).
  In the absence of any codon usage bias, the RSCU values would be 1.00 (this is the case
  for sequence \code{cds} in the exemple thereafter). A codon that is used
  less frequently than expected will have an RSCU value of less than 1.00 and vice versa for a codon 
  that is used more frequently than expected.\cr
  
  Do not use correspondence analysis on RSCU tables as this is a source of artifacts 
  (Perriere and Thioulouse 2002). Within-aminoacid correspondence analysis is a
  simple way to study synonymous codon usage (Charif \emph{et al.} 2005).
  
  If \code{as.data.frame} is FALSE, \code{uco} returns one of these:
  \describe{
  \item{ eff }{ a table of codon counts }
  \item{ freq }{ a table of codon relative frequencies }
  \item{ rscu }{ a numeric vector of relative synonymous codon usage values}
  }
  If \code{as.data.frame} is TRUE, \code{uco} returns a data frame with five columns:
  \describe{
  \item{ aa }{ a vector containing the name of amino-acid }
  \item{ codon }{ a vector containing the corresponding codon }
  \item{ eff }{ a numeric vector of codon counts }
  \item{ freq }{ a numeric vector of codon relative frequencies }
  \item{ rscu }{ a numeric vector of RSCU index }
  }  
}
\value{
  If \code{as.data.frame} is FALSE, the default, a table for \code{eff} and \code{freq} and
  a numeric vector for \code{rscu}. If \code{as.data.frame} is TRUE,
  a data frame with all indices is returned.  
}
\references{
\code{citation("seqinr")} \cr

Sharp, P.M., Tuohy, T.M.F., Mosurski, K.R. (1986) Codon usage in yeast: cluster
analysis clearly differentiates highly and lowly expressed genes.
\emph{Nucl. Acids. Res.}, \bold{14}:5125-5143.\cr

Perriere, G., Thioulouse, J. (2002) Use and misuse of correspondence analysis in
codon usage studies. \emph{Nucl. Acids. Res.}, \bold{30}:4548-4555.\cr

Charif, D., Thioulouse, J., Lobry, J.R., Perriere, G. (2005) Online 
Synonymous Codon Usage Analyses with the ade4 and seqinR packages. 
\emph{Bioinformatics}, \bold{21}:545-547. \url{http://pbil.univ-lyon1.fr/members/lobry/repro/bioinfo04/}.
}
\author{ D. Charif, J.R. Lobry, G. Perriere }
\examples{

## Show all possible codons:
words()

## Make a coding sequence from this:
(cds <- s2c(paste(words(), collapse = "")))

## Get codon counts:
uco(cds, index = "eff")

## Get codon relative frequencies:
uco(cds, index = "freq")

## Get RSCU values:
uco(cds, index = "rscu")

## Show what happens with ambiguous bases:
uco(s2c("aaannnttt"))

## Use a real coding sequence:
rcds <- read.fasta(file = system.file("sequences/malM.fasta", package = "seqinr"))[[1]]
uco( rcds, index = "freq")
uco( rcds, index = "eff")
uco( rcds, index = "rscu")
uco( rcds, as.data.frame = TRUE)

## Show what happens with RSCU when an amino-acid is missing:
ecolicgpe5 <- read.fasta(file = system.file("sequences/ecolicgpe5.fasta",package="seqinr"))[[1]]
uco(ecolicgpe5, index = "rscu")

## Force NA to zero:
uco(ecolicgpe5, index = "rscu", NA.rscu = 0)
}
\keyword{ manip }
