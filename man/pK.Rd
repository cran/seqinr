\name{pK}
\alias{pK}
\docType{data}
\title{pK values for the side chain of charged amino acids from various sources}
\description{
This compilation of pK values is from Joanna Kiraga (2008).
}
\usage{data(pK)}
\format{
  A data frame with the seven charged amino-acid in row and
  six sources in column. The rownames are the one-letter code
  for amino-acids.
}
\source{
Table 2 in Kiraga (2008).
}
\references{
Kiraga, J. (2008) Analysis and computer simulations of variability of
isoelectric point of proteins in the proteomes. PhD thesis, University
of Wroclaw, Poland.

Bjellqvist, B., Hughes, G.J., Pasquali, Ch., Paquet, N., Ravier, F., Sanchez,  J.Ch.,
Frutige,r S., Hochstrasser D. (1993) The focusing positions of polypeptides in
immobilized pH gradients can be predicted from their amino acid sequences.
\emph{Electrophoresis}, \bold{14}:1023-1031.

EMBOSS data were from release 5.0 and were still the same in release 6.6
\url{http://emboss.sourceforge.net/apps/release/6.6/emboss/apps/iep.html}
last visited 2016-06-03.

Murray, R.K., Granner, D.K., Rodwell, V.W. (2006)
\emph{Harper's illustrated Biochemistry.}
27th edition. Published by The McGraw-Hill Companies.

Sillero, A., Maldonado, A. (2006) Isoelectric point determination of proteins
and other macromolecules: oscillating method. 
\emph{Comput Biol Med.}, \bold{36}:157-166.

Solomon, T.W.G. (1998) \emph{Fundamentals of Organic Chemistry}, 5th edition.
Published by Wiley.

Stryer L. (1999) \emph{Biochemia}. czwarta edycja. Wydawnictwo Naukowe PWN.

\code{citation("seqinr")}
}

\examples{
data(pK)
data(SEQINR.UTIL) # for N and C terminal pK values
prot <- s2c("ACDEFGHIKLMNPQRSTVWY")
compoAA <- table(factor(prot, levels = LETTERS))
nTermR <- which(LETTERS == prot[1])
cTermR <- which(LETTERS == prot[length(seq)])

computeCharge <- function(pH, compoAA, pK, nTermResidue, cTermResidue){
  cter <- 10^(-SEQINR.UTIL$pk[cTermResidue,1]) / 
     (10^(-SEQINR.UTIL$pk[cTermResidue,1]) + 10^(-pH))
  nter <- 10^(-pH) / (10^(-SEQINR.UTIL$pk[nTermResidue,2]) + 10^(-pH))
  carg <- as.vector(compoAA['R'] * 10^(-pH) / (10^(-pK['R']) + 10^(-pH)))
  chis <- as.vector(compoAA['H'] * 10^(-pH) / (10^(-pK['H']) + 10^(-pH)))
  clys <- as.vector(compoAA['K'] * 10^(-pH) / (10^(-pK['K']) + 10^(-pH)))
  casp <- as.vector(compoAA['D'] * 10^(-pK['D']) /(10^(-pK['D']) + 10^(-pH)))
  cglu <- as.vector(compoAA['E'] * 10^(-pK['E']) / (10^(-pK['E']) + 10^(-pH)))
  ccys <- as.vector(compoAA['C'] * 10^(-pK['C']) / (10^(-pK['C']) + 10^(-pH)))
  ctyr <- as.vector(compoAA['Y'] * 10^(-pK['Y']) / (10^(-pK['Y']) + 10^(-pH)))
  charge <- carg + clys + chis + nter - (casp + cglu + ctyr + ccys + cter)
  return(charge)
}

pHseq <- seq(from = 0, to = 14, by = 0.1)
Bje <- pK$Bjellqvist
names(Bje) <- rownames(pK)
res <- computeCharge(pHseq, compoAA, Bje, nTermR, cTermR)
plot(pHseq, res, type = "l", ylab = "Charge", las = 1,
  main = paste("Charge of protein\n",c2s(prot)),
  xlab = "pH")
for(j in 2:ncol(pK)){
  src <- pK[,j]
  names(src) <- rownames(pK)
  res <- computeCharge(pHseq, compoAA, src, nTermR, cTermR)
  lines(pHseq, res, lty = j, col = rainbow(5)[j])
}

abline(h=0)
abline(v=computePI(prot))
legend("bottomleft", inset = 0.01, colnames(pK), lty = 1:6, col = c("black", rainbow(5)))
}
\keyword{datasets}
