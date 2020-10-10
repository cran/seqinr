\name{SEQINR.UTIL}
\alias{SEQINR.UTIL}
\docType{data}
\title{utility data for seqinr}
\description{
  This data set gives the genetics code, the name of each codon, the IUPAC one-letter code for aminoacids and the physico-chemical class of amino acid and the pK values of amino acids described in Bjellqvist \emph{et al.} (1993).
}
\format{
 	\code{SEQINR.UTIL} is a list containing the 4 following objects:  
	\describe{	
 	\item{CODES.NCBI}{is a data frame containing the genetics code : The standard ('Universal') genetic code with a selection of non-standard codes. } 
 	\item{CODON.AA}{is a three columns data frame. The first column is a factor containing the codon. The second column is a factor giving the aminoacids names for each codon. The last column is a factor giving the IUPAC one-letter code for aminoacids}
 	\item{AA.PROPERTY}{is a list giving the physico-chemical class of amino acid. The differents classes are the following one : Tiny, Small, Aliphatic, Aromatic, Non.polar, Polar, Charged, Basic, Acidic }
	\item{pK}{is a data frame. It gives the pK values of amino acids described in  Bjellqvist \emph{et al.} (1993) , which were defined by examining polypeptide migration between pH 4.5 to 7.3 in an immobilised pH gradient gel environment with 9.2M and 9.8M urea at 15 degree or 25 degree}
  }
}
\source{
Data prepared by D. Charif.\cr The genetic codes have been taken from the ncbi  database: \url{https://www.ncbi.nlm.nih.gov/Taxonomy/Utils/wprintgc.cgi}. Last visited on 2016-10-05 corresponding to last update of the Genetic Codes: April 30, 2013.\cr The IUPAC one-letter code for aminoacids is descibed at: \url{https://www.bioinformatics.org/sms/iupac.html}. 
 pK values of amino acids were taken from Bjellqvist et al.\cr 
Bjellqvist, B.,Hughes, G.J., Pasquali, Ch., Paquet, N., Ravier, F., Sanchez, J.-Ch., Frutiger, S. & Hochstrasser, D.F.(1993)  The focusing positions of polypeptides in immobilized pH gradients can be predicted from their amino acid sequences.\emph{ Electrophoresis}, \bold{14}, 1023-1031. 
}
\references{
  \code{citation("seqinr")}
}
\examples{
data(SEQINR.UTIL)
}
