\name{translate}
\alias{translate}
\title{ Translate nucleic acid sequences }
\description{
   This function translates nucleic acid sequences into the corresponding 
   peptide sequence. It can translate in any of the 3 forward or three 
   reverse sense frames. In the case of reverse sense, the reverse-complement 
   of the sequence is taken. It can translate using the standard (universal) 
   genetic code and also with non-standard codes. 
}
\usage{
translate(seq, frame = 0, sens = "F", numcode = 1)
}
\arguments{
  \item{seq}{ an object of class \code{seq}. }
  \item{frame}{ Frame(s) (0,1,2) to translate. By default the frame \code{0} is used. }
  \item{sens}{ Sense to translate: \code{F} for forward sense and \code{R} for reverse sense. }
  \item{numcode}{ The ncbi genetic code number for translation. By default the standard genetic code is used. }
}
\details{
 The following genetic codes are described here. The number preceding each code 
 corresponds to \code{numcode}. 
\itemize{	
  \item{1}{ standard }
  \item{2}{ vertebrate.mitochondrial }
  \item{3}{ yeast.mitochondrial }
  \item{4}{ protozoan.mitochondrial+mycoplasma }
  \item{5}{ invertebrate.mitochondrial }
  \item{6}{ ciliate+dasycladaceal }
  \item{9}{ echinoderm+flatworm.mitochondrial }
  \item{10}{ euplotid }
  \item{11}{ bacterial+plantplastid }
  \item{12}{ alternativeyeast }
  \item{13}{ ascidian.mitochondrial } 
  \item{14}{ alternativeflatworm.mitochondrial }
  \item{15}{ blepharism }
  \item{16}{ chlorophycean.mitochondrial }
  \item{21}{ trematode.mitochondrial }
  \item{22}{ scenedesmus.mitochondrial }
  \item{23}{ hraustochytrium.mitochondria }
} 
}
\value{
  \code{translate} returns a vector of character containing the peptide sequence in 
  the standard one-letter IUPAC code. Termination (STOP) codons are translated by 
  the character '*'.
}
\references{
The genetic codes have been taken from the ncbi taxonomy database:
\url{ http://www.ncbi.nlm.nih.gov/Taxonomy/Utils/wprintgc.cgi?mode=c }. 
Last update October 05, 2000.\cr
The IUPAC one-letter code for aminoacids is described at: 
\url{http://www.chem.qmul.ac.uk/iupac/AminoAcid/}

\code{citation("seqinr")}
}
\author{ D. Charif, J.R. Lobry }
\seealso{
For coding sequences obtained from an ACNUC server with \code{\link{query}} it's
better to use the function \code{\link{getTrans}} so that the relevant genetic
code and the relevant frame are automatically used.
The genetic codes are given in the object \code{\link{SEQINR.UTIL}}, a more
human readable form is given by the function \code{\link{tablecode}}. }
\examples{
##
## Toy cds example invented by Leonor Palmeira:
##
toycds <- s2c("tctgagcaaataaatcgg")
translate(seq = toycds) # should be c("S", "E", "Q", "I", "N", "R")
##
## Real cds example:
##
realcds <- read.fasta(File = system.file("sequences/malM.fasta", package ="seqinr"))[[1]]
translate(seq = realcds)
# Biologically correct, only one stop codon at the end
translate(seq = realcds, frame = 3, sens = "R", numcode = 6)
# Biologically meaningless, note the in-frame stop codons

\dontrun{
## Need internet connection.
## Translation of the following genbank entry:
##
## AE003734.PE35        Location/Qualifiers    (length=1833 bp)
##      CDS             join(complement(162997..163210),
##                      complement(162780..162919),complement(161238..162090),
##                      146568..146732,146806..147266)
choosebank("genbank")
query("trans", "N=AE003734.PE35")
getTrans(trans$req[[1]])
## Complex transsplicing operations, the correct frame and the correct 
## genetic code are automatically used for translation into protein.
}
}
\keyword{ manip }
