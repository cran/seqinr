\name{translate}
\alias{translate}
\title{ Translate nucleic acid sequences }
\description{
   This function translates nucleic acid sequences to the corresponding peptide sequence. It can translate in any of the 3 forward or three reverse sense frames. In the case of reverse sense, the reverse-complement of the sequence is taken. It can translate using the standard (Universal) genetic code and also with a selection of non-standard codes. 
}
\usage{
translate(seq, frame = 0, sens = "F", numcode = 1)
}
\arguments{
  \item{seq}{ an object of class \code{seq}. }
  \item{frame}{ Frame(s) (0,1,2) to translate. By default the frame \code{0} is used. }
  \item{sens}{ Sense to translate: \code{F} for forward sense and \code{R} for revers sense. }
  \item{numcode}{ The number of the code to use. By default the standard genetic code is used. }
}
\details{
 The following genetic codes are described here. The number preceding each code corresponds to \code{numcode}. 
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
  \code{translate} returns a vector of character containing the peptide sequence in the standard one-letter IUPAC code. Termination (STOP) codons are translated as the character '*'.
}
\references{
    The genetic codes have been taken from the ncbi taxonomy database: \url{ http://www.ncbi.nlm.nih.gov/Taxonomy/Utils/wprintgc.cgi?mode=c }. Last update October 05, 2000.\cr
The IUPAC one-letter code for aminoacids is described at: 
\url{http://www.chem.qmul.ac.uk/iupac/AminoAcid/}

To have an overview of the seqinR's functionnality, please consult this vignette: 
Charif, D., Lobry, J.R. (2005) SeqinR: a contributed package to the R project for statistical
computing devoted to biological sequences retrieval and analysis. Springer Verlag, \emph{Biological and Medical Physics/Biomedical Series}, in preparation. \cr
}
\author{ D. Charif }
\seealso{\code{\link{SEQINR.UTIL}} }
\examples{
a=read.fasta(File=system.file("sequences/malM.fasta",package ="seqinr"))
translate(a[[1]])
translate(a[[1]],3,"R",6)
}
\keyword{ manip }
