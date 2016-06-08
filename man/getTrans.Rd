\name{getTrans}
\alias{getTrans}
\alias{getTrans.default}
\alias{getTrans.list}
\alias{getTrans.character}
\alias{getTrans.SeqFrag}
\alias{getTrans.SeqAcnucWeb}
\alias{getTrans.SeqFastadna}
\alias{getTrans.qaw}
\alias{getTrans.logical}

\title{Generic function to translate coding sequences into proteins}
\description{
This function translates nucleic acid sequences into the corresponding peptide sequence. It can translate in any of the 3 forward or three reverse sense frames. In the case of reverse sense, the reverse-complement of the sequence is taken. It can translate using the standard (universal) genetic code and also with non-standard codes. Ambiguous bases can also be handled.
}
\usage{
getTrans(object, sens = "F", NAstring = "X", ambiguous = FALSE, ...)
\method{getTrans}{SeqAcnucWeb}(object, sens = "F", NAstring = "X", ambiguous = FALSE, ...,
 frame = "auto", numcode = "auto")
\method{getTrans}{SeqFastadna}(object, sens = "F", NAstring = "X", ambiguous = FALSE, ...,
 frame = 0, numcode = 1)
\method{getTrans}{SeqFrag}(object, sens = "F", NAstring = "X", ambiguous = FALSE, ...,
 frame = 0, numcode = 1)

}
\arguments{
  \item{object}{ an object of the class \code{\link{SeqAcnucWeb}}
    or \code{\link{SeqFastadna}}, or \code{\link{SeqFrag}} or a list of these objects, or an object of class \code{qaw} created by \code{\link{query}} }
  \item{numcode}{ The ncbi genetic code number for translation. By default the standard genetic code is used, and for sequences
    coming from an ACNUC server the relevant genetic code is used by default. }
  \item{NAstring}{ How to translate amino-acids when there are ambiguous bases in codons. }
  \item{ambiguous}{ If TRUE, ambiguous bases are taken into account so that for instance
  GGN is translated to Gly in the standard genetic code. }
  \item{frame}{ Frame(s) (0,1,2) to translate. By default the frame \code{0} is used. }
  \item{sens}{ Direction for translation: \code{F} for the direct strand e and \code{R} for the reverse complementary strand. }
  \item{...}{further arguments passed to or from other methods}
}
\details{
 The following genetic codes are described here. The number preceding each code 
 corresponds to \code{numcode}. 
  \describe{
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
  For a single sequence an object of class \code{character} containing the characters
  of the sequence, either of length 1 when \code{as.string} is TRUE, or of the length
  of the sequence when \code{as.string} is FALSE. For many sequences, a list of these.}
\references{
  \code{citation("seqinr")}
}
\author{D. Charif, J.R. Lobry, L. Palmeira} 
\seealso{
  \code{\link{SeqAcnucWeb}}, \code{\link{SeqFastadna}}, \code{\link{SeqFrag}}\cr
  The genetic codes are given in the object \code{\link{SEQINR.UTIL}}, a more
  human readable form is given by the function \code{\link{tablecode}}. 
  Use \code{\link{aaa}} to get the three-letter code for amino-acids.
}
\examples{
#
# List all available methods for getTrans generic function:
#
   methods(getTrans)
#
# Toy CDS example invented by Leonor Palmeira:
#
  toycds <- s2c("tctgagcaaataaatcgg")
  getTrans(toycds) # should be c("S", "E", "Q", "I", "N", "R")
#
# Toy CDS example with ambiguous bases:
#
  toycds2 <- s2c("tcngarcarathaaycgn")
  getTrans(toycds2) # should be c("X", "X", "X", "X", "X", "X")
  getTrans(toycds2, ambiguous = TRUE) # should be c("S", "E", "Q", "I", "N", "R")
  getTrans(toycds2, ambiguous = TRUE, numcode = 2) # should be c("S", "E", "Q", "X", "N", "R")
#
# Real CDS example:
#
  realcds <- read.fasta(file = system.file("sequences/malM.fasta", package ="seqinr"))[[1]]
  getTrans(realcds)
# Biologically correct, only one stop codon at the end
  getTrans(realcds, frame = 3, sens = "R", numcode = 6)
# Biologically meaningless, note the in-frame stop codons

#
# Complex transsplicing operations, the correct frame and the correct 
# genetic code are automatically used for translation into protein for
# sequences coming from an ACNUC server:
#
\dontrun{
  # Need internet connection.
  # Translation of the following EMBL entry:
  #
  # FT   CDS             join(complement(153944..154157),complement(153727..153866),
  # FT                   complement(152185..153037),138523..138735,138795..138955)
  # FT                   /codon_start=1
  choosebank("emblTP")
  trans <- query("trans", "N=AE003734.PE35")
  getTrans(trans$req[[1]])
}
}
\keyword{ utilities }
