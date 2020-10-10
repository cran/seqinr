\name{translate}
\alias{translate}
\title{ Translate nucleic acid sequences into proteins }
\description{
   This function translates nucleic acid sequences into the corresponding 
   peptide sequence. It can translate in any of the 3 forward or three 
   reverse sense frames. In the case of reverse sense, the reverse-complement 
   of the sequence is taken. It can translate using the standard (universal) 
   genetic code and also with non-standard codes. Ambiguous bases can also
   be handled.
}
\usage{
translate(seq, frame = 0, sens = "F", numcode = 1, NAstring = "X", ambiguous = FALSE)
}
\arguments{
  \item{seq}{ the sequence to translate as a vector of single characters in lower case letters. }
  \item{frame}{ Frame(s) (0,1,2) to translate. By default the frame \code{0} is used. }
  \item{sens}{ Sense to translate: \code{F} for forward sense and \code{R} for reverse sense. }
  \item{numcode}{ The ncbi genetic code number for translation. By default the standard genetic code is used. }
  \item{NAstring}{ How to translate amino-acids when there are ambiguous bases in codons. }
  \item{ambiguous}{ If TRUE, ambiguous bases are taken into account so that for instance
  GGN is translated to Gly in the standard genetic code. }
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
  \item{23}{ thraustochytrium.mitochondria }
  \item{24}{Pterobranchia.mitochondrial}
  \item{25}{CandidateDivision.SR1+Gracilibacteria}
  \item{26}{Pachysolen.tannophilus}
}
}
\value{
  \code{translate} returns a vector of single characters containing the peptide sequence in 
  the standard one-letter IUPAC code. Termination (STOP) codons are translated by 
  the character '*'.
}
\references{
The genetic codes have been taken from the ncbi taxonomy database:
\url{https://www.ncbi.nlm.nih.gov/Taxonomy/Utils/wprintgc.cgi}. 
Last update October 05, 2000.\cr
The IUPAC one-letter code for aminoacids is described at: 
\url{https://www.bioinformatics.org/sms/iupac.html}

\code{citation("seqinr")}
}
\author{D. Charif, J.R. Lobry}
\seealso{
Use \code{\link{tolower}} to change upper case letters into lower case letters.
For coding sequences obtained from an ACNUC server with \code{\link{query}} it's
better to use the function \code{\link{getTrans}} so that the relevant genetic
code and the relevant frame are automatically used.
The genetic codes are given in the object \code{\link{SEQINR.UTIL}}, a more
human readable form is given by the function \code{\link{tablecode}}. 
Use \code{\link{aaa}} to get the three-letter code for amino-acids.}
\examples{
##
## Toy CDS example invented by Leonor Palmeira:
##
toycds <- s2c("tctgagcaaataaatcgg")
translate(seq = toycds) # should be c("S", "E", "Q", "I", "N", "R")
##
## Toy CDS example with ambiguous bases:
##
toycds2 <- s2c("tcngarcarathaaycgn")
translate(toycds2) # should be c("X", "X", "X", "X", "X", "X")
translate(toycds2, ambiguous = TRUE) # should be c("S", "E", "Q", "I", "N", "R")
translate(toycds2, ambiguous = TRUE, numcode = 2) # should be c("S", "E", "Q", "X", "N", "R")
##
## Real CDS example:
##
realcds <- read.fasta(file = system.file("sequences/malM.fasta", package ="seqinr"))[[1]]
translate(seq = realcds)
# Biologically correct, only one stop codon at the end
translate(seq = realcds, frame = 3, sens = "R", numcode = 6)
# Biologically meaningless, note the in-frame stop codons

\dontrun{
## Need internet connection.
## Translation of the following EMBL entry:
##
## FT   CDS             join(complement(153944..154157),complement(153727..153866),
## FT                   complement(152185..153037),138523..138735,138795..138955)
## FT                   /codon_start=1
## FT                   /db_xref="FLYBASE:FBgn0002781"
## FT                   /db_xref="GOA:Q86B86"
## FT                   /db_xref="TrEMBL:Q86B86"
## FT                   /note="mod(mdg4) gene product from transcript CG32491-RZ;
## FT                   trans splicing"
## FT                   /gene="mod(mdg4)"
## FT                   /product="CG32491-PZ"
## FT                   /locus_tag="CG32491"
## FT                   /protein_id="AAO41581.1"
## FT                   /translation="MADDEQFSLCWNNFNTNLSAGFHESLCRGDLVDVSLAAEGQIVKA
## FT                   HRLVLSVCSPFFRKMFTQMPSNTHAIVFLNNVSHSALKDLIQFMYCGEVNVKQDALPAF
## FT                   ISTAESLQIKGLTDNDPAPQPPQESSPPPAAPHVQQQQIPAQRVQRQQPRASARYKIET
## FT                   VDDGLGDEKQSTTQIVIQTTAAPQATIVQQQQPQQAAQQIQSQQLQTGTTTTATLVSTN
## FT                   KRSAQRSSLTPASSSAGVKRSKTSTSANVMDPLDSTTETGATTTAQLVPQQITVQTSVV
## FT                   SAAEAKLHQQSPQQVRQEEAEYIDLPMELPTKSEPDYSEDHGDAAGDAEGTYVEDDTYG
## FT                   DMRYDDSYFTENEDAGNQTAANTSGGGVTATTSKAVVKQQSQNYSESSFVDTSGDQGNT
## FT                   EAQVTQHVRNCGPQMFLISRKGGTLLTINNFVYRSNLKFFGKSNNILYWECVQNRSVKC
## FT                   RSRLKTIGDDLYVTNDVHNHMGDNKRIEAAKAAGMLIHKKLSSLTAADKIQGSWKMDTE
## FT                   GNPDHLPKM"
choosebank("emblTP")
trans <- query("trans", "N=AE003734.PE35")
getTrans(trans$req[[1]])
## Complex transsplicing operations, the correct frame and the correct 
## genetic code are automatically used for translation into protein.
}
}
\keyword{ manip }
