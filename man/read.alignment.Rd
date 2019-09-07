\name{read.alignment}
\alias{read.alignment}
\title{Read aligned sequence files in mase, clustal, phylip, fasta or msf format}
\description{
 Read a file in \code{mase}, \code{clustal}, \code{phylip}, \code{fasta} or \code{msf} format. 
 These formats are used to store nucleotide or protein multiple alignments. 
}
\usage{
read.alignment(file, format, forceToLower = TRUE, ...)
}
\arguments{
  \item{file}{the name of the file which the aligned sequences are to be read from. 
    If it does not contain an absolute or relative path, the file name is relative 
    to the current working directory, \code{\link{getwd}}. }
  \item{format}{a character string specifying the format of the file : \code{mase}, 
  \code{clustal}, \code{phylip}, \code{fasta} or \code{msf} }
  \item{forceToLower}{a logical defaulting to TRUE stating whether the returned
    characters in the sequence should be in lower case (introduced in seqinR
    release 1.1-3).}
  \item{...}{For the \code{fasta} format, extra arguments are passed to the
    \code{\link{read.fasta}} function.}
}
\details{
  \describe{
   \item{"mase"}{The mase format is used to store nucleotide or protein 
   multiple alignments. The beginning of the file must contain a header 
   containing at least one line (but the content of this header may be 
   empty). The header lines must begin by \code{;;}. The body of the 
   file has the following structure: First, each entry must begin by 
   one (or more) commentary line. Commentary lines begin by the character 
   \code{;}. Again, this commentary line may be empty. After the 
   commentaries, the name of the sequence is written on a separate 
   line. At last, the sequence itself is written on the following lines.
}
   \item{"clustal"}{The CLUSTAL format (*.aln) is the format of the 
   ClustalW multialignment tool output. It can be described as follows. 
   The word CLUSTAL is on the first line of the file. The alignment 
   is displayed in blocks of a fixed length, each line in the block 
   corresponding to one sequence. Each line of each block starts with 
   the sequence name (maximum of 10 characters), followed by at least 
   one space character. The sequence is then displayed in upper or 
   lower cases, '-' denotes gaps. The residue number may be displayed 
   at the end of the first line of each block.
}
  \item{"msf"}{ MSF is the multiple sequence alignment format of the 
  GCG sequence analysis package. It begins with the line (all 
  uppercase) !!NA\_MULTIPLE\_ALIGNMENT 1.0 for nucleic acid sequences 
  or !!AA\_MULTIPLE\_ALIGNMENT 1.0 for amino acid sequences. Do 
  not edit or delete the file type if its present.(optional). 
  A description line which contains informative text describing what 
  is in the file. You can add this information to the top of the MSF 
  file using a text editor.(optional) A dividing line which contains 
  the number of bases or residues in the sequence, when the file was 
  created, and importantly, two dots (..) which act as a divider 
  between the descriptive information and the following sequence 
  information.(required) msf files contain some other information: 
  the Name/Weight, a Separating Line which must include two slashes 
  (//) to divide the name/weight information from the sequence 
  alignment.(required) and the multiple sequence alignment.
}
   \item{"phylip"}{ PHYLIP is a tree construction program. The format 
   is as follows: the number of sequences and their length (in characters) 
   is on the first line of the file. The alignment is displayed in an 
   interleaved or sequential format. The sequence names are limited 
   to 10 characters and may contain blanks. 
}
   \item{"fasta"}{ Sequence in fasta format begins with a single-line 
   description (distinguished by a greater-than (>) symbol), followed 
   by sequence data on the next line.
}
}
}
\value{
 An object of class \code{alignment} which is a list with the following components:
  \item{nb}{ the number of aligned sequences }
  \item{nam}{ a vector of strings containing the names of the aligned sequences } 
  \item{seq}{ a vector of strings containing the aligned sequences} 
  \item{com}{ a vector of strings containing the commentaries for each sequence or \code{NA} if there are no comments }
}
\references{ 
\code{citation("seqinr")}
}
\author{D. Charif, J.R. Lobry}
\seealso{
To read aligned sequences in NEXUS format, see the function 
\code{read.nexus} that was available in the \code{CompPairWise} package
(not sure it is still maintained as of 09/09/09).
The NEXUS format was mainly used by the non-GPL commercial PAUP
software.

Related functions: \code{\link{as.matrix.alignment}}, \code{\link{read.fasta}}, 
\code{\link{write.fasta}}, \code{\link{reverse.align}}, \code{\link{dist.alignment}}.

 }
\examples{
mase.res   <- read.alignment(file = system.file("sequences/test.mase", package = "seqinr"),
 format = "mase")
clustal.res <- read.alignment(file = system.file("sequences/test.aln", package = "seqinr"),
 format="clustal")
phylip.res  <- read.alignment(file = system.file("sequences/test.phylip", package = "seqinr"),
 format = "phylip")
msf.res      <- read.alignment(file = system.file("sequences/test.msf", package = "seqinr"),
 format = "msf")
fasta.res    <- read.alignment(file = system.file("sequences/Anouk.fasta", package = "seqinr"),
 format = "fasta")

#
# Quality control routine sanity checks:
#

data(mase); stopifnot(identical(mase, mase.res))
data(clustal); stopifnot(identical(clustal, clustal.res))
data(phylip); stopifnot(identical(phylip, phylip.res))
data(msf); stopifnot(identical(msf, msf.res))
data(fasta); stopifnot(identical(fasta, fasta.res))

#
# Example of using extra arguments from the read.fasta function, here to keep
# whole headers for sequences names.
#

whole.header.test <- 
 read.alignment(file = system.file("sequences/LTPs128_SSU_aligned_First_Two.fasta", 
 package = "seqinr"), format = "fasta", whole.header = TRUE)
whole.header.test$nam

# Sould be:
#
# [1] "D50541\t1\t1411\t1411bp\trna\tAbiotrophia defectiva\tAerococcaceae"      
# [2] "KP233895\t1\t1520\t1520bp\trna\tAbyssivirga alkaniphila\tLachnospiraceae"
#
}

