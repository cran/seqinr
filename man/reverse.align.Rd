\name{reverse.align}
\alias{reverse.align}
\title{ Reverse alignment - from protein sequence alignment to nucleic sequence alignment }
\description{
  This function produces an alignment of nucleic protein-coding sequences, using as a
  guide the alignment of the corresponding protein sequences. 
}
\usage{
reverse.align(nucl.file, protaln.file, input.format = 'fasta', out.file,
  output.format = 'fasta', align.prot = FALSE, numcode = 1,
  clustal.path = NULL, forceDNAtolower = TRUE, forceAAtolower = FALSE)
}
\arguments{
  \item{nucl.file}{ A character string specifying the name of the FASTA format file containing the nucleotide sequences. }
  \item{protaln.file}{A character string specifying the name of the file containing the aligned
    protein sequences. This argument must be provided if \code{align.prot} is
    set to \code{FALSE}. }
  \item{input.format}{ A character string specifying the format of the
    protein alignment file : 'mase', 'clustal', 'phylip', 'fasta' or 'msf'.  }
  \item{out.file}{A character string specifying the name of the output file. }
  \item{output.format}{ A character string specifying the format of the output file. Currently the only
    implemented format is 'fasta'. }
  \item{align.prot}{Boolean. If TRUE, the nucleic sequences are
    translated and then the protein sequences are aligned with the ClustalW program. The path
    of the ClustalW binary must also be given (\code{clustal.path}) }
  \item{numcode}{The NCBI genetic code number for the translation of the
  nucleic sequences. By default the standard genetic code is used.}
  \item{clustal.path}{ The path of the ClustalW binary. This argument
    only needs to be setif \code{align.prot} is TRUE. }
  \item{forceDNAtolower}{logical passed to \code{\link{read.fasta}} for reading 
     the nucleic acid file.}
  \item{forceAAtolower}{logical passed to \code{\link{read.alignment}} for reading 
     the aligned protein sequence file.}
}
\details{
  This function an alignment of nucleic protein-coding sequences using as a
  guide the alignment of the corresponding protein sequences. The file containing
  the nucleic sequences is given in the compulsory argument 'nucl.file';
  this file must be written in the FASTA format.

  The alignment of the protein sequences can either be provided
  directly, trough the 'protaln.file' parameter, or reconstructed with
  ClustalW, if the parameter 'align.prot' is set to TRUE. In the latter
  case, the pathway of the ClustalW binary must be given in the
  'clustal.path' argument. 
  
  The protein and nucleic sequences must have the same name in the files
  \code{nucl.file} and \code{protaln.file}.

  The reverse-aligned nucleotide sequences are written to the file
  specified in the compulsory 'out.file' argument. For now, the only
  output format implemented is FASTA.

  Warning: the 'align.prot=TRUE' option has only been tested on LINUX
  operating systems. ClustalW must be installed on your system in order
  for this to work.  
}
\value{
 NULL
}
\references{
  \code{citation('seqinr')}
}
\author{A. Necşulea}
\seealso{  \code{\link{read.alignment}}, \code{\link{read.fasta}}, \code{\link{write.fasta}}}

\examples{

#
# Read example 'bordetella.fasta': a triplet of orthologous genes from
# three bacterial species (Bordetella pertussis, B. parapertussis and
# B. bronchiseptica):
#

nucl.file <- system.file('sequences/bordetella.fasta', package = 'seqinr')
triplet <- read.fasta(nucl.file)

# 
# For this example, 'bordetella.pep.aln' contains the aligned protein
# sequences, in the Clustal format:
#

protaln.file <- system.file('sequences/bordetella.pep.aln', package = 'seqinr')
triplet.pep<- read.alignment(protaln.file, format = 'clustal')

#
# Call reverse.align for this example:
#
myOutFileName <-tempfile(pattern = "test", tmpdir = tempdir(), fileext = "revalign")
tempdir(check = FALSE)

#reverse.align(nucl.file = nucl.file, protaln.file = protaln.file,
#                     input.format = 'clustal', out.file = 'test.revalign')

reverse.align(nucl.file = nucl.file, protaln.file = protaln.file,
                     input.format = 'clustal', out.file = myOutFileName)

#
# Simple sanity check against expected result:
#

#res.new <- read.alignment("test.revalign", format = "fasta")

res.new <- read.alignment(myOutFileName, format = "fasta")
data(revaligntest)
stopifnot(identical(res.new, revaligntest))

#
# Alternatively, we can use ClustalW to align the translated nucleic
# sequences. Here the ClustalW program is accessible simply by the
# 'clustalw' name.
#

\dontrun{
reverse.align(nucl.file = nucl.file, out.file = 'test.revalign.clustal', 
  align.prot = TRUE, clustal.path = 'clustalw')}
}
\keyword{ manip }
