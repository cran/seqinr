\name{oriloc}
\alias{oriloc}
\title{ Prediction of origin and terminus of replication in bacteria }
\description{
This program finds the putative origin and terminus of 
replication in procaryotic genomes. The program works with
unannotated sequences and therefore uses glimmer2 outputs to 
discriminate between codon positions.
}
\usage{
oriloc(seq.fasta = system.file("sequences/ct.fasta", package ="seqinr"),
 g2.coord = system.file("sequences/ct.coord", package = "seqinr"),
oldoriloc = FALSE, gbk = NULL, clean.tmp.files = TRUE, rot = 0)
}
\arguments{
  \item{seq.fasta}{ the name of a file which contains the dna sequence
of a bacterial chromosome in fasta format }
  \item{g2.coord}{ the name of file which contains the output of
glimmer2 program}
  \item{oldoriloc}{ logical to be set at TRUE to reproduce the
(deprecated) outputs of previous (publication date: 2000) version 
of the oriloc program }
  \item{gbk}{ the URL of a file in GenBank format}
  \item{clean.tmp.files}{ Logical, if TRUE temporary files are removed }
  \item{rot}{ Integer, with zero default value, used to permute circurlarly the genome. }
}
\details{
The method builds on the fact that there are compositional asymmetries between
the leading and the lagging strand for replication. The program works with
unannotated sequences in fasta format and therefore uses glimmer2.0 outputs to
discriminate between codon positions so as to increase the signal/noise ratio.
}
\value{
  A data.frame with seven columns: \code{g2num} for the CDS number in
the \code{g2.coord} file, \code{start.kb} for the start position of CDS
expressed in Kb (this is the position of the first occurence of a
nucleotide in a CDS \emph{regardless} of its orientation), \code{end.kb}
for the last position of a CDS, \code{CDS.excess} for the DNA walk for
gene orientation (+1 for a CDS in the direct strand, -1 for a CDS in
the reverse strand) cummulated over genes, \code{skew} for the cummulated
composite skew in third codon positions, \code{x} for the cummulated
T - A skew in third codon position, \code{y} for the cummulated C - G
skew in third codon positions.
}
\references{ 

The original paper for oriloc:\cr
Frank, A.C., Lobry, J.R. (2000) Oriloc: prediction of replication
boundaries in unannotated bacterial chromosomes. \emph{Bioinformatics}, 
\bold{16}:566-567.\cr
\url{http://bioinformatics.oupjournals.org/cgi/reprint/16/6/560}\cr\cr

A simple informal introduction to DNA-walks:\cr
Lobry, J.R. (1999) Genomic landscapes. \emph{Microbiology Today},
\bold{26}:164-165.\cr
\url{http://www.socgenmicrobiol.org.uk/QUA/049906.pdf}\cr\cr

An early and somewhat historical application of DNA-walks:\cr
Lobry, J.R. (1996) A simple vectorial representation of DNA sequences 
for the detection of replication origins in bacteria. \emph{Biochimie},
\bold{78}:323-326.

To have an overview of the seqinR's functionnality, please consult this vignette: 
Charif, D., Lobry, J.R. (2005) SeqinR: a contributed package to the R project for statistical
computing devoted to biological sequences retrieval and analysis. Springer Verlag, \emph{Biological and Medical Physics/Biomedical Series}, in preparation. \cr
}
\author{ J.R. Lobry and A.C. Frank}
\note{ The method works only for genomes having a single origin of replication 
from which the replication is bidirectional. To detect the composition changes,
a DNA-walk is performed. In a 2-dimensional DNA walk, a C in the sequence 
corresponds to the movement in the positive y-direction and G to a movement 
in the negative y-direction. T and A are mapped by analogous steps along the 
x-axis. When there is a strand asymmetry, this will form a trajectory that 
turns at the origin and terminus of replication. Each step is the sum of 
nucleotides in a gene in third codon positions. Then ortogonal regression is 
used to find a line through this trajectory. Each point in the trajectory will 
have a corresponding point on the line, and the coordinates of each are 
calculated. Thereafter, the distances from each of these points to the origin
(of the plane), are calculated. These distances will represent a form of 
cumulative skew. This permets us to make a plot with the gene position (gene 
number, start or end position) on the x-axis and the cumulative skew (distance)
at the y-axis. Depending on where the sequence starts, such a plot will display 
one or two peaks. Positive peak means origin, and negative means terminus. 
In the case of only one peak, the sequence starts at the origin or terminus 
site. }
\examples{
  \dontrun{  out <- oriloc() }
  \dontrun{  plot(out$st, out$sk, type="l", xlab="Map position in Kb",
    ylab = "Cumulated composite skew", 
    main=expression(italic(Chlamydia~~trachomatis)~~complete~~genome))}
}
\keyword{utilities}
