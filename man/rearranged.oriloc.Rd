\name{rearranged.oriloc}
\alias{rearranged.oriloc}
\title{Detection of replication-associated effects on base composition asymmetry in
  prokaryotic chromosomes.}
\description{
  Detection of replication-associated effects on base composition asymmetry in
  prokaryotic chromosomes.
  }

\usage{
rearranged.oriloc(seq.fasta = system.file("sequences/ct.fasta.gz", package = "seqinr"),
  g2.coord = system.file("sequences/ct.predict", package = "seqinr"))
}

\arguments{
\item{seq.fasta}{The path of the file containing a FASTA-format
  sequence. Default value: 
  the FASTA sequence of the Chlamydia trachomatis chromosome. }
\item{g2.coord}{The path of the file containing the coordinates of the
  protein coding genes found on this chromosome. This file can be
  obtained using the function \code{gbk2g2}. The format of the file is
  similar to the output of the Glimmer2 program. The first column
  contains the index or the name of the gene, the second one contains
  the start position and the third column contains the end position. For
  reverse transcribed genes, the start position is greater than the end position.}
}
\details{
The purpose of this method is to decouple replication-related
and coding sequence-related  effects on base composition asymmetry. In
order to do so, the analyzed chromosome is artificially rearranged to
obtain a perfect gene orientation bias - all forward transcribed genes
on the first half of the chromosome, and all reverse transcribed genes
on the other half.
This rearrangement conserves the relative order of genes within each of
the two groups  - both forward-encoded and reverse-encoded genes are
placed on the rearranged chromosome in increasing order of their
coordinates on the real chromosome.
If the replication mechanism has a significant effect on base
composition asymmetry, this should be seen as a change of slope in the
nucleotide skews computed on the rearranged chromosome; the change of
slope should take place at the origin or the terminus of replication.
Use \code{extract.breakpoints} to detect the position of the changes in
slope on the rearranged nucleotide skews.
}

\value{
  A data.frame with six columns: \code{meancoord.rearr} contains the
  gene index on the rearranged chromosome; \code{gcskew.rearr} contains
  the normalized GC-skew ((G-C)/(G+C)) computed on the third codon positions of
  protein coding genes, still on the rearranged chromosome;  \code{atskew.rearr} contains
  the normalized AT-skew ((A-T)/(A+T)) computed on the third codon positions of
  protein coding genes; \code{strand.rearr} contains the transcription
  strand of the gene (either "forward" or "reverse"); \code{order}
  contains the permutation that was used to obtain a perfect gene
  orientation bias; \code{meancoord.real} contains the mid-coordinate of
  the genes on the real chromosome (before the rearrangement).
}

\references{
  Necşulea, A. and Lobry, J.R. (2007) A New Method for Assessing the 
  Effect of Replication on DNA Base Composition Asymmetry.
  \emph{Molecular Biology and Evolution}, \bold{24}:2169-2179.
}

\author{A. Necşulea}
\seealso{ \code{\link{oriloc}}, \code{\link{draw.rearranged.oriloc}},
  \code{\link{extract.breakpoints}}  }

\examples{

### Example for Chlamydia trachomatis ####

### Rearrange the chromosome and compute the nucleotide skews ###

\dontrun{r.ori <- rearranged.oriloc(seq.fasta = 
   system.file("sequences/ct.fasta.gz", package = "seqinr"),
    g2.coord =  system.file("sequences/ct.predict", package = "seqinr"))}

### Extract the breakpoints for the rearranged nucleotide skews ###



\dontrun{breaks <- extract.breakpoints(r.ori, type = c("gcfw", "gcrev"), 
 nbreaks =c(2, 2), gridsize = 50, it.max = 100)}



### Draw the rearranged nucleotide skews and place the position of the breakpoints ### 
### on the graphics ###

\dontrun{draw.rearranged.oriloc(r.ori, breaks.gcfw = breaks$gcfw$breaks,
 breaks.gcrev = breaks$gcrev$breaks)}


}

\keyword{utilities}
