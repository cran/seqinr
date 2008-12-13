\name{identifiler}
\alias{identifiler}
\docType{data}
\title{Identifiler allele names}
\description{
Names of the alleles in the Applied Biosystem identifiler allelic ladder.
}
\usage{data(identifiler)}
\format{
  A list with 4 components for the four fluorochromes.
  \describe{
    \item{FAM}{a list of 4 loci}
    \item{VIC}{a list of 5 loci}
    \item{NED}{a list of 4 loci}
    \item{PET}{a list of 3 loci}
  }
}

\examples{
data(identifiler)
op <- par(no.readonly = TRUE)
par(mar = c(3,8,4,2)+0.1)
allcount <- unlist(lapply(identifiler, function(x) lapply(x, length)))
barplot(allcount[order(allcount)], horiz = TRUE, las = 1,
main = "Allele count per locus", col = "lightblue")
par(op)
}
