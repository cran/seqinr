\name{fastacc}
\alias{fastacc}
\title{Fast Allele in Common Count}
\description{
The purpose of this function is to compute as fast as possible the number
of allele in common between a target (typically the genetic profile observed at a
crime scene, possibly a mixture with dropouts) and a database reference (typically
genetic profile of individuals). Both are assumed to be pre-encoded at the bit
level in a consistent way.
}
\usage{
fastacc(target, database)
}
\arguments{
  \item{target}{the \code{\link{raw}} encoding of the target, typically 40 octets for a core-CODIS profile in 2009}
  \item{database}{the \code{\link{raw}} encoding of the database. If there are n entries
in the database, then the database must n times longer than the target.}
}
\details{
This function is an RFC state. Comments are welcome.

Genetic profiles are encoded at the bit level. One bit represents one allele.
Count is based on a logical AND at bit level. Bit count is encoded at C level
using the precomputed approach: one indirection with an auxiliary table
of size 256 called \code{bits_in_char} which is pre-computed at R level and
passed at C level.
}
\value{
A vector of \code{\link{integer}} giving for each entry in the database how many
alleles are in common between the entry and the target.
}
\references{
\code{citation("seqinr")}
}
\author{J.R. Lobry}

\section{Warning }{Experimental, first release schedulded for seqinr  2.0-6 by the end of 2009}

\seealso{
FIXME
}
\examples{
#
# NOTE:
#
# This example section is a proof-of-concept stuff. Most code should be
# enbeded in documented functions to avoid verbosity. But at the RFC stage
# this is perhaps not a too bad idea to show how powerfull R is.
#

#
# Let's start from the 16 loci available in the AmpFLSTR kit:
#

path <- system.file("abif/AmpFLSTR_Bins_v1.txt", package = "seqinr")
resbin <- readBins(path)
codis <- resbin[["Identifiler_CODIS_v1"]]
names(codis)

#
# We count how many different alleles are present per locus:
#

na <- unlist(lapply(codis, function(x) length(x[[1]])))
na

#
# The number of octets required to encode a genetic for each locus is then:
#

ceiling(na/8)

#
# We need then a total of 40 octets to code these profiles:
#

sum(ceiling(na/8))

#
# Let's definene a function to encode a profile at a given locus, and vice versa :
#

prof2raw <- function(profile, alleles) {
  if (!is.ordered(alleles)) stop("ordered factor expected for alleles")
  if (!is.character(profile)) stop("vector of character expected for profile")
  noctets <- ceiling(length(alleles)/8)
  res.b <- rawToBits(raw(noctets))
  for (i in 1:length(profile)) {
    res.b[which(profile[i] == alleles)] <- as.raw(1)
  }
  return(packBits(res.b, type = "raw"))
}

raw2prof <- function(rawdata, alleles) {
  if (!is.ordered(alleles)) stop("ordered factor expected for alleles")
  if (!is.raw(rawdata)) stop("vector of raw expected for rawdata")
  res <- as.character(alleles)[as.logical(rawToBits(rawdata))]
  return(paste(res, collapse = ", "))
}

#
# Let now code all alleles present in codis as ordered factors:
#

allalleles <- lapply(codis, function(x) factor(x[, 1], levels = x[, 1], ordered = TRUE))

#
# Let's play with our encoding/decoding utilities with first locus:
#

allalleles[[1]] #  <8 8 9 10 11 12 13 14 15 16 17 18 19 >19
res <- prof2raw(c("8", "9", "13", "14", ">19"), allalleles[[1]])
res # c6 20
rawToBits(res) # 00 01 01 00 00 00 01 01 00 00 00 00 00 01 00 00
raw2prof(res, allalleles[[1]]) #  "8, 9, 13, 14, >19"

#
# Let define a profile with all possible alleles:
#

ladder <- unlist(lapply(allalleles, function(x) prof2raw(as.character(x),x)))
names(ladder) <- NULL
stopifnot(identical(as.integer(ladder), 
 c(255L, 63L, 255L, 255L, 255L, 63L, 255L, 63L, 255L, 31L, 255L, 
 63L, 255L, 255L, 7L, 255L, 3L, 255L, 63L, 255L, 255L, 255L, 255L, 
 15L, 255L, 127L, 255L, 3L, 255L, 255L, 255L, 255L, 3L, 3L, 255L, 
 15L, 255L, 255L, 255L, 7L))) # simple sanity check

#
# Let's make a simulated database. Here we use a random sampling
# with a uniform distribution between all possible profile possible
# at a given locus. A more realist sampling for an individual database
# would be to sample only two alleles at each locus according to
# observed frequencies in populations. 
#

n <- 10^5 # the number of records in the database
DB <- sapply(ladder, function(x) as.raw(sample(0:as.integer(x), size = n, replace = TRUE)))

#
# Now we make sure that the target is in the database:
#

target <- DB[666, ]
DB <- as.vector(t(DB)) # put DB as a flat database (is it usefull?) 

#
# Now we compute the number of alleles in common between the
# target and all the entries in the DB:
#

system.time(res <- fastacc(target,DB)) # Fast, isn't it ?
stopifnot(which.max(res) == 666) # sanity check

#
# Don't run : too tedious for routine check. We check here that complexity is
# linear in time up to a 10 10^6 database size (roughly the size of individual
# profiles at the EU level)
#

\dontrun{
maxn <- 10^7
DB <- sapply(ladder, function(x) as.raw(sample(0:as.integer(x),
  size = maxn, replace = T)))
target <- DB[666, ]
DB <- as.vector(t(DB))

np <- 10
nseq <- seq(from = 10^5, to = maxn, length = np)
res <- numeric(np)
i <- 1
for (n in nseq) {
  print(i)
  res[i] <- system.time(tmp <- fastacc(target, DB[1:n]))[1]
  stopifnot(which.max(tmp) == 666)
  i <- i + 1
}
dbse <- data.frame(list(nseq = nseq, res = res))

x <- dbse$nseq
y <- dbse$res
plot(x, y, type = "b", xlab = "Number of entries in DB", ylab = "One query time [s]",
las = 1, xlim = c(0, maxn), ylim = c(0, max(y)), main = "Data base size effect on query time")
lm1 <- lm(y ~ x - 1)
abline(lm1, col = "red")
legend("topleft", inset = 0.01, legend = paste("y =", formatC(lm1$coef[1],
digits = 3), "x"), col = "red", lty = 1)

#
# On my laptop the slope is 2.51e-08, that is a 1/4 of second to scan a database
# with 10 10^6 entries.
#
}

## end
}
