\name{pmw}
\alias{pmw}
\title{ Protein Molecular Weight }
\description{
  With default parameter values, returns the apparent molecular weight of one mole (6.0221415 e+23)
  of the input protein expressed in gram at see level on Earth with terrestrial
  isotopic composition.
}
\usage{
pmw(seqaa, Ar = c(C = 12.0107, H = 1.00794, O = 15.9994,
N = 14.0067, P = 30.973762, S = 32.065), gravity = 9.81,
unit = "gram", checkseqaa = TRUE)
}
\arguments{
  \item{seqaa}{ a protein sequence as a vector of single chars. Allowed values are
  "*ACDEFGHIKLMNPQRSTVWY", non allowed values are ignored. }
  \item{Ar}{ a named vector for the mean relative atomic masses of CHONPS atoms. Defaults
  values are from to the natural terrestrial sources according to the 43rd IUPAC 
  General Assembly in Beijing, China in August 2005 (See 
  \url{https://iupac.org/category/recent-releases/}
  for updates).}
  \item{gravity}{ gravitational field constant in standard units. Defaults to 9.81 m/s2, that
  is to the average value at see level on Earth. Negative values are not allowed. }
  \item{unit}{ a string that could be "gram" to get the result in grams (1 g = 0.001 kg) 
  or "N" to get the result in Newton units (1 N = 1 kg.m/s2). }
  \item{checkseqaa}{ if TRUE \code{pmw()} warns if a non-allowed character in seqaa is found.}
}
\details{
\describe{
\item{Algorithm}{Computing the molecular mass of a protein is close to a linear form on
  amino-acid frequencies, but not exactly since we have to remove n - 1 water 
  molecules for peptidic bound formation.}
\item{Cysteine}{All cysteines are supposed to be in reduced (-SH) form.}
\item{Methionine}{All methionines are supposed to be not oxidized.}
\item{Modifications}{No post-traductional modifications (such as phosphorylations) 
are taken into account.}
\item{Rare}{Rare amino-acids (pyrolysine and selenocysteine) are not handled.}
\item{Warning}{Do not use defaults values for Ar to compute the molecular mass
of alien's proteins: the isotopic composition for CHONPS atoms could be different
from terrestrial data in a xenobiotic context. Some aliens are easily offended, make
sure not to initiate one more galactic war by repporting wrong results. }
}
}
\value{
  The protein molecular weight as a single numeric value. 
}
\references{
\code{citation("seqinr")}
}
\author{J.R. Lobry}
\seealso{ \code{\link{s2c}}, \code{\link{c2s}}, \code{\link{aaa}}, \code{\link{a}} }

\examples{
allowed <- s2c("*ACDEFGHIKLMNPQRSTVWY") # All allowed chars in a protein
pmw(allowed)
all.equal(pmw(allowed), 2395.71366) # Should be true on most platforms
#
# Compute the apparent molecular weight on Moon surface:
#
pmw(allowed, g = 1.6)
#
# Compute the apparent molecular weight in absence of gravity:
#
pmw(allowed, g = 0) # should be zero
#
# Reports results in Newton units:
#
pmw(allowed, unit = "N")
#
# Compute the mass in kg of one mol of this protein:
#
pmw(allowed)/10^3
#
# Compute the mass for all amino-acids:
#
sapply(allowed[-1], pmw) -> aamw
names(aamw) <- aaa(names(aamw))
aamw
}
\keyword{ utilities }
