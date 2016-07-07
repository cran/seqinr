\name{chargaff}
\alias{chargaff}
\docType{data}
\title{Base composition in ssDNA for 7 bacterial DNA}
\description{
Long before the genomic era, it was possible to get some data
for the global composition of single-stranded DNA chromosomes
by direct chemical analyses. These data are from Chargaff's lab
and give the base composition of the L (Ligth) strand for
7 bacterial chromosomes.
}
\usage{data(chargaff)}
\format{
  A data frame with 7 observations on the following 4 variables.
  \describe{
    \item{[A]}{frequencies of A bases in percent}
    \item{[G]}{frequencies of G bases in percent}
    \item{[C]}{frequencies of C bases in percent}
    \item{[T]}{frequencies of T bases in percent}
  }
}
\details{
Data are from Table 2 in Rudner \emph{et al.} (1969) for the
L-strand. Data for \emph{Bacillus subtilis} were taken from
a previous paper: Rudner \emph{et al.} (1968). This is in
fact the average value observed for two different strains
of \emph{B. subtilis}: strain W23 and strain Mu8u5u16.\cr
Denaturated chromosomes can be separated by a technique of
intermitent gradient elution from a column of methylated
albumin kieselguhr (MAK), into two fractions, designated,
by virtue of their buoyant densities, as L (light) and H
(heavy). The fractions can be hydrolyzed and subjected to
chromatography to determined their global base composition.\cr
The surprising result is that we have almost exactly A=T
and C=G in single stranded-DNAs. The second paragraph page
157 in Rudner \emph{et al.} (1969) says: "Our previous
work on the complementary strands of \emph{B. subtilis} DNA
suggested an additional, entirely unexpected regularity,
namely, the equality in either strand of 6-amino and 6-keto
nucleotides ( A + C = G + T). This relationship, which
would normally have been regarded merely as the consequence
of base-pairing in DNA duplex and would not have been predicted
as a likely property of a single strand, is shown here to
apply to all strand specimens isolated from denaturated DNA
of the AT type (Table 2, preps. 1-4). It cannot yet be said
to be established for the DNA specimens from the equimolar
and GC types (nos. 5-7)."

Try \code{example(chargaff)} to mimic figure page 17 in Lobry
(2000) :

\if{html}{\figure{chargaff.png}{options: width=400}}
\if{latex}{\figure{chargaff.png}{options: width=12cm}}


Note that \code{example(chargaff)} gives more details:
the red areas correspond to non-allowed values beause the sum
of the four bases frequencies cannot exceed 100\%.
The white areas correspond to possible values (more exactly
to the projection from \code{R^4} to the corresponding \code{R^2} planes
of the region of allowed values).
The blue lines correspond to the very small subset of allowed
values for which we have in addition PR2 state, that is
\code{[A]=[T]} and \code{[C]=[G]}. Remember, these data are for ssDNA!
}

\source{
Rudner, R., Karkas, J.D., Chargaff, E. (1968) Separation of
\emph{B. subtilis} DNA into complementary strands, III. Direct
Analysis. \emph{Proceedings of the National Academy of Sciences of the United States of America}, \bold{60}:921-922.\cr
Rudner, R., Karkas, J.D., Chargaff, E. (1969) Separation of microbial deoxyribonucleic acids into complementary strands. \emph{Proceedings of the National Academy of Sciences of the United States of America}, \bold{63}:152-159.\cr

}
\references{
Lobry, J.R. (2000) The black hole of symmetric molecular evolution. Habilitation thesis, Université Claude Bernard - Lyon 1. \url{https://pbil.univ-lyon1.fr/members/lobry/articles/HDR.pdf}.

\code{citation("seqinr")}
}

\examples{
data(chargaff)
op <- par(no.readonly = TRUE)
par(mfrow = c(4,4), mai = rep(0,4), xaxs = "i", yaxs = "i")
xlim <- ylim <- c(0, 100)

for( i in 1:4 )
{
  for( j in 1:4 )
  {
    if( i == j )
    {
      plot(chargaff[,i], chargaff[,j],t = "n", xlim = xlim, ylim = ylim,
      xlab = "", ylab = "", xaxt = "n", yaxt = "n")
      polygon(x = c(0, 0, 100, 100), y = c(0, 100, 100, 0), col = "lightgrey")
      for( k in seq(from = 0, to = 100, by = 10) )
      {
        lseg <- 3
        segments(k, 0, k, lseg)
        segments(k, 100 - lseg, k, 100)
        segments(0, k, lseg, k)
        segments(100 - lseg, k, 100, k)
      }
      string <- paste(names(chargaff)[i],"\n\n",xlim[1],"\% -",xlim[2],"\%")
      text(x=mean(xlim),y=mean(ylim), string, cex = 1.5)
    }
    else
    {
      plot(chargaff[,i], chargaff[,j], pch = 1, xlim = xlim, ylim = ylim,
      xlab = "", ylab = "", xaxt = "n", yaxt = "n", cex = 2)
      iname <- names(chargaff)[i]
      jname <- names(chargaff)[j]
      direct <- function() segments(0, 0, 50, 50, col="blue")
      invers <- function() segments(0, 50, 50, 0, col="blue")
      PR2 <- function()
      {
        if( iname == "[A]" & jname == "[T]" ) { direct(); return() }
        if( iname == "[T]" & jname == "[A]" ) { direct(); return() }
        if( iname == "[C]" & jname == "[G]" ) { direct(); return() }
        if( iname == "[G]" & jname == "[C]" ) { direct(); return() }
        invers()
      }
      PR2()
      polygon(x = c(0, 100, 100), y = c(100, 100, 0), col = "pink4")
      polygon(x = c(0, 0, 100), y = c(0, 100, 0))
    }
  }
}
# Clean up
par(op)
}
\keyword{datasets}
