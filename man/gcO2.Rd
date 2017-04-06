\name{gcO2}
\alias{gcO2}
\docType{data}
\title{GC content and aerobiosis in bacteria}
\description{
  This data set was used in Naya \emph{et al.} (2002) to study the relationship between the genomic G+C content of bacteria and whether they are (stricly) aerobes or anaerobes.
}
\format{
\code{gcO2} is a data frame.  
}
\source{
Naya, H., Romero, H., Zavala, A., Alvarez, B. and Musto, H. (2002) Aerobiosis increases the Genomic Guanine Plus Cytosine Content (GC%) in prokaryotes. \emph{Journal of Molecular Evolution}, \bold{55}:260-264.

Data imported into seqinr by J.R. Lobry on 09-OCT-2016. Original source location given in the article was \code{http://oeg.fcien.edu.uy/GCprok/} but is no more active. Data were copied at \url{http://pbil.univ-lyon1.fr/R/donnees/gcO2.txt} (\emph{cf.} section 2.1 in Lobry, J.R (2004) Life history traits and genome structure: aerobiosis and G+C content in bacteria. \emph{Lecture Notes in Computer Sciences}, \bold{3039}:679-686). Import was from this last ressource. There are 130 aerobic genera in this data set while fig. 1 in Naya \emph{et al.} (2002) gives 126. There is no way to track down the reason for this difference because the original data set was lost (Héctor Musto pers. comm.). The number of anaerobic genera (n = 69) is consistent between the present data set and fig. 1 in Naya \emph{et al.} (2002).
}
\references{
  \code{citation("seqinr")}
}
\examples{
data(gcO2)
}
