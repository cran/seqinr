\name{getType}
\alias{getType}
\title{ To Get Sequence Type }
\description{
 This function allows to get all the sequence's type present in the opened ACNUC databank.  
}
\usage{
getType(socket)
}
\arguments{
  \item{socket}{ An object of class \code{connection} returned by \code{choosebank}. }
}
\value{
  It returns a list containing for each sequence type, a short description.
}
\references{ 
To have an overview of the seqinR's functionnality, please consult this vignette: 
Charif, D., Lobry, J.R. (2005) SeqinR: a contributed package to the R project for statistical
computing devoted to biological sequences retrieval and analysis. Springer Verlag, \emph{Biological and Medical Physics/Biomedical Series}, in preparation. \cr
}
\author{ D. Charif }
\seealso{ \code{\link{choosebank}}, \code{\link{query}}, \code{\link{plot.SeqAcnucWeb}} }
\examples{
\dontrun{s = choosebank("genbank")}
\dontrun{geType(s$socket) }
}
\keyword{utilities}
