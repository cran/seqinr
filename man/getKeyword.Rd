\name{getKeyword}
\alias{getKeyword}
\alias{getKeyword.SeqAcnucWeb}
\alias{getKeywordsocket}
\alias{getKeyword.default}
\title{ Generic Function to get keyword(s) associated to a sequence }
\description{
  This function allows to get keyword(s) associated to a sequence. Method for the class of sequences \code{SeqAcnucWeb}. 
}
\usage{
getKeyword(object)
}
\arguments{
  \item{object}{an object of the class \code{SeqAcnucWeb}}
}
\value{
   A vector of strings containing the keyword(s) associated to a sequence.
}
\references{ 
To have an overview of the seqinR's functionnality, please consult this vignette: 
Charif, D., Lobry, J.R. (2005) SeqinR: a contributed package to the R project for statistical
computing devoted to biological sequences retrieval and analysis. Springer Verlag, \emph{Biological and Medical Physics/Biomedical Series}, in preparation. \cr
}
\author{D. Charif }
\seealso{ \code{\link{query}} }
\examples{
 \dontrun{s=choosebank.socket("embl")}
 \dontrun{query.socket(s$socket,"fc","sp=felis catus et t=cds")}
 \dontrun{getKeyword(fc$req[[7]])}
}
\keyword{ manip }
