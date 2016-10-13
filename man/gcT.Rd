\name{gcT}
\alias{gcT}
\docType{data}
\title{GC content and temperature in bacteria}
\description{
  This data set was used in Galtier and Lobry (1997) to study the relationship between the optimal growth temperature of bacteria and their G+C content at the genomic level and locally were selection is active to maintain secondary structures in the stems of RNAs.
}
\format{
 	\code{gcT} is a list containing the 9 following components:  
	\describe{	
 	\item{species}{is a data frame containing the optimal growth temperature and genomic G+C content for 772 bacterial species. Detailled explanations for this table and the following are available in the \code{README} component.}
 	\item{genus}{is a data frame containing the optimal growth temperature and genomic G+C content for 224 bacterial genus.}
 	\item{details}{is a data frame with more information, see \code{README}.}
 	\item{gc16S}{is a data frame containing the optimal growth temperature and stems G+C content for 16S RNA from 165 bacterial genus.}
 	\item{gctRNA}{is a data frame containing the optimal growth temperature and stems G+C content for tRNA from 51 bacterial genus.}
 	\item{gc23S}{is a data frame containing the optimal growth temperature and stems G+C content for 23S RNA from 38 bacterial genus.}
 	\item{gc5S}{is a data frame containing the optimal growth temperature and stems G+C content for 5S RNA from 71 bacterial genus.}
 	\item{README}{is the original README file from \code{ftp://biom3.univ-lyon1.fr/pub/datasets/JME97/} last updated 13-MAY-2002.}
 	\item{importgcT}{is the R script used to import data.}
  }
}
\source{
Galtier, N. & Lobry, J.R. (1997). Relationships between genomic G+C 
content, RNA secondary structures, and optimal growth temperature 
in prokaryotes. \emph{Journal of Molecular Evolution} \bold{44}:632-636.\cr

Data imported into seqinr with the R script given in the last component of the dataset by J.R. Lobry on 09-OCT-2016. 
}
\references{
  \code{citation("seqinr")}
}
\examples{
data(gcT)
}
