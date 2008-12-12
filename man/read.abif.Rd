\name{read.abif}
\alias{read.abif}
\title{Read ABIF formatted files}
\description{
ABIF stands for Applied Biosystem Inc. Format, a binary fromat modeled after TIFF format.
Corresponding files usually have an \code{*.ab1} or \code{*.fsa} extension.
}
\usage{
read.abif(filename, max.bytes.in.file = file.info(filename)$size, pied.de.pilote = 1.2, verbose = FALSE)
}
\arguments{
  \item{filename}{The name of the file.}
  \item{max.bytes.in.file}{The size in bytes of the file, defaulting to what is returned by \code{\link{file.info}}}	
  \item{pied.de.pilote}{Safety factor: the argument \code{n} to \code{\link{readBin}} is set as \code{pied.de.pilote*max.bytes.in.file}.}	
  \item{verbose}{logical [FALSE]. If TRUE verbose mode is on.}	
}
\details{
This function is experiemental and has not been intensively tested. All data are imported
into memory, there is no attempt to read items on the fly.
}
\value{
A list with three components: \code{Header} which is a list that contains various low-level information,
among which \code{numelements} is the number of elements in the directory and \code{dataoffset}
the offset to find the location of the directory. \code{Directory} is a data.frame for the directory
of the file with the number of row being the number of elements in the directory and the 7
columns describing various low-level information about the elements. \code{Data} is a list
with the number of components equal to the number of elements in the directory. 
}
\references{ 
\code{citation("seqinR")}

Anonymous (2006) Applied Biosystem Genetic Analysis Data File Format.
Available at \url{http://www.appliedbiosystems.com/support/software_community/ABIF_File_Format.pdf}.
Last visited on 03-NOV-2008.
}
\author{J.R. Lobry}
\seealso{\code{\link{readBin}} which is used here to import the binary file and \code{\link{file.info}} to
get the size of the file.}
\examples{
# An example is needed here (read a file and check with data(something)
}
