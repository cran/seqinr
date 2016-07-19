\name{extractseqs}
\alias{extractseqs}
\alias{exseq}
\title{To extract the sequences information of a sequence or a list of sequence in different formats}
\description{
 The function allows to extract large amount of data as whole genome sequences,using different output formats and types of extraction.
 This function is not yet available for windows in zlib mode.
}

\usage{
extractseqs(listname,socket = autosocket(), format="fasta",
operation="simple",feature="xx", bounds="xx", minbounds="xx",
 verbose = FALSE, nzlines=1000, zlib = FALSE)
exseq(listname,socket = autosocket(),
 format="fasta",operation="simple", feature="xx",
 bounds="xx", minbounds="xx", verbose = FALSE,  nzlines=1000, zlib = FALSE)
}

\arguments{
  \item{listname}{the name of list on server (may be a virtual list) }
  \item{socket}{an object of class \code{sockconn} connecting to a remote ACNUC
                        database (default is a socket to the last opened database).}
  \item{format}{the format of output.Can be \code{acnuc}, \code{fasta},\code{flat} or \code{coordinates} }
  \item{operation}{the type of extraction. Can be  \code{simple}, \code{translate}, \code{fragment}, \code{feature} or \code{region}  }
  \item{feature}{-optional- the feature to be extracted (for operations "feature" or "region"): a feature table item (CDS, mRNA,...)}
  \item{bounds}{-optional- the bounds for extraction (for operations "fragment" or "region") }
  \item{minbounds}{-optional- the minimal bounds for extraction (for operations "fragment" or "region") }
  \item{verbose}{if \code{TRUE}, verbose mode is on}
  \item{nzlines}{number of line in zlib mode}
  \item{zlib}{logical. If TRUE sequences are download in zlib compress mode.}
}

\details{
 To extract a list of sequences (lrank argument) or a single sequence (seqnum argument)
using different output formats and types of extraction.
All formats except "coordinates" extract sequence data.
Format "coordinates" extract coordinate data; start > end indicates the complementary strand.
\describe{
	\item{\strong{listname}}{sequence list name.}
	\item{\strong{socket}}{a socket of class connection and sockconn returned by \code{choosebank}. 
		Default value (auto) means that the socket will be set to to the socket component of the banknameSocket variable. }
	\item{\strong{format}}{\code{acnuc}, \code{fasta}, \code{flat} or \code{coordinates}}
	\item{\strong{operation}}{\code{simple}, \code{translate}, \code{fragment}, \code{feature} or \code{region}}
	\item{\strong{feature}}{(for operations "feature" or "region") a feature table item (CDS, mRNA,...).
		\describe{
			\item{\emph{simple}}{each sequence or subsequence is extracted.}
			\item{\emph{translate}}{meaningful only for protein-coding (sub)sequences that are extracted as protein sequences. Nothing is extracted for non-protein coding sequences.}
			\item{\emph{fragment}}{Allows to extract any part of the sequence(s) in list.
			Such part is specified by the bounds and minbounds arguments according to the syntax suggested by these examples:
			\tabular{lll}{
			  \tab 132,1600  \tab to extract from nucl. 132 to nucl 1600 of the sequence. If applied to a subsequence, coordinates are in the parent seq relatively to the subsequence start point.\cr
			  \tab -10,10    \tab to extract from 10 nucl. BEFORE the 5' end of the sequence to nucl. 10 of it. Useful only for subsequences, and produces a fragment extracted from its parent sequence.\cr
			  \tab e-20,e+10 \tab to extract from 20 nucl. BEFORE the 3' end of the sequence to 10 nucl. AFTER its 3' end. Useful only for subsequences, and  produces a fragment extracted from its parent sequence.\cr
			  \tab -20,e+5   \tab to extract from 20 nucl. BEFORE the 5' end of the sequence to 5 nucl. AFTER its 3' end.
			}
			}
			}}
	\item{\strong{bounds}}{(for operations "fragment" or "region") see syntax above.}
	\item{\strong{minbounds}}{same syntax as bounds. When the sequence data is too short for this quantity
		to be extracted, nothing is extracted. When the sequence data is between minbounds and bounds,
		extracted sequence data is extended by N's to the desired length.}
	
}

}

\value{
  Sequence data.
}
\references{ 
\code{citation("seqinr")}
}
\author{S. Penel}
\seealso{
  \code{\link{choosebank}}, 
  \code{\link{query}}
  \code{\link{getlistrank}}
}
\examples{
 \dontrun{# Need internet connection
 choosebank("emblTP")
 mylist <- query("mylist", "k=globin", virtual = TRUE)
 mylist.fasta <- exseq("mylist", verbose = TRUE)
 # 103 lines of FASTA 
 stopifnot(length(mylist.fasta) == 103)
 closebank()
 }
}
\keyword{utilities}
