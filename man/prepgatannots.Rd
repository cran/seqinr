\name{prepgetannots}
\alias{prepgetannots}
\alias{pga}
\title{Select annotation lines in an ACNUC database}
\description{
This function is called before using \code{\link{getAnnot}} or
\code{\link{modifylist}} with a \code{scan} type operation to
select the annotation lines to be returned or scanned.
}

\usage{
prepgetannots(what = "all", setfor = c("scan", "getannots"),
                       socket = autosocket(), verbose = FALSE)
pga(what = "all", setfor = c("scan", "getannots"),
                       socket = autosocket(), verbose = FALSE)
}

\arguments{
  \item{what}{the default "all" means that all annotation lines are selected.
    This can be more specific, see details.}
  \item{setfor}{this is used when \code{what} has its default "all" value.
    The behaviour is different for \code{\link{getAnnot}} and
    \code{\link{modifylist}} with a \code{scan} type operation:
    annotations but not sequences are scanned, but sequences can
    be returned by \code{\link{getAnnot}}. The default value is "scan".}
  \item{socket}{an object of class \code{sockconn} connecting to an ACNUC server}
  \item{verbose}{logical, if TRUE mode verbose is on}
}

\details{
The names of annotation lines in the opened ACNUC database is
returned by \code{\link{countfreelists}}, they are forced to upper
case letters by \code{prepgetannots} when supplied with the
\code{what} argument.

For the EMBL/SWISSPROT format, keys are: ALL, AC, DT, KW, OS, OC, OG, OH, 
RN, RC, RP, RX, RA, RG, RT, RL, DR, AH, AS, CC, FH, FT, SQ, SEQ.

For GenBank: ALL, ACCESSION, VERSION, KEYWORDS, SOURCE, ORGANISM, 
REFERENCE, AUTHORS, CONSRTM, TITLE, JOURNAL, PUBMED, REMARK, COMMENT, 
FEATURES, ORIGIN, SEQUENCE.

For FT (embl, swissprot) and FEATURES (GenBank), one or more specific 
feature keys can be specified using lines with only uppercase and such as

FEATURES|CDS
FT|TRNA

Keys ALL and SEQ/SEQUENCE stand for all annotation and sequence lines, respectively.
For the scan operation, key ALL stand for the DE/DEFINITION lines, 
and SEQ/SEQUENCE cannot be used (annotations but not sequence are scanned).}

\value{
The function returns invisibly the annotation lines names.
}
\references{ 
\code{citation("seqinr")}
}
\author{J.R. Lobry}
\seealso{
  \code{\link{getAnnot}}, \code{\link{modifylist}}, \code{\link{countfreelists}}
}

\examples{
 \dontrun{# Need internet connection
  choosebank("genbank")
  mylist <- query("mylist","n=AQF16SRRN")
  pga() # We want to scan all annotations, including FEATURES
  mylist <- modifylist("mylist", operation = "strain", type = "scan")
  mylist$nelem # should be 1
 }
}
\keyword{utilities}
