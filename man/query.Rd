\name{query}
\alias{query}
\title{To get a list of sequence names from an ACNUC data base located on the web}
\description{
This is a major command of the package. It executes all sequence retrievals using any selection criteria the data base allows.  The sequences are coming from ACNUC data base located on the web and they are transfered by socket. The command produces the list of all sequence names that fit the required criteria. The sequence names belong to the class of sequence \code{SeqAcnucWeb}.
}
\usage{
query(listname, query, socket = autosocket(), 
invisible = TRUE, verbose = FALSE, virtual = FALSE)
}
\arguments{
  \item{listname}{The name of the list as a quoted string of chars}
  \item{query}{A quoted string of chars containing the request with the syntax given in the details section}
  \item{socket}{an object of class \code{sockconn} connecting to a remote ACNUC
                        database (default is a socket to the last opened database).}
  \item{invisible}{if \code{FALSE}, the result is returned visibly.}
  \item{verbose}{if \code{TRUE}, verbose mode is on}
  \item{virtual}{if \code{TRUE}, no attempt is made to retrieve the information about
    all the elements of the list. In this case, the \code{req} component of the list is set to 
    \code{NA}.}
}
\details{
The query language defines several selection criteria and operations between 
lists of elements matching criteria. It creates mainly lists of sequences, but 
also lists of species (or, more generally, taxa) and of keywords.
See \url{http://doua.prabi.fr/databases/acnuc/cfonctions.html#QUERYLANGUAGE}
for the last update of the description of the query language.\cr

Selection criteria (no space before the = sign) are: 
\describe{
  \item{SP=taxon}{seqs attached to taxon or any other below in tree; @ wildcard possible}
  \item{TID=id}{seqs attached to given numerical NCBI's taxon id}
  \item{K=keyword}{seqs attached to keyword or any other below in tree; @ wildcard possible}
  \item{T=type}{seqs of specified type}
  \item{J=journalname}{seqs published in journal specified using defined journal code}
  \item{R=refcode}{seqs from reference specified such as in jcode/volume/page (e.g., JMB/13/5432)}
  \item{AU=name}{seqs from references having specified author (only last name, no initial)}
  \item{AC=accessionno}{seqs attached to specified accession number}
  \item{N=seqname}{seqs of given name (ID or LOCUS); @ wildcard possible}
  \item{Y=year}{seqs published in specified year; > and < can be used instead of =}
  \item{O=organelle}{seqs from specified organelle named following defined code (e.g., chloroplast)}
  \item{M=molecule}{seqs from specified molecule as named in ID or LOCUS annotation records}
  \item{ST=status}{seqs from specified data class (EMBL) or review level (UniProt)}
  \item{F=filename}{seqs whose names are in given file, one name per line (unimplemented use \code{\link{clfcd}} instead)}
  \item{FA=filename}{seqs attached to accession numbers in given file, one number per line (unimplemented use \code{\link{clfcd}} instead)}
  \item{FK=filename}{produces the list of keywords named in given file, one keyword per line (unimplemented use \code{\link{clfcd}} instead)}
  \item{FS=filename}{produces the list of species named in given file, one species per line (unimplemented use \code{\link{clfcd}} instead)}
  \item{listname}{the named list that must have been previously constructed}
  }
Operators (always followed and preceded by blanks or parentheses) are:
\describe{
  \item{AND}{intersection of the 2 list operands}
  \item{OR}{union of the 2 list operands}
  \item{NOT}{complementation of the single list operand}
  \item{PAR}{compute the list of parent seqs of members of the single list operand}
  \item{SUB}{add subsequences of members of the single list operand}
  \item{PS}{project to species: list of species attached to member sequences of the operand list}
  \item{PK}{project to keywords: list of keywords attached to member sequences of the operand list}
  \item{UN}{unproject: list of seqs attached to members of the species or keywords list operand}
  \item{SD}{compute the list of species placed in the tree below the members of the species list operand}
  \item{KD}{compute the list of keywords placed in the tree below the members of the keywords list operand}
  }
The query language is case insensitive.Three operators (AND, OR, NOT) 
can be ambiguous because they can also occur within valid criterion values. 
Such ambiguities can be solved by encapsulating elementary selection 
criteria between escaped double quotes.
}

\value{
The result is directly assigned to the object \code{listname} in the user workspace.
This is an objet of class \code{qaw}, a list with the following 6 components:

  \item{call}{the original call}
  \item{name}{the ACNUC list name}
  \item{nelem}{the number of elements (for instance sequences) in the ACNUC list}
  \item{typelist}{the type of the elements of the list. Could be SQ for a list of
    sequence names, KW for a list of keywords, SP for a list of species names.}
  \item{req}{a list of sequence names that fit the required criteria or \code{NA} when
    called with parameter \code{virtual} is \code{TRUE}}
  \item{socket}{the socket connection that was used}
}
\references{ 
To get the release date and content of all the databases located at the pbil, please look at 
the following url: \url{http://doua.prabi.fr/search/releases}\cr
Gouy, M., Milleret, F., Mugnier, C., Jacobzone, M., Gautier,C. (1984) ACNUC: a nucleic acid sequence data base and analysis system. 
\emph{Nucl. Acids Res.}, \bold{12}:121-127.\cr
Gouy, M., Gautier, C., Attimonelli, M., Lanave, C., Di Paola, G. (1985) 
ACNUC - a portable retrieval system for nucleic acid sequence databases:
logical and physical designs and usage.
\emph{Comput. Appl. Biosci.}, \bold{3}:167-172.\cr
Gouy, M., Gautier, C., Milleret, F. (1985) System analysis and nucleic acid sequence banks.
\emph{Biochimie}, \bold{67}:433-436.\cr

\code{citation("seqinr")}
}
\author{J.R. Lobry, D. Charif}
\note{Most of the documentation was imported from ACNUC help
files written by Manolo Gouy}
\seealso{
  \code{\link{choosebank}}, 
  \code{\link{getSequence}}, 
  \code{\link{getName}},
  \code{\link{crelistfromclientdata}}
}
\examples{
 \dontrun{
 # Need internet connection
 choosebank("genbank")
 bb <- query("bb", "sp=Borrelia burgdorferi")
 # To get the names of the 4 first sequences:
 sapply(bb$req[1:4], getName)
 # To get the 4 first sequences:
 sapply(bb$req[1:4], getSequence, as.string = TRUE) 
 }
}
\keyword{utilities}
