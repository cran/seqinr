\name{query}
\alias{query}
\alias{print.qaw}
\title{To get a list of sequence names from an ACNUC data base located on the web}
\description{
This is a major command of the package. It executes all sequence retrievals using any selection criteria the data base allows.  The sequences are coming from ACNUC data base located on the web and they are transfered by socket. The command produces the list of all sequence names that fit the required criteria. The sequence names belong to the class of sequence \code{SeqAcnucWeb}.
}
\usage{
query(socket,listname,query, invisible = FALSE)
}
\arguments{
  \item{socket}{a socket of class connection returned by \code{choosebank}. }
  \item{listname}{The name of the list as a quoted string of chars}
  \item{query}{A quoted string of chars containing the request with the syntax given in the details section}
  \item{invisible}{if \code{TRUE}, the result of the query will be invisible but assigned is the environment}
}
\details{
Each selection criterion is written using the following syntax:

\item{c = criterion value}{where c indicates which criterion is used. 
Many selection criteria are available. They correspond mainly to the 
structured elements of the sequence documentation in the data banks,
and are detailled thereafter. Criteria can be combined using 3 logical 
operations:   
                              
criterion1 ET criterion2 : logical AND (sequences that fit criteria 1 and 2 
simultaneously).

criterion1 OU criterion2 : logical OR (sequences that fit at least one of both criteria).

NO criterion1 : logical negation (sequences that do not fit criterion 1).

Parentheses can be used to delimit the range of operations.
List of sequences can be re-used at will, which is very convenient to
fragment complexe requests into simple requests. For instance, here are
two equivalent ways to get all coding sequences from \emph{Escherichia coli} 
that are not partial:

\preformatted{s=choosebank("genbank")}
\code{query(s$socket,"final","sp=escherichia coli ET t=cds ET NO k=partial")}
\preformatted{s=choosebank("genbank")}
\code{query(s$socket,"eco","sp=escherichia coli")}
\preformatted{query(s$socket,"ecocds","eco ET t=cds")}
\code{query(s$socket,"final","ecocds ET NO k=partial")}

}

\item{SP = species name}{ sequences from given (group of) species.                
The special character @ can be used to match any group of characters in      
the species name, ex: SP=RATTUS@.
Use of space is allowed. Examples: ESCHERICHIA COLI, @COLI, E@COLI. Species names are tree-structured according to the biological classification 
of species.}

\item{K = keyword}{ sequences having a given keyword. Since keywords are    
tree structured, as are species, you will select all    
sequences associated to keywords further down in tree.  
(@ can be used to match any group of characters) }

\item{R = reference code}{sequences from a given reference. References are specified as follows depending on the type of document:}

 \tabular{rlll}{
   \tab Document \tab Format \tab Example\cr
   \tab Journal article \tab journal\_code/volume/1st\_page \tab jme/34/17\cr
   \tab Book  \tab book/year/1st\_author \tab book/1980/broker\cr
   \tab Thesis \tab  thesis/year/1st\_author \tab thesis/1984/wildgruber\cr
   \tab Patent \tab patent/patent\_coded\_number \tab patent/ep0238993\cr
   \tab Unpublished, or submitted \tab unpubl/year/1st\_author \tab unpubl/1993/cho
}
                    
\item{J = journal name}{sequences published in a given journal.}                                   
\item{Y = year}{sequences published in given year (e.g. 1982).}
\item{Y > year}{sequences published after or during a given year.}
\item{Y < year}{sequences published before or during a given year.}

\item{AU = author}{sequences published by given author(s). Use @ to specify
any letters in name (e.g. @ORMOND@ for Van Ormondt).
Only last names are indexed - initials are ignored. All authors of journal articles are indexed. Only the first author of books, theses, patents and other documents is indexed.
}
 
\item{T = sequence type}{ sequences of given type. You generally obtain 
subsequences with this criterion because types are for example tRNA, 
rRNA or protein gene.
Type should not be confused with molecule which denotes the chemical nature of the sequenced molecule (\emph{e.g.}, DNA, mRNA, tRNA). Type is defined only for the nucleotide sequence banks. Presently the existing types are:}

\tabular{lll}{
ID      \tab Locus entry \tab (EMBL, SWISS-PROT, NRSub)\cr
LOCUS   \tab Locus entry \tab (GenBank, Hovergen, EMGLib)\cr
CDS     \tab .PE protein coding region \tab (all)\cr
RRNA    \tab .RR mature ribosomal RNA \tab (all)\cr
TRNA    \tab .TR mature transfer RNA \tab (all)\cr
MISC\_RNA\tab .RN other structural RNA coding region \tab (EMBL, GenBank, Hovergen, NRSub, EMGLib)\cr
SNRNA   \tab .SN small nuclear RNA \tab (EMBL, GenBank, Hovergen, EMGLib)\cr
SCRNA   \tab .SC small cytoplasmic RNA \tab (EMBL, GenBank, Hovergen, NRSub, EMGLib)\cr
3'INT   \tab .3I 3' intron \tab (Hovergen)\cr
3'NCR   \tab .3F 3' non-coding region  \tab  (Hovergen)\cr
5'INT   \tab .5I 5' intron  \tab (Hovergen)\cr
5'NCR   \tab .5F 5' non-coding region  \tab  (Hovergen)\cr
CPG     \tab .CG CpGobs/CpGexp>0.5 \tab (Hovergen)\cr
INT\_INT \tab .IN internal intron   \tab   (Hovergen)
}

Each entry of a FEATURE TABLE describing a coding region of a DNA fragment gives rise to a subsequence equal to the fragments described in the location of the feature. The type of the resulting subsequence equals the key of the corresponding feature table entry. The name of the resulting subsequence is  built by adding to the parent sequence's name an extension uniquely identifying this particular feature. 

Sequences of a given type are generally subsequences, \emph{i.e.}, fragments of parent sequences, except if the coding region covers totally the parent sequence, in which case ACNUC does not create a subsequence.
                 
\item{O = organelle}{sequences from a given organelle. 
Organelle (\emph{e.g.}, chloroplast, mitochondrion) denotes the nature of the genome that harbors a particular gene. By extension, ACNUC also sees the nucleus as an organelle. Also, a nuclear-encoded gene coding for a protein exported to an organelle is considered as a nuclear gene. The existing organelles are:}

\tabular{lll}{
CHLOROPLAST   \tab Chloroplast genome   \tab (EMBL, GenBank, NBRF, Hovergen)\cr
MITOCHONDRION \tab Mitochondrial genome \tab (EMBL, GenBank, NBRF, Hovergen)\cr
KINETOPLAST   \tab Kinetoplast genome   \tab (EMBL, GenBank, Hovergen)\cr
NUCLEAR       \tab Nuclear genome       \tab (all)
}


\item{M = molecule name}{ sequences with given chemical structure. 
In ACNUC, molecule denotes the chemical nature of the sequenced molecule (\emph{e.g.}, DNA, mRNA, tRNA). 
Molecule should not be confused with type which identifies the encoded molecule (\emph{e.g.}, protein, tRNA, rRNA). Thus the sequence of a tRNA gene has DNA for molecule because DNA rather than tRNA was sequenced. The subsequence covering the tRNA region has tRNA for type because this is the nature of the encoded product. Molecule is defined only for the nucleotide sequence banks (GenBank, EMBL, Hovergen, NRSub, and CGDB). Presently the existing molecules are:} 

\tabular{lll}{
DNA    \tab Sequenced molecule is DNA   \tab (all)\cr
RNA    \tab Sequenced molecule is RNA   \tab (all)\cr
MRNA   \tab Sequenced molecule is mRNA  \tab (GenBank, Hovergen)\cr
RRNA   \tab Sequenced molecule is rRNA  \tab (GenBank, Hovergen)\cr
TRNA   \tab Sequenced molecule is tRNA  \tab (GenBank, Hovergen)\cr
URNA   \tab Sequenced molecule is snRNA \tab (GenBank, Hovergen)
}
  
\item{N = sequence name}{ sequence of given name.}
           
\item{AC = accession number}{ sequences of given accession number.}

\item{F = file name}{ sequences whose names are in a specified file.}                                
\item{FA = file name}{ sequences whose accesion numbers are in a specified file.} 
}
\value{
  A list with the following components:
  \item{bank}{the name of the bank that has been choosen by \code{choosebank.socket}}	
  \item{call}{original call}
  \item{name}{list name}
  \item{req}{a list of sequence names  that fit the required criteria}
}
\references{ 
To get the release date and content of all the databases located at the pbil, please look at the following url: \url{http://pbil.univ-lyon1.fr/search/releases.php}\cr
Gouy, M., Milleret, F., Mugnier, C., Jacobzone, M., Gautier,C. (1984) ACNUC: a nucleic acid sequence data base and analysis system. 
\emph{Nucl. Acids Res.}, \bold{12}:121-127.\cr
Gouy, M., Gautier, C., Attimonelli, M., Lanave, C., Di Paola, G. (1985) 
ACNUC - a portable retrieval system for nucleic acid sequence databases:
logical and physical designs and usage.
\emph{Comput. Appl. Biosci.}, \bold{3}:167-172.\cr
Gouy, M., Gautier, C., Milleret, F. (1985) System analysis and nucleic acid sequence banks.
\emph{Biochimie}, \bold{67}:433-436. \cr

To have an overview of the seqinR's functionnality, please consult this vignette: 
Charif, D., Lobry, J.R. (2005) SeqinR: a contributed package to the R project for statistical
computing devoted to biological sequences retrieval and analysis. Springer Verlag, \emph{Biological and Medical Physics/Biomedical Series}, in preparation. \cr
}
\author{J.R. Lobry & D. Charif}
\note{Most of the documentation was imported from ACNUC help
files written by Manolo Gouy}
\seealso{ \code{\link{choosebank}}, \code{\link{getSequence}}, \code{\link{plot.SeqAcnucWeb}} }
\examples{
 \dontrun{s = choosebank("genbank")}
 \dontrun{query(s$socket,"ecoli","sp=escherichia coli@")}
 \dontrun{ecoli}
 # To have the 4 first names of the sequence
 \dontrun{ecoli$req[1:4]}
 \dontrun{ecoli$req[[5]]}
 \dontrun{ecoli$call}
}
\keyword{utilities}
