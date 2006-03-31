	#####################################################################################
	# 		classes de séquences						    #
	#  toutes les classes doivent avoir exactement la même interface à savoir:	    #
	#  une fonction initNomClasse qui retourne une instance de la classe		    #
	#  des spécialisations des fonctions                                                #
	#	     getSequence(seq) retourne un vecteur de char	     		    #
	#            getFrag(seq,begin,end) retourne un vecteur de caractères               #
	#            getLength(seq) retourne un "entier"                                    #
	#            getName(seq) retourne une chaîne                                       #               
	#	     getAnnot(seq,nl) reourne un vecteur de string                          #   
        #            getExon(seq) retourne la position des exons                            # 
     	#	     getKeyword(seq) retourne les mots-clef associés à une séquence         #
	#	     getTrans(seq) retourne un vecteur de char                              # 
	#####################################################################################

###################################################################################################
#                                                                                                 #
#                          Generic functions section                                              #
#                                                                                                 #
###################################################################################################


getFrag <-  function(object, begin, end) {
  if(! inherits(object,c("SeqFastadna", "SeqFastaAA", "SeqAcnucWeb", "SeqFrag")))
  { 
    getFrag.default(object, begin, end) 
  } else {
    UseMethod("getFrag")
    }
}

getSequence <- function(object, as.string = FALSE){
  if(! inherits(object, c("SeqFastadna", "SeqFastaAA", "SeqAcnucWeb", "SeqFrag"))) 
  {
    getSequence.default(object, as.string = as.string)
  } else {
    UseMethod("getSequence")
  }
}

getLength <-  function(object) {
  if(! inherits(object, c("SeqFastadna", "SeqFastaAA", "SeqAcnucWeb", "SeqFrag"))) 
  {
    getLength.default(object)
  } else { 
    UseMethod("getLength")
  }
}

getName <-  function(object) {
  if(! inherits(object, c("SeqFastadna", "SeqFastaAA", "SeqAcnucWeb", "SeqFrag"))) 
  {
    getName.default(object)
  } else { 
    UseMethod("getName")
  }
}

getAnnot <- function(object, nbl = 10000) {
  if(! inherits(object,c("SeqFastadna", "SeqFastaAA", "SeqAcnucWeb", "SeqFrag"))) 
  {
    getAnnot.default(object, nbl = nbl)
  }
  else UseMethod("getAnnot")
}

getLocation <- function(object) {
  if(! inherits(object, c("SeqAcnucWeb"))) 
  {
    getLocation.default(object)
  } else {
    UseMethod("getLocation")
  }
}

getKeyword <- function(object) {
  if(! inherits(object,c("SeqAcnucWeb"))) 
  {
    getKeyword.default(object)
  } else {
    UseMethod("getKeyword")
  }
}

getTrans <- function(object, frame = 0, sens = "F", numcode = 1, NAstring = "X", ambiguous = FALSE){
  if(! inherits(object, c("SeqFastadna", "SeqFastaAA", "SeqAcnucWeb", "SeqFrag"))) 
  {
    getTrans.default(object, frame = frame, sens = sens, numcode = numcode, NAstring = NAstring, ambiguous = ambiguous)
  } else {
    UseMethod("getTrans")
  }
}


###################################################################################################
#                                                                                                 #
#                          Default methods section                                                #
#                                                                                                 #
###################################################################################################


getSequence.default <- function(object, as.string = FALSE){
  if( as.string ){
    if( length(object) == 1) {
      return(object)
    } else {
      return(c2s(object))
    }
  } else {
    if( length(object) == 1) {
      return(s2c(object))
    } else {
      return(object)
    }
  }
}


getFrag.default = function(object,begin,end){ 
	if(length(object) == 1) object=s2c(object)	
 	xx = tolower(object)
# 	if(length(grep("[acgtu]",xx)) != length(xx)) stop("Biological sequence is needed !")
 	if(length(grep("[acgtu]",xx)) != length(xx)) warning("Sequence with non acgtu characters!")
 	if(begin>length(xx) || end>length(xx) || begin>end) stop("borns are not correct")	
 	else return(xx[begin:end])		
}

getLength.default = function(object){
	if(length(object) == 1) object=s2c(object)	
 	xx = tolower(object)
 	#if(length(grep("[acgtu]",xx)) != length(xx)) stop("Biological sequence is needed !")
	if(length(grep("[acgtu]",xx)) != length(xx)) warning("Sequence with non acgtu characters!")
 	return(length(xx))
}

getName.default = function(object){
 	stop("no name")
}

getAnnot.default <- function(object, nbl){ 
 	stop("no annotation for this sequence")
}

getLocation.default = function(object){
 	stop("no information about the position")
}

getKeyword.default = function(object){
 	stop("no keyword for this sequence")
}

getTrans.default <- function(object, frame = 0, sens = "F", numcode = 1, NAstring = "X", ambiguous = FALSE){
	translate(object, frame, sens, numcode, NAstring = NAstring, ambiguous = ambiguous)
}




	########################################################################################################
	#		Classe de sequence SeqFastadna et ses méthodes:                                        #
	#	La classe de séquence SeqFasta pour les séquences résultants de la lecture d'un fichier au     # 
	#	format fasta.                                                                                  #
	########################################################################################################

	##################################################################################################
	# as.SeqFasta sera appelée au moment de la lecture d'un fichier au format fasta par read.fasta() #
	##################################################################################################

as.SeqFastadna = function(object, name = NULL, Annot = NULL){
	object = tolower(object)
	attributes(object) = list(name = name, Annot = Annot)
	class(object) = "SeqFastadna"	
        return(object)
        }

is.SeqFastadna = function(object){
	inherits(object,"SeqFastadna")
}

getSequence.SeqFastadna <- function(object, as.string = FALSE){
  if( ! as.string ){
    return(object)
  } else {
    return(c2s(object))
  }
}

getFrag.SeqFastadna = function(object, begin, end){
	if(end > getLength(object)) stop("invalid end")	
	newSeq = object[begin:end]
	newSeq = as.SeqFrag(newSeq, begin, end, compl = TRUE, name = getName(object))
	return(newSeq)
	}

getLength.SeqFastadna = function(object){
	return(length(object))
	}

getName.SeqFastadna = function(object){
	return(attr(object,"name"))
}

getAnnot.SeqFastadna = function(object,nbl){
	return(attr(object,"Annot"))
}

summary.SeqFastadna <- function(object, alphabet = s2c("acgt"), ...){
	length <- getLength(object)
	compo <- count(object, 1, alphabet = alphabet)
	return(list(length = length , composition = compo, GC = GC(object)))
}

getTrans.SeqFastadna <-  function(object, frame = 0, sens = "F", numcode = 1, NAstring = "X", ambiguous = FALSE){
	translate(object, frame = frame, sens = sens, numcode = numcode, NAstring = NAstring, ambiguous = ambiguous)
}
	


	


	###############################################################################
	#		Classe de sequences SeqFastaAA et ses méthodes:               #
	###############################################################################

as.SeqFastaAA = function(object, name = NULL, Annot = NULL){
	attributes(object)=list(name = name, Annot= Annot)
	class(object)="SeqFastaAA"	
        return(object)
        }

is.SeqFastaAA = function(object){
	inherits(object,"SeqFastaAA")
}

getSequence.SeqFastaAA <- function(object, as.string = FALSE){
  if( ! as.string ){
    return(object)
  } else {
    return(c2s(object))
  }
}


getFrag.SeqFastaAA = function(object, begin, end){
	if(end > getLength(object)) stop("invalid end")	
	newSeq = object[begin:end]
	newSeq = as.SeqFrag(newSeq, begin, end, compl = TRUE, name = getName(object))
	return(newSeq)
	}


getLength.SeqFastaAA = function(object){
	return(length(object))
	}


getName.SeqFastaAA = function(object){
	return(attr(object,"name"))
}

getAnnot.SeqFastaAA = function(object,nbl){
	return(attr(object,"Annot"))
}

summary.SeqFastaAA = function(object,...){
	length = getLength(object)
	compo = table(factor(object, levels = levels(SEQINR.UTIL$CODON.AA$L)))
	return(list(length = length, composition=compo/length, AA.Property=AAstat(object,plot=FALSE)[[2]]))
}


####################################################################################################
#												   #
#	Classe de Sequences SeqAcnucWeb                                                            #
#												   #			
####################################################################################################





as.SeqAcnucWeb = function( object, length, frame, ncbigc, socket = F  ){

	class(object)="SeqAcnucWeb"
	attributes(object)=list(class="SeqAcnucWeb",socket=socket,length=length,frame=frame,ncbigc=ncbigc)
	object
}


is.SeqAcnucWeb = function( object ){	
	inherits(object ,"SeqAcnucWeb")
}



#JRL ajout as.string

getSequence.SeqAcnucWeb = function(object, as.string = FALSE){
  b <- attr(object, "length")
  getSequenceSocket(attr(object,"socket"),object,start=1,length=b, as.string = as.string)
}



getFrag.SeqAcnucWeb = function(object ,begin, end ){

	b = getLength(object)
	if((end > b) || (begin > b)) stop("born out of limits")  
	bb=end-begin+1
	newSeq = getSequenceSocket(attr(object,"socket"),object,start=begin,length=bb)
	newSeq = as.SeqFrag(newSeq,begin=begin,end=end,compl=TRUE,name=getName(object))
	return(newSeq)
}



getName.SeqAcnucWeb = function(object ){	

	return( as.character(object) )

}

#simon:
getLength.SeqAcnucWeb = function( object ){

	return( attr(object,"length"))

}



getAnnot.SeqAcnucWeb <- function(object, nbl = 10000){
		
	return( readAnnots.socket( socket= attr(object,"socket"),name = object, nl = nbl) ) 

}


getKeyword.SeqAcnucWeb = function(object){
	
	return( unlist(getKeywordsocket( socket= attr(object,"socket"), name=object)))
}

getLocation.SeqAcnucWeb = function(object){ 
	
	return( getLocationSocket( socket= attr(object,"socket"), name=object))
}



#
# Translation of CDS into proteins from sequences from an ACNUC server. By
# default, the genetic code and the frame are automatically propagated.
#
getTrans.SeqAcnucWeb <- function(object, frame = "auto", sens = "F", numcode = "auto", NAstring = "X", ambiguous = FALSE){
  dnaseq <- getSequence(object)
  if(numcode == "auto") {
    numcode <- attr(object, "ncbigc")
  }
  numcode <- as.numeric(numcode)
  if(frame == "auto") {
    frame <- attr(object, "frame")
  }
  frame <- as.numeric(frame)
  translate(seq = dnaseq, frame = frame, numcode = numcode, NAstring = NAstring, ambiguous = ambiguous) 
}




	############################################################################
	#		Classe de sequences SeqFrag et ses méthodes:               #
	############################################################################




as.SeqFrag = function(object,begin,end,compl=FALSE,name="frag"){
	if(compl){ attr(object,"seqMother") = name }
	else attr(object,"seqMother") = getName(seq)
        attr(object,"begin") = begin
	attr(object,"end") = end
	class(object) = "SeqFrag"
        return(object)
        }

is.SeqFrag = function(object){
	inherits(object,"SeqFrag")
}


getSequence.SeqFrag <- function(object, as.string = FALSE){
  if( ! as.string ){
    return(object)
  } else {
    return(c2s(object))
  }
}


getFrag.SeqFrag = function(object,begin,end){
        if((end<begin) || (end>getLength(object)))  stop("invalid end")
        newBegin = attr(object,"begin")+begin-1
        newEnd = attr(object,"begin")+end-1
	newSeq = object[begin:end]
        newSeq = as.SeqFrag(object = newSeq, begin = newBegin, end = newEnd, compl = TRUE, name = getName(object))
	return(newSeq)
        }

getLength.SeqFrag = function(object){
	return(attr(object,"end")-(attr(object,"begin")+1))
}

getName.SeqFrag = function(object){
	return(attr(object,"seqMother"))
}

getTrans.SeqFrag <- function(object, frame=0, sens= "F", numcode=1, NAstring = "X", ambiguous = FALSE){
	translate(object, frame = frame, sens = sens, numcode = numcode, NAstring = NAstring, ambiguous = ambiguous)
}



	
