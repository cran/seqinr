########################
# char to string
########################

c2s <- function( chars = c("m","e","r","g","e","d") )
{
  return( paste( chars, collapse = "" ) )
}

###########################
# string to char
############################

s2c = function (string) 
{
  if(is.character(string) & is.vector(string)){
  return(.Call("s2c", string, PACKAGE = "seqinr"))
}
  else stop("non-character argument in s2c()")
}



###########################
# Conversion of the numeric encoding of a DNA sequence into
# a vector of chars
############################

n2s <- function(nseq, levels = c("a", "c", "g", "t"), base4 = TRUE)
{
  if( base4 )
    levels[nseq + 1]
  else
    levels[nseq]
}

###############################
# simple numerical encoding of a DNA sequence that by default
# is independent of locale.
###############################

s2n <- function(seq, levels = c("a", "c", "g", "t"), base4 = TRUE)
{
  if( base4 )
    unclass(factor(seq, levels = levels ) ) - 1
  else
    unclass(factor(seq, levels = levels ) )
}

################################
# GC.percent
#################################

GC = function(seq)
{
	c=count(seq,1)
	cc=(c[2]+c[3])/sum(c)
	return(as.vector(cc))
}



##########################################
# Conversion one-letter code to 3-letters code for amino-acids
##########################################

aaa <- function( aa )
{
  aa1 <- s2c("*ACDEFGHIKLMNPQRSTVWY")
  aa3 <- c("Stp", "Ala", "Cys", "Asp", "Glu", "Phe", "Gly", "His", "Ile",
           "Lys", "Leu", "Met", "Asn", "Pro", "Gln", "Arg", "Ser", "Thr",
           "Val", "Trp", "Tyr")
  convert <- function( x )
  {
    if( all( x != aa1 ) )
    { 
      warning("Unknown one letter code for aminoacid")
      return( NA )
    }
    else
    {
      return( aa3[which( x == aa1 )] )
    }
  }
  return( as.vector(unlist(sapply( aa, convert ) ) ) )
}

##########################################
# Conversion 3-letters code to one letter code for amino-acids
##########################################

a <- function( aa )
{
  aa1 <- s2c("*ACDEFGHIKLMNPQRSTVWY")
  aa3 <- c("Stp", "Ala", "Cys", "Asp", "Glu", "Phe", "Gly", "His", "Ile",
           "Lys", "Leu", "Met", "Asn", "Pro", "Gln", "Arg", "Ser", "Thr",
           "Val", "Trp", "Tyr")
  convert <- function( x )
  {
    if( all( x != aa3 ) )
    { 
      warning("Unknown 3-letters code for aminoacid")
      return( NA )
    }
    else
    {
      return( aa1[which( x == aa3 )] )
    }
  }
  return( as.vector(unlist(sapply( aa, convert ) ) ) )
}



#########################################
# revers a sequence
#######################################

invers<-function(seq)
{
	rev(seq)
}

##########################################
#complement a sequences
###########################################

comp<-function(seq){
	return(as.vector(n2s((3-s2n(seq)))))
}	


######################
# GC3		     #
######################

GC3 = function(seq){
	sequence <- splitseq( seq )
	codons <- words(length = 3, alphabet = s2c("acgt"))	
	eff=table(factor( sequence , levels=codons))
	f =round(eff/(floor(length(seq)/3)),4)
	return(as.vector(f %*% EXP$CG3))
	}

######################
# GC2		     #
######################


GC2 = function(seq){
	sequence <- splitseq( seq )
	codons <- words(length = 3, alphabet = s2c("acgt"))	
	eff=table(factor( sequence , levels=codons))
	f =round(eff/(floor(length(seq)/3)),4)
	return(as.vector(f %*% EXP$CG2))
	}
