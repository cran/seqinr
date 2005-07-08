########################
# char to string
########################

"c2s" <- function( chars = c("m","e","r","g","e","d") )
{
  return( paste( chars, collapse = "" ) )
}

###########################
# string to char
############################

"s2c" <- function (string) 
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

"n2s" <- function(nseq, levels = c("a", "c", "g", "t"), base4 = TRUE)
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

"s2n" <- function(seq, levels = c("a", "c", "g", "t"), base4 = TRUE)
{
  if( base4 )
    unclass(factor(seq, levels = levels ) ) - 1
  else
    unclass(factor(seq, levels = levels ) )
}

################################
# GC.percent
#################################

"GC" <- function(seq)
{
        sum(seq=='c'|seq=='g')/length(seq)
}



##########################################
# Convert one-letter code to 3-letters code for amino-acids
##########################################

"aaa" <- function( aa )
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

"a" <- function( aa )
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
# reverse a sequence
#######################################

"invers" <- function(seq)
{
	rev(seq)
}

##########################################
#complement a sequences
###########################################

"comp" <- function(seq){
	return(as.vector(n2s((3-s2n(seq)))))
}	


######################
# GC1		     #
######################


"GC1" <- function(seq){
	GC(seq[seq(1,length(seq),by=3)])
}

######################
# GC2		     #
######################


"GC2" <- function(seq){
	GC(seq[seq(2,length(seq),by=3)])
}

######################
# GC3		     #
######################

"GC3" <- function(seq){
        GC(seq[seq(3,length(seq),by=3)])
}
