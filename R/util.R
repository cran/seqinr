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

s2c <- function (string) 
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


################################
# G+C content
#################################

GC <- function(seq, forceToLower = TRUE, exact = TRUE)
{
	#
	# Check that sequence is a vector of chars:
	#
	if(nchar(seq[1]) > 1) stop("sequence is not a vector of chars")
	#
	# Force to lower-case letters if requested:
	#
	if(forceToLower) seq <- tolower(seq)
	
	if(! exact){
		result <- sum(seq == 'c' | seq == 'g')/length(seq)
	} else {
		#
		# First pass to get an estimate of the base content based
		# only on non-amibuous bases:
		#
		na <- sum( seq == "a" )
		nt <- sum( seq == "t" )
		nc <- sum( seq == "c" )
		ng <- sum( seq == "g" )
		#
		# Now we have our firt estimate of GC vs. AT base frequencies:
		#
		ngc <- ng + nc
		nat <- na + nt

                 
		#
		# weak and strong bases are 100% informative with respect
		# to the GC content, we just add them:
		#
		# s : Strong (g or c)
		# w : Weak (a or t)
		#
		ngc <- ngc + sum( seq == "s" )
		nat <- nat + sum( seq == "w" )

                
		
		##########################
		# Ambiguous base section #
		##########################
		
		#
		# m : Amino (a or c)
		#
		nm <- sum( seq == "m")
		ngc <- ngc + nm*nc/(na + nc)
		nat <- nat + nm*na/(na + nc)

               
		#
		# k : Keto (g or t)
		#
		nk <- sum( seq == "k" )
		ngc <- ngc + nk*ng/(ng + nt)
		nat <- nat + nk*nt/(ng + nt)

		#
		# r : Purine (a or g)
		#
		nr <- sum( seq == "r" )
		ngc <- ngc + nr*ng/(ng + na)
		nat <- nat + nr*na/(ng + na)
                
               
		#
		# y : Pyrimidine (c or t)
		#
		ny <- sum( seq == "y" )
		ngc <- ngc + ny*nc/(nc + nt)
		nat <- nat + ny*nt/(nc + nt)

                
		#
		# v : not t (a, c or g)
		#
		nv <- sum( seq == "v" )
		ngc <- ngc + nv*(nc + ng)/(na + nc + ng)
		nat <- nat + nv*na/(na + nc + ng)
		#
		# h : not g (a, c or t)
		#
		nh <- sum( seq == "h" )
		ngc <- ngc + nh*nc/(na + nc + nt)
		nat <- nat + nh*(na + nt)/(na + nc + nt)
		#
		# d : not c (a, g or t)
		#
		nd <- sum( seq == "d" )
		ngc <- ngc + nd*ng/(na + ng + nt)
		nat <- nat + nd*(na + nt)/(na + ng + nt)
		#
		# b : not a (c, g or t)
		#
		nb <- sum( seq == "b" )
		ngc <- ngc + nb*(nc + ng)/(nc + ng + nt)
		nat <- nat + nb*nt/(nc + ng + nt)
		#
		# n : any (a, c, g or t) is not informative, so
		# we compute the G+C content as:
		#
                
		result <- ngc/(ngc + nat)
	}
	return(result)
}

######################
# GC1		     #
######################

GC1 <- function(seq, ...){
	GC(seq[seq(1, length(seq), by = 3)], ...)
}

######################
# GC2		     #
######################

GC2 <- function(seq, ...){
	GC(seq[seq(2, length(seq), by = 3)], ...)
}

######################
# GC3		     #
######################

GC3 <- function(seq, ...){
	GC(seq[seq(3, length(seq), by = 3)])
}


##########################################
# Convert one-letter code to 3-letters code for amino-acids
##########################################

aaa <- function( aa )
{
  aa3 <- c("Stp", "Ala", "Cys", "Asp", "Glu", "Phe", "Gly", "His", "Ile",
           "Lys", "Leu", "Met", "Asn", "Pro", "Gln", "Arg", "Ser", "Thr",
           "Val", "Trp", "Tyr") # One letter code order
  if(missing(aa)) return(aa3)
  aa1 <- a()

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
  if(missing(aa)) return(aa1)
  aa3 <- aaa()

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


