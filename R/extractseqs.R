# Recuperation des sequences
# Simon Octobre 2006




##########################################################################################
#                                                                                                 #
#                                         extractseqs.socket                                       #
#                                                                                                 #
##########################################################################################

extractseqs <- function(listname, socket = autosocket(), format="fasta", operation="simple", feature="xx", bounds="xx", minbounds="xx", verbose = FALSE, nzlines=1000, zlib = FALSE){
  if (zlib == TRUE && .Platform$OS.type == "windows") {
  	stop(paste("Zlib compressed sockets is not implemented for windows.\n"))
	}
  debug<-0
  if (verbose) debug <- 1

  if(verbose) cat("I'm checking the arguments...\n")

  if( !inherits(socket, "sockconn") ) stop(paste("argument socket = ", socket, "is not a socket connection."))
  if( !is.character(listname) ) stop(paste("argument listname = ", listname, "is not a character string."))
  if(verbose) cat("... and everything is OK up to now.\n")
  
    
# Check arguments:
# Check if  format is acnuc", "fasta", or "flat"

 if(verbose) cat("Format is ",format,"\n")
 if ((format != "fasta") &&  (format != "flat") && (format != "acnuc")) stop(paste("argument format = ", format, "is wrong. Format should be \"fasta\", \"flat\" or \"acnuc\"! "))
 

# Check if operation "is simple", "translate", "fragment", "feature" or "region"

 if(verbose) cat("Operation is ",operation,"\n")
 if ((operation != "simple") && (operation != "translate") && (operation != "fragment") && (operation != "feature") && (operation != "region") ) {
 	stop(paste("argument operation = ", operation, "is wrong. Operation should be \"simple\", \"translate\", \"fragment\", \"feature\" or \"region\"! "))
	}
 

# Check optionals

if ((feature != "xx") && (verbose))cat("feature = ", feature, ".\n")
if ((bounds != "xx") && (verbose))  cat("bounds = ", bounds, ".\n")
if ((minbounds != "xx") && (verbose))  cat("minbounds = ", minbounds, ".\n")


if ((operation == "feature") && (feature =="xx")) stop(paste("You should specify a feature!\n"))

if ((operation == "fragment") && (bounds =="xx")) stop(paste("You should specify bounds!\n"))

if ((operation == "region") && ((bounds =="xx") || (feature =="xx"))) stop(paste("You should specify bounds and region!\n"))

#
# Build request:
#
# listname is a list

  lrank <- glr(listname)
  if(verbose) cat("The rank of the list ",listname, "is ",lrank,".\n")
  if (is.na(lrank)) {
  	stop(paste("Problem in rank list!\n"))
  	}
  
  request <- paste("extractseqs&lrank=", lrank, "&format=", format, "&operation=", operation, sep = "")
  if (feature != "xx") request <- paste(request,"&feature=", feature, sep = "")
  if (bounds != "xx") request <- paste(request,"&bounds=",bounds, sep = "")
  if (minbounds != "xx") request <- paste(request,"&minbounds=",minbounds, sep = "")
  
  if(zlib) {
    request <- paste(request,"&zlib=T", sep = "")
  }
  else {
    request <- paste(request,"&zlib=F", sep = "")
  }
  if(verbose) cat("request : ",request,"\n")
  
  # Write request into the socket:
  
  writeLines(request , socket, sep="\n")
	
  # Read result from server:

  if(zlib) {
    lastres <-  .Call("getzlibsock", socket, nzlines, debug,PACKAGE = "seqinr")
  }
  else {
    lastres <- readLines(socket)
  }
  if(zlib) {
    #
    # Remove empty lines at the end of lastres:
    #
    return( lastres[nchar(lastres) != 0] )
  }
  else {
    #
    # Remove flags from server:
    #
    lastres <- lastres[-1]                       # code=0
    lastres <- lastres[-length(lastres)]         # extractseqs END. 
    lastres <- lastres[-grep("count=", lastres)] # count=
    return(lastres)
  }
}

#
# Define a shorthcut for extractseqs
#
exseq <- extractseqs


