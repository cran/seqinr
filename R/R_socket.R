###################################################################################################
#                                                                                                 #
# Functions to communicate with a remote ACNUC database through sockets.                          #
#                                                                                                 #
###################################################################################################


###################################################################################################
#                                                                                                 #
#                                         choosebank                                              #
#                                                                                                 #
# To select an ACNUC database or to get the list of available databases from an ACNUC server.     #
#                                                                                                 #
###################################################################################################

choosebank <- function(bank = NA , host = "pbil.univ-lyon1.fr", port = 5558, verbose = FALSE,
              timeout = 5, infobank = FALSE, tagbank = NA){

  #
  # Print parameter values if verbose mode is on:
  #
  if(verbose){ 
    cat("Verbose mode is on, parameter values are:\n")
    cat(paste("  bank = ", deparse(substitute(bank)), "\n"))
    cat(paste("  host = ", deparse(substitute(host)), "\n"))
    cat(paste("  port = ", deparse(substitute(port)), "\n"))
    cat(paste("  timeout = ", deparse(substitute(timeout)), "seconds \n"))
    cat(paste("  infobank = ", deparse(substitute(infobank)), "\n"))
    cat(paste("  tagbank = ", deparse(substitute(tagbank)), "\n"))
  }
  
  #
  # Check parameter values (to be completed):
  #
  if( !is.na(tagbank) ){
    if(verbose) cat("I'm checking the tagbank parameter value...\n")
    if( !(tagbank %in% c("TEST", "TP")) ){
      if(verbose) cat("... and I was able to detect an error.\n")
      stop("non allowed value for tagbank parameter.\n")
    } else {
      if(verbose) cat("... and everything is OK up to now.\n")
    }  
  }
  
  #
  # Check that sockets are available:
  #
  if(verbose) cat("I'm ckecking that sockets are available on this build of R...\n")
  if( !capabilities("sockets") ){
    stop("Sockets are not available on this build of R.")
   } else {
    if(verbose) cat("... yes, sockets are available on this build of R.\n")
  }
  
  # 
  # Try to open socket connection:
  #
  if(verbose) cat("I'm trying to open the socket connection...\n")
  oldtimeout <- getOption("timeout")
  options(timeout = timeout)
  socket <- try( socketConnection( host = host, port = port, server = FALSE, blocking = TRUE))
  options(timeout = oldtimeout)
  if(inherits(socket, "try-error")) {
    errmess <- paste("I wasn't able to open the socket connection:\n",
                     "  o Check that your are connected to the internet.\n",
                     "  o Check that port", port, "is not closed by a firewall.\n",
                     "  o Try to increase timeout value (current is", timeout, "seconds).\n")
    stop(errmess)
  } else {
    if(verbose) cat("... yes, I was able to open the socket connection.\n")
  }

  #
  # Read the answer from server:
  #
  if(verbose) cat("I'm trying to read answer from server...\n")
  rep1 <- readLines(socket, n = 1)
  if(verbose) cat(paste("... answer from server is:", rep1, "\n"))
  
  #
  # Client ID definition : seqinr + package version number
  #  (internal note: log file is: /mnt/users/ADE-User/racnuc/log)
  #
  clientID <- paste("seqinr_", packageDescription("seqinr")$Version, sep = "")
  if(verbose) cat(paste("I'm trying to identify myself as", clientID, "to the server...\n"))
  request <- paste("clientid&id=", clientID, sep = "")
  writeLines( request, socket, sep = "\n")
  rep <- readLines(socket, n = 1)  
  if(verbose) cat(paste("... answer from server is:", rep, "\n"))
  res <- parser.socket(rep)  
  if( res[1] == "0") {
    if(verbose) cat("... and everything is OK up to now.\n")  
  } else {
    stop("I don't know what this error code means for clientid, please contact package maintener.\n")
  }
  ###############################################################################
  #
  # Try to get the list of available banks from server:
  #
  ###############################################################################

  if(verbose) cat("I'm sending a knowndbs request to server...\n")
  if( !is.na(tagbank) ){
    askforbank <- paste("knowndbs&tag=", tagbank, sep = "")
    if(verbose) cat("... and the tagbank value wasn't empty.\n")
  } else {
    askforbank <- "knowndbs"
    if(verbose) cat("... and the tagbank value was empty.\n")
  }
  writeLines(askforbank, socket, sep = "\n")
  rep <- readLines(socket, n = 1)
  nbank <- as.numeric(parser.socket(rep))
  if(verbose) cat(paste("... there are", nbank, "banks available from server.\n"))
  
  #
  # Read bank infos from server:
  #
  res <- readLines(socket, n = nbank)
  if(verbose) cat(paste(res, "\n"))
  
  resdf <- as.data.frame(list(bank = I(rep("NAbank", nbank)), 
                  status = I(rep("NAstatus", nbank)), 
                  info = I(rep("NAinfo", nbank))))
  for(i in seq_len(nbank))
    resdf[i, ] <- unlist(strsplit(res[i], split = "\\|"))[1:3]
  for(i in seq_len(nbank))
    for(j in seq_len(3))
      resdf[i, j] <- trimSpace(resdf[i, j])   
           
  ###############################################################################
  #
  # If no bank name is given, return the list of available banks from server:
  #
  ###############################################################################
  if( is.na(bank) ){  
  
    if(verbose) cat("No  bank was given...\n")
   
    #
    # Return just bank names or all info depending on infobank parameter value:
    #
    if( !infobank ){
      if(verbose) cat("infobank parameter is FALSE, I'm just returning bank names\n")
      return(as.vector(resdf$bank))
    } else {
      if(verbose) cat("infobank parameter is TRUE, I'm returning all bank infos\n")
      return(resdf) 
      }
  } else {

    ###############################################################################
    #
    # If a bank name is given, try to open it from server:
    #
    ###############################################################################
    
    # 
    # Try to open bank from server:
    #
    if(verbose) cat("I'm trying to open the bank from server...\n")
    request <- paste("acnucopen&db=", bank, sep = "") 
    writeLines( request, socket, sep = "\n")
    rep2 <- readLines(socket, n = 1)
    if(verbose) cat(paste("... answer from server is: ", rep2, "\n"))             

    #
    # Check answer from server:
    #
    if(verbose) cat("I'm trying to interpret the answer from server...\n")
    res <- parser.socket(rep2)
    
    if( res[1] == "0") {
      if(verbose) cat("... and everything is OK up to now.\n")
      
      #
      # Try to get informations from HELP file: 
      #
      if(verbose) cat("I'm trying to get information on the bank...\n")
      request <- "ghelp&file=HELP&item=CONT"
      writeLines( request, socket, sep = "\n")
      rep2 <- readLines(socket, n = 1)
      if(verbose) cat(paste("... and answer from server is: ", rep2, ".\n"))   
      res2 <- parser.socket(rep2)
      nblhelp <- res2[1]
      if(verbose) cat("Number of lines=", nblhelp,".\n")
      if (as.numeric(nblhelp) > 2){
        bankhelp <- readLines(socket, n = (as.integer(nblhelp) - 1))
        for(i in seq_len(length(bankhelp))) bankhelp[i] <- trimSpace(bankhelp[i])
        bankrel <- bankhelp[1]
      } else {
        bankhelp <- "there is no information available about the contents of this bank"
        bankrel <-  "there is no information available about the contents of this bank"
        #cat("Note: there is no information available about the contents of this bank.\n")
      }
      
      #
      # Try to get status info:
      #
      status <- "unknown"
      for(i in seq_len(nbank)){
          if (resdf[i,1] == bank) status<-resdf[i,2]
      }

      #
      # Set up the ACNUC server for following queries:
      #
      if(verbose) cat("I'm trying to set up the server for following queries...\n")
      writeLines("prep_requete", socket, sep = "\n")
      rep3 <- readLines(socket, n = 1)
      
      #
      # Re-patch pas beau:
      #
      if(length(rep3) == 0){
        if(verbose) cat("... answer from server is empty!\n")
        while(length(rep3) == 0){
          if(verbose) cat("... reading again.\n")
          rep3 <- readLines(socket, n = 1)
        }
      }
      if(verbose) cat("... answer from server is: ", rep3, "\n")
      res3 <- parser.socket(rep3)
      if( res3[1] == "0") {
        if(verbose) {
          cat("... and everything is OK up to now.\n")
          cat(paste("... and there are", res3[2], "free lists available from server.\n"))
        }
      } else {
        if(verbose) cat("I was able to detect an error while seting up remote bank.\n")
        stop("There was an error while seting up remote bank.\n")
      }
      
      #
      # Build result and assign it in the global environment:
      #
      res<-list(socket = socket, bankname = bank, totseqs = res[3], totspecs = res[4], totkeys = res[5], release = bankrel, status = status,details = bankhelp)
      assign("banknameSocket", res, .GlobalEnv)
      invisible(res)
    } else {
      if(verbose) cat("I was able to detect an error while opening remote bank.\n")
      rm(socket)
      if( res[1] == "3" ){
        stop(paste("Database with name -->", bank, "<-- is not known by server.\n", sep = ""))
      }
      if( res[1] == "4" ){
        stop(paste("Database with name -->", bank, "<-- is currently off for maintenance, please try again later.\n", sep = ""))
      }
      if( res[1] == "5" ){
        stop(paste("Database with name -->", bank, "<-- is currently opened and has not been closed.\n", sep = ""))
      }
      stop("I don't know what this error code means for acnucopen, please contact package maintener.\n")
    }
  }
} 

###################################################################################################
#                                                                                                 #
#                                         closebank                                               #
#                                                                                                 #
# To close an ACNUC database                                                                      #
#                                                                                                 #
###################################################################################################

closebank <- function(bank = NA , host = "pbil.univ-lyon1.fr", port = 5558, verbose = FALSE){

  #
  # Check arguments:
  #
  if(verbose) cat("I'm supposed to check arguments here...\n")
  if(verbose) cat("... but I wasn't explained how to do that for now.\n")
  
  #
  # Use default bank if no bank is provided:
  #
  if( is.na(bank) ){
    if(verbose) cat("No bank argument was provided, I'm trying to close default bank.\n")
    bank <- get("banknameSocket", .GlobalEnv)
  }
  
  #
  # Send "acnucclose" to server:
  #
  if(verbose) cat("I'm trying to send an acnucclose message to server...\n")
  writeLines("acnucclose", bank$socket, sep = "\n")
  rep <- readLines(bank$socket, n = 1)
  if(verbose) cat("... answer from server is: ", rep, "\n")
  res <- parser.socket(rep)
  if( res[1] == "0") {
    if(verbose) cat("... and everything is OK up to now.\n")
  } else {
    if(verbose) cat("I was able to detect an error while closing remote bank.\n")
    if(res[1] == "3") stop("no database was opened by the server on this socket.\n")
    stop("I do not understand what this error code means, please contact package maintainer.\n")
  }
  
  #
  # Send "quit" to server:
  #
  if(verbose) cat("I'm trying to send a quit message to server...\n")
  writeLines("quit", bank$socket, sep = "\n")
  rep <- readLines(bank$socket, n = 1)
  if(verbose) cat(paste("... answer from server is: -->", rep, "<--\n", sep = ""))
  if(rep == "OK acnuc socket stopped"){
    if(verbose) cat("... and everything is OK up to now, which is not too bad since we have closed everything!\n")
  } else {
    if(verbose) cat("... and I do not understand this answer from server.\n")
    warning("I do not understand answer for quit from server, please contact package maintainer.\n")
  }
  
  #
  # Close connection:
  #
  if(verbose) cat("I'm trying to close connection...\n")
  res <- try(close.connection(bank$socket))
  if( inherits(res, "try-error") ){
    if(verbose) cat("I was able to detect an error while closing connection.\n")
    stop("problem while closing connection.\n")
  } else {
    if(verbose) cat("... and everything is OK up to now.\n")
  }
}

###################################################################################################
#                                                                                                 #
#                                         parser.socket                                           #
#                                                                                                 #
#                      Utility function to parse answers from ACNUC server.                       #
#                                                                                                 #
###################################################################################################

parser.socket <- function(p)
{
  if(is.null(p)){
    return(NULL)
  }
  p1 <- s2c(p)
  b <- grep("=", p1)
  a <- c(grep("&", p1), length(p1) + 1)
  return(unlist(lapply(seq_len(length(a)), function(x){substr(p, (b[x]+1), (a[x] - 1))})))
}
  
###################################################################################################
#                                                                                                 #
#                                         getSequenceSocket                                       #
#                                                                                                 #
###################################################################################################

getSequenceSocket <- function( socket, name, start, length, as.string = FALSE){
  request <- paste("gfrag&name=", name, "&start=", start, "&length=", formatC(length, format = "d"), sep = "")
  writeLines(request, socket, sep="\n")
  s <- readLines(socket, n = 1)

  if(length(s) == 0){
    warning(paste("invalid sequence name:", name))
    return(NA)
  } else {   
    sequence <- unlist(strsplit(s, split = "&"))[2]
    if( as.string ){
      return(sequence)
    } else {
      return(s2c(sequence))
    }
  }
}
  
###################################################################################################
#                                                                                                 #
#                                         getAttributsocket                                       #
#                                                                                                 #
# To get sequence attributes from server.                                                         #
#                                                                                                 #
###################################################################################################

getAttributsocket <- function( socket, name){
  request <- paste("isenum&name=", name, sep = "")
  writeLines( request, socket, sep = "\n")
  res <- readLines(socket, n = 1)
  p <- parser.socket(res)
  return( list(length = as.numeric(p[2]), frame = as.numeric(p[3]), gencode = as.numeric(p[4])) )
}

###################################################################################################
#                                                                                                 #
#                                         readAnnots.socket                                       #
#                                                                                                 #
###################################################################################################

readAnnots.socket <- function(socket, name, nl){
#
# Check arguments:
#
  if(nl <= 0){
    warning("Negative or zero value for argument nl, forced to 1.")
    nl <- 1
  }
#
# Build request:
#
  request <- paste("read_annots&name=", name, "&nl=", nl, sep = "")
  writeLines(request , socket, sep="\n")
#
# Read result from server:
#  
  res <- readLines(socket , n = nl)
#
# Remove the "nl=xx&" answer from server on first line:
#
  newfirstline <- unlist(strsplit(res[1], "&"))[2]
  res[1] <- newfirstline
  return(res)
}

###################################################################################################
#                                                                                                 #
#                                         getNumber.socket                                        #
#                                                                                                 #
###################################################################################################

getNumber.socket <- function( socket, name){
  request <- paste("isenum&name=", name, sep = "")
  writeLines(request, socket, sep = "\n")
  s <- readLines(socket, n = 1)
  return(parser.socket(s)[1])
}

###################################################################################################
#                                                                                                 #
#                                         query                                                   #
#                                                                                                 #
###################################################################################################

query <- function(listname, query, socket = "auto", invisible = TRUE, verbose = FALSE, virtual = FALSE) 
{
  #
  # Definition of the utility function simon() used only in query():
  #
  simon <-function(res, socket) {
    x <- parser.socket(res)
    y <- (x[c(2,3,6,7)])
    acnucy <- as.SeqAcnucWeb(substring(y[1], 2, nchar(y[1]) - 1), y[2], y[3], y[4], socket = socket)
    acnucy
  }
  #
  # Check arguments:
  #
  if(verbose) cat("I'm checking the arguments...\n")

  if (socket == "auto"){
    if(verbose) cat("No socket were specified, using default.\n")
    socket <- get("banknameSocket", .GlobalEnv)$socket
  }
  
  if( !inherits(socket, "sockconn") ) stop(paste("argument socket = ", socket, "is not a socket connection."))
  if( !is.character(listname) ) stop(paste("argument listname = ", listname, "is not a character string."))
  if( !is.character(query) ) stop(paste("argument query = ", query, "is not a character string."))
  if( !is.logical(invisible) ) stop(paste("argument invisible = ", invisible, "should be TRUE or FALSE."))
  if( is.na(invisible) ) stop(paste("argument invisible = ", invisible, "should be TRUE or FALSE."))
  if(verbose) cat("... and everything is OK up to now.\n")
  
  #
  # Check the status of the socket connection:
  #
  if(verbose) cat("I'm checking the status of the socket connection...\n")
  #
  # Ca marche pas: summary.connection leve une exception et on ne va pas plus loin
  #
  if(!isOpen(socket)) stop(paste("socket:", socket, "is not opened."))
  if(!isOpen(socket, rw = "read")) stop(paste("socket:", socket, "can not read."))
  if(!isOpen(socket, rw = "write")) stop(paste("socket:", socket, "can not write."))
  if(verbose) cat("... and everything is OK up to now.\n")

  #
  # Send request to server:
  #
  if(verbose) cat("I'm sending query to server...\n")
  request <- paste("proc_requete&query=\"", query, "\"&name=\"", listname, "\"", sep = "")
  writeLines(request, socket, sep = "\n")
  res <- readLines(socket, n = 1)
  #
  # C'est ici qu'il y a un probleme de timeout. Suit un patch pas beau
  #
  
  if(verbose) cat(paste("... answer from server is:", res, "\n"))
  if(length(res) == 0){
    if(verbose) cat("... answer from server is empty!\n")
    while(length(res) == 0){
      if(verbose) cat("... reading again.\n")
      res <- readLines(socket, n = 1)
    }
  }
  #
  # Analysing answer from server:
  #
  if(verbose) cat("I'm trying to analyse answer from server...\n")
  p <- parser.socket(res)
  if(p[1] != "0"){
    if(verbose) cat("... and I was able to detect an error.\n") 
    stop(paste("invalid request:", p[2], sep = ""))
  }
  
  if(verbose) cat("... and everything is OK up to now.\n")
  lrank <- p[2]
  if(verbose) cat(paste("... and the rank of the resulting list is:", lrank, ".\n"))
  nelem <- as.integer(p[3])
  if(verbose) cat(paste("... and there are", nelem, "elements in the list.\n"))
  typelist <- p[4]
  if(verbose) cat(paste("... and the elements in the list are of type", typelist, ".\n"))
  if(typelist == "SQ"){
    if(p[5] == "T"){
      if(verbose) cat("... and there are only parent sequences in the list.\n")
    } else {
      if(verbose) cat("... and there are *not* only parent sequences in the list.\n")
    }
  }
  
  #
  # Get full list informations: 
  #
  if( !virtual ){
    if(verbose) cat("I'm trying to get the infos about the elements of the list...\n")
    writeLines(paste("nexteltinlist&lrank=", lrank, "&first=1&count=", nelem, sep = ""), socket, sep = "\n")
    res <- readLines(socket, n = nelem, ok = FALSE)
    if( length(res) != nelem )
    {
      if(verbose) cat("... and I was able to detect an error...\n")
      stop(paste("only", length(res), "list elements were send by server out of", nelem, "expected.\n")) 
    } else {
      if(verbose) cat(paste("... and I have received", nelem, "lines as expected.\n"))
    }
  
    liste <- lapply(res, simon, socket=socket) 

  #
  # Virtual list case:
  #
  } else {
    if(verbose) cat("I'am *not* trying the infos about the elements of the list since virtual is TRUE.\n")
    liste <- NA
  }
  #
  # Return results:
  #
  result <- list(call = match.call(), name = listname, nelem = nelem, typelist = typelist, 
    req = as.list(liste), socket = socket)
  class(result) <- c("qaw")
  assign(listname, result, env = .GlobalEnv)
  if(invisible == TRUE){
    invisible(result)
  } else {
    return(result)
  }
}

###################################################################################################
#                                                                                                 #
#                                         print.qaw                                               #
#                                                                                                 #
###################################################################################################

print.qaw <- function(x, ...)
{
  cat("\n")
  cat("\n$socket: ")
  print(x$socket)
  cat("\n$banque: ")
  #cat(get("bankName",env=.GlobalEnv)) # Ca pas bon
  cat("\n$call: ")
  print(x$call)
  cat("$name: ")
  print(x$name)
  cat("\n")
  sumry <- array("", c(1, 4), list(1, c("list", "length", "mode", "content")))
  sumry[1, ] <- c("$req",length(x$req),"character","sequences")
  class(sumry) <- "table"
  print(sumry)
  cat("\n")
}

###################################################################################################
#                                                                                                 #
#                                         getKeywordsocket                                        #
#                                                                                                 #
###################################################################################################

getKeywordsocket <- function(socket, name){
  #modif simon
  writeLines(paste("isenum&name=", name, sep = ""), socket, sep = "\n")
  res <- readLines(socket, n = 1)
  number <- parser.socket(res)[1] 

  writeLines(paste("readsub&num=", number, sep = ""), socket, sep = "\n")
  res2 <- readLines(socket, n = 1) 
  rr <- parser.socket(res2)

  writeLines(paste("readshrt&num=", rr[7], sep = ""), socket, sep = "\n")
  res3 <- readLines(socket, n = 1)
  #modif simon   

  # Get the nb of kw (not used here)
  # nbkws <- parser.socket(res3)[2]

  #recupere la liste de paires val, next 
  tmpl <- unlist(strsplit(res3, "&"))
  #transforme en liste
  tmpl <- unlist(strsplit(tmpl[3],","))
  kwl <- unlist(tmpl)[c(TRUE, FALSE)]

  lapply(kwl, function(x){
    writeLines(paste("readkey&num=", x, sep = ""), socket, sep = "\n")  
    res4 <- readLines(socket, n = 1)
    res <-parser.socket(res4)[2]
    substring(res[1], 2, nchar(res[1]) - 1)
  })

} 

###################################################################################################
#                                                                                                 #
#                                         getLocationSocket                                       #
#                                                                                                 #
###################################################################################################

getLocationSocket <- function( socket, name){

   writeLines(paste("isenum&name=",name,sep=""),socket,sep="\n")
         res = readLines( socket , n=1 )
         number = parser.socket(res)[1] 
   
         writeLines(paste("readsub&num=",number,sep=""),socket,sep="\n")
         res2 = readLines( socket , n=1 ) 
         rr = parser.socket(res2)
  
   # Test si subsequence           
     
   l=list() 
         if(as.numeric(rr[5]) != 0){
     warning("It's a parent sequence\n")
     return( NA )
    }
         else {
    i=1
    writeLines(paste("readext&num=",rr[6],sep=""),socket,sep="\n")    
    res3 = readLines( socket , n=1 )
    r = parser.socket(res3)
    l[[i]] = as.numeric(c(r[3],r[4]))
    n=r[5] 
  }
        while(as.numeric(n) != 0){
    i=i+1
    writeLines(paste("readext&num=",n,sep=""),socket,sep="\n")   
    res4 = readLines( socket , n=1 )
    rrr = parser.socket(res4)
    l[[i]] = as.numeric(c(rrr[3],rrr[4]))
    n=rrr[5]
      }
  return(l)
} 


###################################################################################################
#                                                                                                 #
#                              readfirstrec                                                       #
#                                                                                                 #
#                Returns the record count of the specified ACNUC index file.                      #                                                                                                 #
#                                                                                                 #
# ==>   readfirstrec&type=[AUT|BIB|ACC|SMJ|SUB|LOC|KEY|SPEC|SHRT|LNG|EXT|TXT]                     #
# <==  code=xx&count=xx                                                                           #
# Returns the record count of the specified ACNUC index file.                                     #
# Code != 0 indicates error.                                                                      #
#                                                                                                 #
###################################################################################################

readfirstrec <- function(socket = "auto", type)
{
  allowedtype <- c("AUT", "BIB", "ACC", "SMJ", "SUB", "LOC", "KEY", "SPEC", 
                   "SHRT", "LNG", "EXT", "TXT")
  if(missing(type)){
    return(allowedtype)
  }
  
  #
  # Use default bank if no socket is given:
  #
  if(socket == "auto"){
    socket <- get("banknameSocket", .GlobalEnv)$socket
  }
  
  #
  # Build the request:
  #
  request <- paste("readfirstrec&type=", type, sep = "", collapse = "")
  
  #
  # Send request:
  #
  
  writeLines(request, socket, sep = "\n") 
  #
  # Read answer from server:
  #
  
  s <- readLines(socket, n = 1)
  rep <- parser.socket(s)
  
  #
  # Check answer from server:
  #
  if(rep[1] != "0"){
    warning("Server returns an error")
    return(NA)
  } else {
    return(as.numeric(rep[2]))
  }
}



