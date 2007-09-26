###################################################################################################
#                                                                                                 #
#                              crelistfromclientdata                                              #
#                                                                                                 #
#                To create on server a bitlist from data lines sent by client                     #                                                                                                 #
#                                                                                                 #
# ==>  crelistfromclientdata{&type=[SQ|AC|SP|KW]}&nl=xx                                           #
# 0 or more lines of data sent by client to server                                                #
# <==  code=0&name="xx"&lrank=xx&count=xx\n                                                       #
# To create on server a bitlist from data lines sent by client.                                   #
# type: the type of data sent to server (SQ=seqs, AC=acc nos, SP=species, KW=keywords)            #
#      SQ by default                                                                              #
# nl: announces the number of data lines that follow (0 is OK)                                    #
# code: 0 iff OK                                                                                  #
#      3 no list creation is possible                                                             #
#      4 EOF while reading the nl lines from client                                               #
# name: name of bitlist created from this data                                                    #
# lrank: rank of this bitlist                                                                     #
# count: count of elements in bitlist                                                             #
#                                                                                                 #
###################################################################################################

crelistfromclientdata <- function(listname, file, type, socket = "auto", invisible = TRUE, verbose = FALSE, 
virtual = FALSE) {
  #
  # Check arguments:
  #
  if(verbose) cat("I'm checking the arguments...\n")

  if(!file.exists(file)) stop(paste("input file", file, "doesn't exist."))
  if( ! type %in% c("SQ", "AC", "SP", "KW") ) stop("wrong value for type argument.")
  if (socket == "auto"){
    if(verbose) cat("No socket were specified, using default.\n")
    socket <- get("banknameSocket", .GlobalEnv)$socket
  }
  
  if( !inherits(socket, "sockconn") ) stop(paste("argument socket = ", socket, "is not a socket connection."))
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
  # Read user file:
  #
  infile <- file(description = file, open = "r")
  data <- readLines(infile)
  close(infile)
  nl <- length(data)
  #
  # Send request to server:
  #
  if(verbose) cat("I'm sending query to server...\n")
  request <- paste("crelistfromclientdata&type=", type, "&nl=", nl, sep = "")
  if(verbose) writeLines(c(request, data))
  writeLines(c(request, data), socket, sep = "\n")
  res <- readLines(socket, n = 1)
  #
  # Check for non empty answer from server:
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
    if( p[1] == "3" ) stop("no list creation is possible")
    if( p[1] == "4" ) stop("EOF while reading the nl lines from client")
    stop(paste("unknown error code from server:", p[1]))
  }
  if(verbose) cat("... and everything is OK up to now.\n")
  
  if(verbose){
    cat("listname is:", p[2], "\n")
    cat("list rank is:", p[3], "\n")
    cat("list count of elements is:", p[4], "\n")
  }
  #
  # Use query with user parameters:
  #
  if(invisible){
  invisible(query(listname = listname, query = p[2], socket = socket, invisible = TRUE, 
    verbose = verbose, virtual = virtual))
  } else {
    return(query(listname = listname, query = p[2], socket = socket, invisible = FALSE, 
    verbose = verbose, virtual = virtual))
  }
}

clfcd <- crelistfromclientdata
