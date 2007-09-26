################################################################################
#
#                                         plot.SeqAcnucWeb
#
################################################################################

plot.SeqAcnucWeb <- function(x,  type = "all", ...){
  verbose <- FALSE # a passer en argument si besoin est
  #
  # Check arguments:
  #
  if(!inherits(x, "SeqAcnucWeb")) stop("Sequence of class SeqAcnucWeb is needed")
  
  socket <- attr(x, "socket")

  types <- getType()$sname
  types <- types[-c(1,3)] # to remove 3'INT and 5'INT
  if(verbose) cat(paste("types:", types, sep = "\n"))
  
  if(type == "all"){
    ptype <- types
  } else {
    if(sum(type %in% types) != length(type)) stop("Please check the type argument !")
    ptype <- type
  }

  #
  # Get parent sequence and plot organization:
  #
  par(mfrow = c(2,1), lend = "butt")
  
  q <- paste("me n=", x, sep="")
  
  query(listname = "me", query = q, socket = socket)
  l <- as.integer(getLength(get("me", .GlobalEnv)$req[[1]]))
  if(verbose) cat("\nl = ", l)

  cx <- c(0, 1.1*l)
  cy <- c(0, 15)
  plot(cx, cy, ann = FALSE, type = "n", axes = FALSE)
  axis(1, col.axis = "blue")
  title(main = paste("Physical position (base) of the subsequences","\n","on the parent sequence",
          get("me", .GlobalEnv)$req[[1]], sep=" "), font.main = 3, col.main = "blue")
  mtext(paste("(length of the parent sequence = ", l, ")", sep=""), cex = 0.7, font = 3)

  # Si x est une sous séquence alors dire le type

  if(get("me", .GlobalEnv)$req[[1]] != x){
    if(verbose) cat("x est une sous-sequence\n")
    writeLines(paste("isenum&name=",x,sep=""),socket,sep="\n")
    res <- readLines( socket , n=1 )
    writeLines(paste("readsub&num=",as.numeric(parser.socket(res)[1]),sep=""),socket,sep="\n")
    res <- readLines( socket , n=1 )
    writeLines(paste("readsmj&num=",as.numeric(parser.socket(res)[4]),"&nl=1",sep=""),socket,sep="\n")
    res <- readLines( socket , n=2 )
    ty <- substring(noquote(parser.socket(res[2]))[2],4,nchar(noquote(parser.socket(res[2]))[2])-1)
    p <- getLocationSocket(socket,x)
    lapply(p,function(x){rect(x[1],0,x[2],1,col="red", border="red", lend = "butt" )})
    plot(c(0,l),c(0,10),type="n",axes=FALSE,ann=FALSE)
    title("Legend",font.main=4)
    legend(9,legend=ty,fill="red",bg="cornsilk",ncol = 1)
    names(p) <- x
    return(p)
  } else{
    if(verbose) cat("x n'est pas une sous-sequence\n")
    q <- paste("fi n=", x, sep = "")
    query(listname = "filles", query = q, socket = socket)
    if(length(get("filles", .GlobalEnv)$req)==1 && get("filles", .GlobalEnv)$req == x){ 
      rect(0,1,getLength(x),2,col= "red",border="red", lend = "butt")
      legend(9,legend=x,fill="red",bg="cornsilk",ncol = 1)
      return(list(x))
    }
    cou <- 0
    rap <- numeric()
    nb <- numeric()
    posi <- list()
    
    for(i in seq_len(length(ptype))){
      cou <- cou + 1
      q <- paste("filles et t=", ptype[i], sep = "")
      if(verbose) cat("query = ", q, "\n")

      query(socket = socket, listname = "tmp", query = q)
      if(get("tmp", .GlobalEnv)$nelem != 0){
        if(is.na(get("tmp", .GlobalEnv)$req[[1]]) || get("tmp", .GlobalEnv)$req[[1]] == x ){ 
          cou <- cou-1
        } else{  
          u <- lapply(get("tmp", .GlobalEnv)$req, getLocation)
          names(u) <- get("tmp", .GlobalEnv)$req
          lapply(u, function(x){ lapply(x,function(x){rect(x[1],0+cou,x[2],1+cou,col=cou, border=cou, lend = "butt" )})})
          rap[cou] <- i
          nb[cou] <- length(u)
          posi[[cou]] <- u
        }
      }
    }
    plot(c(0,l), c(0,10), type = "n", axes = FALSE, ann = FALSE)
    title("Legend",font.main=4)
    legend(9,legend=paste(ptype[rap],"(",nb,")",sep=""),fill= seq_len(cou),bg="cornsilk",ncol = 4)
    par(mfrow=c(1,1))
    resu = lapply(posi,function(x){lapply(x,unlist)})
    names(resu) = ptype[rap]
    return( resu )
  }
  #
  #  workspace cleanup
  #
  rm("me", pos = .GlobalEnv)
  rm("filles", pos = .GlobalEnv)
  rm("tmp", pos = .GlobalEnv)
}
