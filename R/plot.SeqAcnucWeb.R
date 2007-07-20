###################################################################################################
#
#                                         plot.SeqAcnucWeb
#
###################################################################################################

plot.SeqAcnucWeb <- function(x,  type = "all", ...){
  verbose <- FALSE # a passer en argument si besoin est
  #
  # Check arguments:
  #
  if(!inherits(x, "SeqAcnucWeb")) stop("Sequence of class SeqAcnucWeb is needed")
  
  socket <- attr(x, "socket")

  types <- unlist(lapply(getType(socket),function(x) x[1]))
  if(verbose) cat(paste("types:", types, sep = ""))
  
  if(type == "all"){
    ptype <- types
  } else {
    if(sum(type %in% types) != length(type)) stop("Please check the type argument !")
    ptype <- type
  }

  #
  # Get parent sequence and plot organization:
  #
  par(mfrow = c(2,1))
  
  q = paste("me n=", x, sep="")
  
  query(socket, listname= "me", query = q, invisible = TRUE)
  l = getLength(me$req[[1]])
  cx = c(0,l+(1/10)*l)
  cy = c(0,15)
  plot(cx,cy,ann=FALSE,type="n",axes=FALSE)
  axis(1, col.axis = "blue")
  title(main = paste("Physical position (base) of the subsequences","\n","on the parent sequence", me$req[[1]], sep=" "),font.main=3, col.main="blue")
  mtext(paste("(length of the parent sequence = ", l, ")", sep=""), cex = 0.7, font = 3)

  # Si x est une sous séquence alors dire le type

  if(me$req[[1]] != x){
  
  writeLines(paste("isenum&name=",x,sep=""),socket,sep="\n")
  res = readLines( socket , n=1 )
  writeLines(paste("readsub&num=",as.numeric(parser.socket(res)[1]),sep=""),socket,sep="\n")
  res=readLines( socket , n=1 )
  writeLines(paste("readsmj&num=",as.numeric(parser.socket(res)[4]),"&nl=1",sep=""),socket,sep="\n")
  res = readLines( socket , n=2 )
  ty = substring(noquote(parser.socket(res[2]))[2],4,nchar(noquote(parser.socket(res[2]))[2])-1)
  p = getLocationSocket(socket,x)
  kk=lapply(p,function(x){rect(x[1],0,x[2],1,col="red", border="red" )})
  plot(c(0,l),c(0,10),type="n",axes=FALSE,ann=FALSE)
  title("Legend",font.main=4)
  legend(9,legend=ty,fill="red",bg="cornsilk",ncol = 1)
  names(p)=x
  return(p)
}

  else{
    q = paste("fi n=",x,sep="")
    query(socket = socket, listname = "filles", query = q, invisible = TRUE)
    if(length(filles$req)==1 && filles$req == x){ 
      rect(0,1,getLength(x),2,col= "red",border="red")
      legend(9,legend=x,fill="red",bg="cornsilk",ncol = 1)
      return(list(x))
    }
    cou = 0
    rap=numeric()
    nb=numeric()
    posi = list()
    
    for(i in seq_len(length(ptype))){
      cou = cou+1
      q=paste("filles et t=",ptype[i],sep="")
      query(socket = socket, listname = "tmp", query = q, invisible = TRUE)
      if(is.na(tmp$req[[1]]) || tmp$req[[1]] == x ){ 
        cou = cou-1
      }
      else{  
      u = lapply(tmp$req,getLocation)
      names(u)=tmp$req
      lapply(u, function(x){ lapply(x,function(x){rect(x[1],0+cou,x[2],1+cou,col=cou, border=cou )})})
      rap[cou]=i
      nb[cou]=length(u)
      posi[[cou]]=u
    }
    }
    plot(c(0,l),c(0,10),type="n",axes=FALSE,ann=FALSE)
    title("Legend",font.main=4)
    legend(9,legend=paste(ptype[rap],"(",nb,")",sep=""),fill= seq_len(cou),bg="cornsilk",ncol = 4)
    par(mfrow=c(1,1))
    resu = lapply(posi,function(x){lapply(x,unlist)})
    names(resu) = ptype[rap]
    return( resu )
  }
  #
  # Manque netoyage environement parent
  #
}
