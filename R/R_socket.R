###################################################################################################
# Ces fonctions permettent d'interroger les banques structurées sous ACNUC au travers des sockets.#
###################################################################################################



choosebank = function(bank = NA ,host = "pbil.univ-lyon1.fr", port = 5558){

	# ouverture d'un "client socket" sur le serveur pbil et sur le port 5558
	socket = socketConnection( host = host, port = port, server = F, blocking=T)
	rep1 = readLines(socket, n=1)

	# Si pas de banques spécifiées: liste des banques
	if(is.na(bank)){
	writeLines("knowndbs",socket, sep = "\n")
	rep = readLines(socket, n=1)
	res = readLines(socket, n=as.numeric(parser.socket(rep)))
	res = sapply(res,function(x){
		 pos=grep(" ",s2c(x))
		 substr(x,1,(pos[1]-1))
		})
	return(as.vector(res))
	}

	# Ouverture de la banque
	request = paste("acnucopen&db=",bank,sep="") 
	writeLines( request, socket, sep = "\n")
	rep2 = readLines(socket, n=1) 
        res = parser.socket(rep2)

	# vérification du bon fonctionnement de la connection et ouverture de la banque
	if(res[1] != "0"){
		print("bank name incorrect")
		rm(socket)
	}
	else{ 
		assign("banknameSocket",bank,.GlobalEnv)
		return(list(socket=socket, bankname = bank, totseqs = res[3], totspecs=res[4], totkeys=res[5]))
	}
}	
	

parser.socket = function(p)
{
	p1=s2c(p)
	b=grep("=",p1)
  	a=c(grep("&",p1),length(p1)+1)
	return(unlist(lapply(1:length(a),function(x){substr(p,(b[x]+1),(a[x]-1))})))
}
	

getSequenceSocket = function( socket, name, start, length){
	
	request2 = paste("gfrag&name=", name,"&start=", start, "&length=", length, sep= "")
	writeLines( request2, socket, sep="\n" )
	s = readLines(socket,n=1)

	if(length(s)==0){
		 print("invalid sequence name")
		}
	else{		
		s = s2c(s)
		sequence = s[(grep("&",s)+1):length(s)]
		return(sequence)
	}
}
	


getAttributsocket = function( socket, name){
	
	# Récupération des attributs d'une séquence
	
	request = paste( "isenum&name=",name, sep="")
	writeLines( request, socket, sep="\n")
	res = readLines( socket, n=1 )
	p=parser.socket(res)
	l=list( length = as.numeric(p[2]),frame = as.numeric(p[3]), gencode =as.numeric(p[4]) )
	return(l)
}


readAnnots.socket = function( socket, name, nl){

	  request= paste( "read_annots&name=", name, "&nl=", nl, sep= "")
	   writeLines( request , socket, sep="\n")
	   readLines( socket , n=nl )
}




getNumber.socket = function( socket, name){
	
	request = paste( "isenum&name=",name, sep="")
	writeLines( request, socket, sep="\n")
	s = readLines(socket,n=1)
	return(parser.socket(s)[1])
}

query = function (socket, listname, query, invisible = FALSE) 
{
    writeLines("prep_requete", socket, sep = "\n")
    readLines(socket, n = 1)
    request = paste("proc_requete&query=\"", query, "\"&name=\"", 
        listname, "\"", sep = "")
    writeLines(request, socket, sep = "\n")
    res = readLines(socket, n = 1)
    p = parser.socket(res)
    if (as.numeric(p[1]) != 0) 
        stop(paste("invalid request:", p[2], sep = ""))
    lrank = p[2]
    first = 1
    liste = character(as.numeric(p[3]))
    for (i in 1:length(liste)) {
        writeLines(paste("nexteltinlist&lrank=", lrank, "&first=", 
            first, "&type=SQ", sep = ""), socket, sep = "\n")
        res = readLines(socket, n = 1)
        r = parser.socket(res)
        first = r[1]
        liste[i] = r[2]
    }
    liste = lapply(liste, function(x){substring(x,2,nchar(x)-1)})
    liste = lapply(liste, as.SeqAcnucWeb, socket)	
    result = list(call = match.call(), name = listname, req = as.list(liste), 
        socket = socket)
    class(result) = c("qaw")
    assign(listname, result, env = .GlobalEnv)
    if(invisible == TRUE) invisible(result)
    else print(result)
}


print.qaw <- function(x, ...)
{

	cat("\n")
	cat("\n$socket: ")
	print(x$socket)
	cat("\n$banque: ")
	cat(get("banknameSocket",env=.GlobalEnv))
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



getKeywordsocket <- function( socket, name){

         writeLines(paste("isenum&name=",name,sep=""),socket,sep="\n")
         res = readLines( socket , n=1 )
         number = parser.socket(res)[1] 

         writeLines(paste("readsub&num=",number,sep=""),socket,sep="\n")
         res2 = readLines( socket , n=1 ) 
         rr = parser.socket(res2)
         
         writeLines(paste("readshrt&num=",rr[7],sep=""),socket,sep="\n")
         res3 = readLines( socket , n=1 ) 
         
	 p1=s2c(res3)
	 b=c(which(p1=="="))
  	 a=c(which(p1=="&"))
         d=c(which(p1==","),length(p1)+1)
	 o=character(length(a))
         o[1]=substr(res3,a[2]+1,d[1]-1)
	 s = seq(2,length(a)*2,by=2)
         for(i in 1:(length(s)-1)){o[i+1] = substr(res3,d[s[i]]+1,d[s[i]+1]-1)} 

	 lapply(o,function(x){
          	writeLines(paste("readkey&num=",x,sep=""),socket,sep="\n")	
	        res4 = readLines( socket , n=1 ) 
                parser.socket(res4)[2]
})

} 


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



getType = function(socket){
	writeLines( "readfirstrec&type=SMJ",socket, sep="\n" ) 
	s = readLines(socket,n=1)
	rep = parser.socket(s)
	if(rep[1]!="0") stop("erreur")
	rep = as.numeric(rep)
	writeLines( paste("readsmj&num=",10,"&nl=",rep[2]-10,sep=""), socket, sep="\n" ) 
	ss = readLines(socket,n=rep[2]-9)
	occ = grep("name=\"04",ss)
	h = ss[occ]
	return(lapply(h,function(x){ c(substring(noquote(parser.socket(x))[2],4,nchar(noquote(parser.socket(x))[2])-1),substring(noquote(parser.socket(x))[4],2,nchar(noquote(parser.socket(x))[4])-1)) }))
}





plot.SeqAcnucWeb = function(x,  type = "all", ...){


  # Vérification des arguments

  if(! inherits(x,c("SeqAcnucWeb"))) stop("Sequence of class SeqAcnucWeb is needed")
  socket = attr(x,"socket")

  types = unlist(lapply(getType(socket),function(x) x[1]))
  
  if(type == "all") ptype = types
  else{
    if(sum(type %in% types) != length(type)) stop("Please check the type argument !")
    else(ptype = type)
  }

  # Récupération de la séquence mère et organistion du plot

  par(mfrow=c(2,1))
  
  q = paste("me n=",x,sep="")
  
  query(socket,listname= "me",query = q,invisible = TRUE)
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
    
    for(i in 1:length(ptype)){
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
    legend(9,legend=paste(ptype[rap],"(",nb,")",sep=""),fill=c(1:cou),bg="cornsilk",ncol = 4)
    par(mfrow=c(1,1))
    resu = lapply(posi,function(x){lapply(x,unlist)})
    names(resu) = ptype[rap]
    return( resu )
  }
}

         





