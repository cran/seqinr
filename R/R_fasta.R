read.fasta <- function(File = system.file("sequences/ct.fasta", package = "seqinr"), seqtype="DNA")
{
  seq <- readLines(File) 
  ind <- c(grep(">",seq),length(seq)+1)
  nomseq <- as.list(c(rep(0,(length(ind)-1))))
  sequences <- as.list(rep(0,length(ind)-1))
  for(i in 1:(length(ind)-1))
  {
    nomseq[[i]]<-unlist(strsplit(seq[ind[i]]," "))[1]
    nomseq[[i]]=substr(nomseq[[i]],2,nchar(nomseq))	
    sequences[[i]]<-s2c(paste(seq[(ind[i]+1):(ind[i+1]-1)],collapse=""))
    if(seqtype == "DNA"){ sequences[[i]]=tolower( sequences[[i]])}
    attributes(sequences[[i]])=list(name=nomseq[[i]],Annot=(seq[ind[i]]),class = switch(seqtype,"AA"="SeqFastaAA", "DNA"= "SeqFastadna"))
  }
  names(sequences)=nomseq
  return(sequences)
}


