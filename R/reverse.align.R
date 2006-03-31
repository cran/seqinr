reverse.align <- function(nucl.file,
                          protaln.file,
                          input.format = "fasta",
                          out.file,
                          output.format = "fasta",
                          align.prot = FALSE,
                          numcode = 1,
                          clustal.path = NULL){

  seq.nucl <- read.fasta(nucl.file)



  
  if(align.prot==FALSE){  ## the protein alignment file is provided 
    protaln <- read.alignment(protaln.file,format=input.format)
  }
  
  else{ ## protein alignment file not provided, have to align with clustal 
    tmp <- tempfile(pattern="clustal")
    protseq.file <- tempfile(pattern="protein")
    write.fasta(sequences=lapply(seq.nucl, function(x) translate(x, numcode=numcode)), names=names(seq.nucl), file.out=protseq.file) 
    system(paste(clustal.path," -outfile=",tmp ," -infile=",protseq.file,sep=""))
    protaln <- read.alignment(tmp,format="clustal")
    input.format="clustal"
  }
  
  ## check if the sequences are given in the same order
  ordername <- unlist(lapply(protaln$nam, function(x) which(names(seq.nucl)==x)))
  seq.nucl <- seq.nucl[ordername]

   
  index <- as.list(rep(0,length(seq.nucl)))
  names(index) <- protaln$nam
  cds.aln <- list()
  length(cds.aln) <- length(seq.nucl)
  names(cds.aln) <- protaln$nam

  if(input.format %in% c("fasta","clustal","phylip","mase")){
    gapchar <- "-"
  }
  if(input.format == "msf"){
    gapchar <- "."
  }
  
  for(k in seq_len(nchar(protaln$seq[1]))){
    allaln=TRUE
    for(j in seq_len(length(seq.nucl))){
      if(substr(protaln$seq[j],k,k)!=gapchar){
        index[[j]]=index[[j]]+1
      }
      else{
          allaln=FALSE
        }
      }
      if(allaln){
        for(j in seq_len(length(seq.nucl))){
          cds.aln[[j]]=c(cds.aln[[j]],seq.nucl[[j]][(3*index[[j]]-2):(3*index[[j]])] )
        }
      }
    }
    
 write.fasta(sequences=cds.aln,names=names(seq.nucl),file.out=out.file,open="w")
 return(NULL)
 
}

