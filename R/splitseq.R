splitseq <- function(seq,  frame=0, word=3){
 if(word==1) seq[frame:length(seq)] 
 l <- floor((length(seq)-frame)/word)*word
 seq <- seq[frame+1:(length(seq)-(length(seq)-l))]
 ff<-split(seq,gl(l/word, word))
 return(as.vector(unlist(lapply(ff,c2s))))
}
