count<-function(seq,word,frame=0){
	l=(length(seq)-(word+frame))+1
	s=seq[(frame+1):(frame+l)]
	seq=seq[(frame+1):length(seq)]
	if (word==1){
	table(factor(seq,levels=levels(as.factor(words(1)))))
	}
	else{
	for(i in 2:word) s=paste(s,seq[i:(i+l-1)],sep="")
	table(factor(s,levels=levels(as.factor(words(word)))))
	}
    }



