translate <- function(seq, frame=0, sens="F", numcode=1)
{
	if(sens=="R") seq<-comp(invers(seq))
	else if(sens=="F") seq <- seq

	n <- c(a=2,c=1,g=3,t=0)
    	seq <- as.vector(n[seq])
	seq <- as.numeric(seq) 
	l <- floor((length(seq)-frame)/3)*3
	a <- seq(frame+1,l+frame,3)
	b <- seq(frame+2,l+frame,3)
	c <- seq(frame+3,l+frame,3)
	tra <- 16*seq[a]+4*seq[b]+seq[c]+1
	code <- unlist(strsplit(as.vector(SEQINR.UTIL$CODES.NCBI$CODES[numcode]),""))	
	code[tra]
}




