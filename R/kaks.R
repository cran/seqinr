kaks = function(x){
	if(attr(x,"class") != "alignment") error("object x must be of class alignment")
	if(x$nb <= 1) return(0)
	tmp<-x
	l = .Call("kaks",x$seq,x$nb,PACKAGE="seqinr")
	tmp$seq<-l[[5]]
	assign(as.character(as.list(match.call())$x),tmp, envir = globalenv())
	m = lapply(l[1:4],function(k){
	if(! is.null(x$nam)) tmp = matrix( k, x$nb, x$nb, byrow = TRUE, dimnames=list(x$nam,x$nam))
	else{
	 n = paste("seq",c(1:x$nb),sep="")
	 tmp = matrix( k, x$nb, x$nb, byrow = TRUE, dimnames=list(n,n))
	}
	 as.dist(t(tmp))
	})
	m = lapply(m,round,digits=6)
	names(m)=c("ka","ks","vka","vks")
	return(m)
}


