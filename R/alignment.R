read.alignment = function( File, format)
{

	ali = switch( format,
	fasta = .Call("read_fasta_align",File,PACKAGE="seqinr"), 
	mase = .Call("read_mase",File,PACKAGE="seqinr"),
	phylip = .Call("read_phylip_align",File,PACKAGE="seqinr"),
	msf = .Call("read_msf_align",File,PACKAGE="seqinr"),
	clustal = .Call("read_clustal_align",File,PACKAGE="seqinr"))
	ali = lapply(ali, as.character)
	if(format == "mase") ali = list(nb = as.numeric(ali[[1]]), nam = ali[[2]], seq = ali[[3]], com =ali[[4]]) 
	else ali = list(nb = as.numeric(ali[[1]]), nam = ali[[2]], seq = ali[[3]], com = NA)
	class(ali)="alignment"
	return(ali)
}


dist.alignment = function( x , matrix = "similarity" )
{
	if (!inherits(x, "alignment")) 
        stop("Object of class 'alignment' expected")
	t1 = c("similarity","identity")
	m1 = grep(matrix,t1)
	m2 = as.numeric(.Call("is_a_protein_seq",x$seq[1],PACKAGE="seqinr") >= 0.8)
	l = x$nb
	maxseq = length(x$seq)
	dist = .Call("distance",x$seq,l,m1,m2,PACKAGE="seqinr")
	mat = matrix(dist,l,l, byrow =TRUE)
        dimnames(mat) = list(x$nam,x$nam)
	return( as.dist(mat) )
}
