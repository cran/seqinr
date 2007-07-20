#
# Read files of aligned sequences in various formats
#
read.alignment <- function(file, format, File = NULL)
{
  if(!is.null(File)) file <- File
  ali <- switch( format,
	fasta = .Call("read_fasta_align", file, PACKAGE = "seqinr"), 
	mase = .Call("read_mase", file, PACKAGE = "seqinr"),
	phylip = .Call("read_phylip_align", file, PACKAGE = "seqinr"),
	msf = .Call("read_msf_align", file, PACKAGE = "seqinr"),
	clustal = .Call("read_clustal_align", file, PACKAGE = "seqinr")
  )
  ali <- lapply(ali, as.character)
  if(format == "mase"){
    ali <- list(nb = as.numeric(ali[[1]]), nam = ali[[2]], seq = ali[[3]], com = ali[[4]]) 
  } else {
    ali <- list(nb = as.numeric(ali[[1]]), nam = ali[[2]], seq = ali[[3]], com = NA)
  }
  class(ali) <- "alignment"
  return(ali)
}
