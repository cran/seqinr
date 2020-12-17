#
# Read files of aligned sequences in various formats
#
read.alignment <- function(file, format, forceToLower = TRUE, ...)
{
  #
  # Check if the file is an URL:
  #
  urlcheck = substr(file, 0,4)
  if (urlcheck == "http") {
    # Copy the contents to a tmpfile to be read later
    lines <- readLines(file)
    tmpaln <- tempfile(pattern = "readaln")
    writeLines(lines,tmpaln)
    file <-tmpaln
  }

  #
  # Check that we have read permission on the file:
  #
  file <- path.expand(file)
  if(file.access(file, mode = 4) != 0) stop(paste("File", file, "is not readable"))

  fasta2ali <- function(file, ...){
  	tmp <- read.fasta(file, as.string = TRUE, ...)
  	list(length(tmp), getName(tmp), unlist(getSequence(tmp, as.string = TRUE)))
  }
  ali <- switch( tolower(format),
	fasta = fasta2ali(file, ...),
	mase = .Call("read_mase", file, PACKAGE = "seqinr"),
	phylip = .Call("read_phylip_align", file, PACKAGE = "seqinr"),
	msf = .Call("read_msf_align", file, PACKAGE = "seqinr"),
	clustal = .Call("read_clustal_align", file, PACKAGE = "seqinr"),
	stop("Wrong format name: Format available are fasta, mase, phylip, msf, clustal")
  )

  ali <- lapply(ali, as.character)
  #cleaning \r char
  ali <- lapply(ali, function (x ){gsub ('\r','',x)})
  if(forceToLower) ali[[3]] <- lapply(ali[[3]], tolower)
  if(format == "mase"){
    ali <- list(nb = as.numeric(ali[[1]]), nam = ali[[2]], seq = ali[[3]], com = ali[[4]])
  } else {
    ali <- list(nb = as.numeric(ali[[1]]), nam = ali[[2]], seq = ali[[3]], com = NA)
  }
  class(ali) <- "alignment"
  return(ali)
}
