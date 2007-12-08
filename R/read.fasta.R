read.fasta <- function(file = system.file("sequences/ct.fasta", package = "seqinr"), 
  seqtype = "DNA", File = NULL, as.string = FALSE, forceDNAtolower = TRUE,
  set.attributes = TRUE, legacy.mode = TRUE, seqonly = FALSE, strip.desc = FALSE)
{
  #
  # Check arguments:
  #
  if(!is.null(File)){
    file <- File
    warning("File is deprecated, use file instead")
  }
  #
  # Read the fasta file as a vector of strings:
  #
  lines <- readLines(file)
  #
  # Remove comment lines starting with a semicolon ';'
  #
  if(legacy.mode){
    comments <- grep("^;", lines)
    if(length(comments) > 0) lines <- lines[-comments]
  }
  #
  # Get the line numbers where sequences names are:
  #
  ind <- which(substr(lines, 1L, 1L) == ">")
  #
  # Compute the total number of sequences:
  #
  nseq <- length(ind)
  if(nseq == 0){
    stop("no line starting with a > character found")
  }
  #
  # Localize sequence data:
  #
  start <- ind + 1
  end <- ind - 1
  end <- c(end[-1], length(lines))
  #
  # Read sequences:
  #
  sequences <- lapply(seq_len(nseq), function(i) paste(lines[start[i]:end[i]], collapse = ""))
  if(seqonly) return(sequences)
  #
  # Read sequence names:
  #
  nomseq <- lapply(seq_len(nseq), function(i){
    firstword <- strsplit(lines[ind[i]], " ")[[1]][1]
    substr(firstword, 2, nchar(firstword))
  })
  #
  # Turn DNA sequences in lower case letters if required:
  #
  if(seqtype == "DNA"){
    if(forceDNAtolower){
      sequences <- as.list(tolower(sequences))
    }
  }
  #
  # Turn it into a vector of single chars if required:
  #
  if(as.string == FALSE) sequences <- lapply(sequences, s2c)
  #
  # Set sequence attributes when required:
  #
  if(set.attributes){
    for(i in seq_len(nseq)){
      Annot <- lines[ind[i]]
      if(strip.desc) Annot <- substr(Annot, 2L, nchar(Annot))
      attributes(sequences[[i]]) <- list(name = nomseq[[i]], 
        Annot = Annot,
        class = switch(seqtype, "AA" = "SeqFastaAA", "DNA" = "SeqFastadna"))
    }
  }
  #
  # Give the sequences names to the list elements:
  #
  names(sequences) <- nomseq
  return(sequences)
}


