read.fasta <- function(file = system.file("sequences/ct.fasta", package = "seqinr"), 
  seqtype = "DNA", File = NULL, as.string = FALSE, forceDNAtolower = TRUE,
  set.attributes = TRUE)
{
  #
  # Check arguments:
  #
  if(!is.null(File)) file <- File
  #
  # Read the fasta file as a vector of strings:
  #
  lines <- readLines(file)
  #
  # Get the line numbers where sequences names are:
  #
  ind <- grep(">", lines)
  #
  # Compute the total number of sequences:
  #
  nseq <- length(ind)
  #
  # Add an extra line so that the last sequence is not lost:
  #
  ind <- c(ind, length(lines) + 1)
  #
  # Make empty lists to store results:
  #
  nomseq <- vector(mode = "list", length = nseq)
  sequences <- vector(mode = "list", length = nseq)
  #
  # Main loop to read sequences:
  #  
  for(i in seq_len(nseq))
  {
    #
    # Sequence name ends with the first space character:
    #
    nomseq[[i]] <- unlist(strsplit(lines[ind[i]], " "))[1]
    #
    # Remove the first '>' character:
    #
    nomseq[[i]] <- substr(nomseq[[i]], 2, nchar(nomseq[[i]]))
    #
    # Read sequence:
    #
    sequences[[i]] <- paste(lines[(ind[i]+1):(ind[i+1]-1)], collapse = "")
    #
    # Turn it into a vector of single chars if required:
    #
    if( as.string == FALSE) sequences[[i]] <- s2c(sequences[[i]])
    #
    # Turn DNA sequences in lower case letters if required:
    #
    if(seqtype == "DNA"){
      if(forceDNAtolower){
        sequences[[i]] <- tolower( sequences[[i]])
      }
    }
    #
    # Set sequence attributes when required:
    #
    if(set.attributes){
      attributes(sequences[[i]]) <- list(name = nomseq[[i]], 
        Annot = (lines[ind[i]]), class = switch(seqtype, "AA" = "SeqFastaAA", "DNA" = "SeqFastadna"))
    }
  }
  #
  # Give the sequences names to the list elements:
  #
  names(sequences) <- nomseq
  return(sequences)
}


