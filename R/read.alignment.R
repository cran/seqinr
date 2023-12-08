#
# Read files of aligned sequences in various formats
#
read.alignment <- function(file, format, forceToLower = TRUE, oldclustal = FALSE, ...)
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

    testReadClustalInR <- function(file, forceToLower = TRUE, ...){
      filecontent <- readLines(file, ...)
      firstline <- filecontent[1]
      if(substr(firstline, 1, 7) != "CLUSTAL"){
        stop("File not in CLUSTAL format!")
      }
      # skip firt line:
      filecontent <- filecontent[-1]

      # Remove empty lines:
      emptylines <- which(nchar(filecontent) == 0L)
      filecontent <- filecontent[-emptylines]

      # Remove consensus lines:
      conslines <- which(substr(filecontent, 1, 1) == " ")
      filecontent <- filecontent[-conslines]

      # Look for sequence start position: firt blank character followed
      # by a non-blank character
      characters <- s2c(filecontent[1])
      white <- characters[-length(characters)] == " "
      black <- characters[-1] != " "
      istart <- which(white & black)[1]

      # Get sequence names
      nam <- character()
      nseq <- 1
      while(TRUE){
        seqname <- substr(filecontent[nseq], 1, istart - 1)
        # remove trailling blanks
        seqname <- s2c(seqname)
        for(i in length(seqname):1){
          if(seqname[i] == " "){
            seqname <- seqname[-i]
          } else {
            break
          }
        }
        seqname <- c2s(seqname)
        if(seqname %in% nam){
          nseq <- nseq - 1
          break # alredy seen
        }
        nam[nseq] <- seqname
        nseq <- nseq + 1
      }

      # remove non sequences stuff
      for(i in 1:length(filecontent)){
        # Remove sequence name
        filecontent[i] <- substr(filecontent[i], istart + 1, nchar(filecontent[i]))
        # Look for sequence end position: first non-blank character followed
        # by a blank character after sequence start
        characters <- s2c(filecontent[i])
        black <- characters[-length(characters)] != " "
        white <- characters[-1] == " "
        iend <- which(black & white)[1]
        # Keep only sequence characters
        # (if the format present a number at the end of the line )
        if ('FALSE' %in% black) {
          filecontent[i] <- substr(filecontent[i], 1, iend)
        }
      }
      # Concatenation of sequence lines in a single string
      seq <- vector(mode = "list")
      for(iseq in seq_len(nseq)){
        ii <- seq(iseq, length(filecontent), by = nseq)
        seq[[iseq]] <- paste0(filecontent[ii], collapse = "")
        if(forceToLower) seq[[iseq]] <- tolower(seq[[iseq]])
      }
      #
      return(as.alignment(nb = nseq, nam = nam, seq = seq, com = NA))
    }


    ali <- switch( tolower(format),
                   fasta = fasta2ali(file, ...),
                   mase = .Call("read_mase", file, PACKAGE = "seqinr"),
                   phylip = .Call("read_phylip_align", file, PACKAGE = "seqinr"),
                   msf = .Call("read_msf_align", file, PACKAGE = "seqinr"),
                   clustal = {
                      if (oldclustal == TRUE) {
                        .Call("read_clustal_align", file, PACKAGE = "seqinr")
                      }
                      else {
                        testReadClustalInR(file, ...)
                      }
                   },
                   stop("Wrong format name: Format available are fasta, mase, phylip, msf, clustal")
    )

    if ((oldclustal == FALSE) && tolower(format) == "clustal") {
      return(ali)
    }
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
