uco <- function (seq, frame = 0, index = c("eff", "freq", "rscu"), 
as.data.frame = FALSE, NA.rscu = NA) 
{
    choice <- match.arg(index)
    
    if(any(seq%in%LETTERS)){
      seq <- tolower(seq)
    }
    sequence <- splitseq(seq = seq, frame = frame, word = 3)

    if( as.data.frame == FALSE ) {
      eff <- table(factor(sequence, levels = SEQINR.UTIL$CODON.AA$CODON))
      if(choice == "eff") return(eff)
      
      freq <- eff/(floor(length(seq)/3))
      if(choice == "freq") return(freq)
      
      T <- split(freq, SEQINR.UTIL$CODON.AA$AA)
      rscu <- lapply(T, function(x) {
        return(x/((1/length(x)) * sum(x)))
      })
      names(rscu) <- NULL
      rscu <- unlist(rscu)[as.character(SEQINR.UTIL$CODON.AA$CODON)]
      is.na(rscu[!is.finite(rscu)]) <- TRUE
      rscu[is.na(rscu)] <- NA.rscu
      return(rscu)
    } else { # return all indices in a data.frame
      eff <- table(factor(sequence, levels = SEQINR.UTIL$CODON.AA$CODON))
      freq <- eff/(floor(length(seq)/3))
      T <- split(freq, SEQINR.UTIL$CODON.AA$AA)
      rscu <- lapply(T, function(x) {
        return(x/((1/length(x)) * sum(x)))
      })
      names(rscu) <- NULL
      rscu <- unlist(rscu)[as.character(SEQINR.UTIL$CODON.AA$CODON)]
      is.na(rscu[!is.finite(rscu)]) <- TRUE
      rscu[is.na(rscu)] <- NA.rscu
      df <- data.frame(SEQINR.UTIL$CODON.AA$AA, eff = eff, freq = as.vector(freq), RSCU = rscu)
      names(df) = c("AA", "codon", "eff", "freq", "RSCU")
      return(df)
    }
}

