count <- function(seq, word, frame = 0, freq = FALSE, alphabet = s2c("acgt")){
  l <- (length(seq) - (word + frame)) + 1
  s <- seq[(frame+1):(frame+l)]
  seq <- seq[(frame+1):length(seq)]
  if (word == 1){
    counts <- table(factor(seq, levels = levels(as.factor(words(1, alphabet = alphabet)))))
  }
  else{
    for(i in 2:word) s <- paste(s, seq[i:(i + l - 1)], sep = "")
    counts <- table(factor(s, levels = levels(as.factor(words(word, alphabet = alphabet)))))
  }
  if (freq == FALSE){
    return(counts)
  }
  else{
    return((counts/(length(seq) - word + 1)))
  }
}
