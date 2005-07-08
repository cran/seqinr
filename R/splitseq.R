splitseq <- function(seq, frame = 0, word = 3){
#
# Compute all start positions of words to be returned:
#
  starts <- seq(from = frame + 1, to = length(seq), by = word)
  ends <- starts + word - 1
#
# Extract them all:
#
  res <- substring(c2s(seq), starts, ends)
#
# remove last one if not correct length:
#
  if(nchar(res[length(res)]) != word) res <- res[-length(res)]
  return(res)
}
