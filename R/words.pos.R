words.pos <- function(pattern, text, extended=TRUE, perl=FALSE)
{
  position <- regexpr(pattern, text, extended, perl)[1]
  result <- numeric(0)
  while(position != -1 )
  {
    result <- c(result, position )
    text <- substr(text, position+1, nchar(text))
    position <- regexpr(pattern, text, extended, perl)[1]
  }
  return(cumsum(result))
}

