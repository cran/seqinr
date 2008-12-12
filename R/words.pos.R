words.pos <- function(pattern, text, ignore.case = FALSE, extended = TRUE,
                      perl = TRUE, fixed = FALSE, useBytes = TRUE, ...)
{
  position <- regexpr(pattern, text, ignore.case, extended, perl, fixed, useBytes, ...)[1]
  result <- numeric(0)
  while(position != -1 )
  {
    result <- c(result, position )
    text <- substr(text, position + 1, nchar(text))
    position <- regexpr(pattern, text, ignore.case, extended, perl, fixed, useBytes, ...)[1]
  }
  return(cumsum(result))
}

