trimSpace <- function(x, leading = TRUE, trailing = TRUE, space = "[:space:]"){
  if(leading){
    pattern <- paste("^[", space, "]*", sep = "", collapse = "")
    x <- sub(pattern = pattern, replacement =  "", x = x, extended = TRUE)
  }
  if(trailing){
    pattern <- paste("[", space, "]*$", sep = "", collapse = "")
    x <- sub(pattern = pattern, replacement =  "", x = x, extended = TRUE)
  }
  return(x)
}
