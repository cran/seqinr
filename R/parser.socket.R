###################################################################################################
#                                                                                                 #
#                                         parser.socket                                           #
#                                                                                                 #
#                      Utility function to parse answers from ACNUC server.                       #
#                                                                                                 #
###################################################################################################

parser.socket <- function(onelinefromserver)
{
  if(is.null(onelinefromserver)){
    return(NULL)
  }
  #
  # Answers from server looks like : "code=0&lrank=2&count=150513&type=SQ&locus=F"
  # 
  loc <- gregexpr("=[^=&]*", onelinefromserver)[[1]]
  substring(onelinefromserver, loc + 1, loc + attr(loc, "match.length") - 1)
}
