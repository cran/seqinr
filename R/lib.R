.First.lib <- function(lib, pkg)
{
  library.dynam("seqinr", pkg, lib)
  file1 <- file.path(lib, pkg, "data", "SEQINR.UTIL.RData")
  file2 <- file.path(lib, pkg, "data", "EXP.RData")
 
  load (file1, envir = as.environment(match("package:seqinr", search())))
  load (file2, envir = as.environment(match("package:seqinr", search())))
}

.Last.lib <- function(libpath=.path.package(package = "seqinr"))
{
  if(exists("SEQINR.UTIL", envir = globalenv())){
    rm(SEQINR.UTIL, envir = globalenv())
  }
  if(exists("EXP", envir = globalenv())){
    rm(EXP, envir = globalenv())
  }
}
