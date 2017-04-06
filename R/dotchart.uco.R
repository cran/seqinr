dotchart.uco <- function(x, numcode = 1, aa3 = TRUE, pt.cex = 0.7, 
  alphabet = s2c("tcag"), pch = 21, gpch = 20, bg = par("bg"), cex = 0.7,
  color = "black", gcolor = "black", lcolor = grey(0.9), xlim, ...)
{
  if( is.null(names(x)) ) names(x) <- words( alphabet = alphabet )
  bcknames <- names(x)
  x <- as.numeric(x)
  names(x) <- bcknames
#
# General sorting 
#
  x <- sort(x)
  labels <- names(x)
  stringlabel = paste(labels, sep = "", collapse = "")
  groups <- as.factor(translate(s2c(stringlabel), numcode =  numcode))
  gdata <- sapply(split(x, groups), sum)
#
# Now, sorting by aa order
#
  gordered <- rank(gdata)
  xidx <- numeric(64)

  for( i in seq_len(64) )
  {
    xidx[i] <- -0.01*i + gordered[groups[i]]
  }

  x <- x[order(xidx)]
  labels <- names(x)
  stringlabel = paste(labels, sep = "", collapse = "")
  aa <- translate(s2c(stringlabel), numcode =  numcode)
  groups <- factor(aa, levels = unique(aa))
  gdata <- sapply(split(x, groups), sum)

  if( missing(xlim) ) xlim <- c(0, max(gdata))
  if( aa3 ) levels(groups) <- aaa(levels(groups))

  dotchart(x = x, labels = labels, groups = groups, gdata = gdata,
   pt.cex = pt.cex, pch = pch, gpch = gpch, bg = bg, color = color,
   gcolor = gcolor, lcolor = lcolor, cex = cex, xlim, ...)
#
# Return invisibly for further plots
#
  result <- list(0)
  result$x <- x
  result$labels <- labels
  result$groups <- groups
  result$gdata <- gdata

  ypg <- numeric( length(levels(groups)) )
  i <- 1
  for( aa in levels(groups) )
  {
    ypg[i] <- length(which(groups == aa)) + 2
    i <- i + 1
  }
  ypg <- rev(cumsum(rev(ypg))) - 1
  names(ypg) <- levels(groups)
  result$ypg <- ypg

  ypi <- numeric( length(x) )
  for( i in seq_len(length(x)) )
  {
    ypi[i] <- ypg[groups[i]]
  }
  antirank <- function(x) 
  {
    return( seq(length(x),1,by=-1 ))
  }
  ypi <- ypi - unlist(sapply(split(x, groups),antirank))
  names(ypi) <- labels
  result$ypi <- ypi

  return( invisible(result) ) 
}
