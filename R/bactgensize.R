dia.bactgensize <- function(
  fit = 2, p = 0.5, m1 = 2000, sd1 = 600, m2 = 4500, sd2 = 1000, p3 = 0.05,
  m3 = 9000, sd3 = 1000)
{

  bactURL <- "http://ergo.integratedgenomics.com/GOLD/search.cgi?orgcol=org&sizecol=size&org_name1=genus&gensp=&org_domain=BACTERIAL&org_status=ALL&size2=ALL&org_size=Kb&phylogeny2=ALL&gen_institution=ALL&gen_funding=ALL&gen_data=ALL&cont=ALL&selection=submit+search"
  archURL <- "http://ergo.integratedgenomics.com/GOLD/search.cgi?orgcol=org&sizecol=size&org_name1=genus&gensp=&org_domain=ARCHAEAL&org_status=ALL&size2=ALL&org_size=Kb&phylogeny2=ALL&gen_institution=ALL&gen_funding=ALL&gen_data=ALL&cont=ALL&selection=submit+search"
  bactdata <- readLines(bactURL)
  archdata <- readLines(archURL)

#
# Heuristic: data are at the line of maximum length
#

  bactdata <- bactdata[which(max(nchar(bactdata))==nchar(bactdata))]
  archdata <- archdata[which(max(nchar(archdata))==nchar(archdata))]

#
# Split by Table lines:
#

  bactdata <- unlist(strsplit(bactdata,split="<tr>"))
  archdata <- unlist(strsplit(archdata,split="<tr>"))

#
# Keep only lines where genome size is documented:
#

  bactdata <- bactdata[grep(" Kb", bactdata)]
  archdata <- archdata[grep(" Kb", archdata)]

#
# Heuristic: species names are in italic
#

  archnames <- unlist(strsplit(archdata,split="<i>"))
  archnames <- archnames[grep("</i>", archnames)]
  archnames <- strsplit(archnames, split="</i>")
  archnames <- unlist(lapply(archnames,"[",1)) #Keep the first element
  archnames <- strsplit(archnames,split="<br>")
  archgenus <- unlist(lapply(archnames,"[",1))
  archspecies <- unlist(lapply(archnames,"[",2))
  if( length(archspecies) != length(archgenus) ) stop("length(archspecies) != length(archgenus)")


  bactnames <- unlist(strsplit(bactdata,split="<i>"))
  bactnames <- bactnames[grep("</i>", bactnames)]
  bactnames <- strsplit(bactnames, split="</i>")
  bactnames <- unlist(lapply(bactnames,"[",1)) #Keep the first element
  bactnames <- strsplit(bactnames,split="<br>")
  bactgenus <- unlist(lapply(bactnames,"[",1))
  bactspecies <- unlist(lapply(bactnames,"[",2))
  if( length(bactspecies) != length(bactgenus) ) stop("llength(bactspecies) != length(bactgenus)")

#
# Get genome size
#

  archgs <- unlist(strsplit(archdata, split="<th>"))
  archgs <- archgs[grep(" Kb", archgs)]
  archgs <- strsplit(archgs, split=" ")
  archgs <- unlist(lapply(archgs, "[", 1))

  bactgs <- unlist(strsplit(bactdata, split="<th>"))
  bactgs <- bactgs[grep(" Kb", bactgs)]
  bactgs <- strsplit(bactgs, split=" ")
  bactgs <- unlist(lapply(bactgs, "[", 1))

#
# Check lengths consistency
#

  if( length(bactgs) != length(bactgenus) ) stop("length(bactgs) != length(bactgenus)")
  if( length(archgs) != length(archgenus) ) stop("length(archgs) != length(archgenus)")

#
# Build data frame
#

  genus <- data.frame(c(archgenus, bactgenus))
  species <- data.frame(c(archspecies, bactspecies))
  gs <- data.frame(as.numeric(c(archgs, bactgs)))

  tmp <- cbind(genus, species, gs)
  data <- data.frame(tmp)
  names(data) <- c("genus", "species", "gs")


  sizeKb <- data$gs
  n <- length(sizeKb)

#
# Graphics
#
  x <- seq( min(sizeKb), max(sizeKb), le=200)
  mybreaks <- seq(min(sizeKb),max(sizeKb),length=15)
  vscale <- diff(mybreaks)[1]*n

  if(fit == 0)
  {
    hst <- hist(sizeKb, freq = TRUE, 
      breaks = mybreaks,
      main=paste("Genome size distribution for", n, "bacterial genomes"),
      xlab="Genome size [Kb]",
      ylab="Genome count",
      col="lightgrey")

    dst <- density(sizeKb)
    lines(x=dst$x, y=vscale*dst$y)
    legend(x = max(sizeKb)/2, y = 0.75*max(hst$counts), lty=1,
      "Gaussian kernel density estimation")
      
    mtext(paste("Source of data: GOLD (Genomes OnLine Database)",date()))
  }
##########################################
#
# Fitting a mixture of two normal distributions
#
###########################################
  if(fit == 2)
  {
    logvraineg <- function(param, obs) 
    {
      p <- param[1]
      m1 <- param[2]
      sd1 <- param[3]
      m2 <- param[4]
      sd2 <- param[5]

      -sum(log(p*dnorm(obs,m1,sd1)+(1-p)*dnorm(obs,m2,sd2)))
    }

    attach(nlm(logvraineg, c(p, m1, sd1, m2, sd2), obs=sizeKb))

    y1 <- vscale*estimate[1]*dnorm(x, estimate[2], estimate[3])
    y2 <- vscale*(1-estimate[1])*dnorm(x, estimate[4], estimate[5])

    hst <- hist(sizeKb, freq = TRUE, plot = FALSE, breaks = mybreaks)    
    ymax <- max(y1, y2, hst$counts)
    
    
    hist(sizeKb, freq = TRUE, ylim=c(0,ymax),
      breaks = mybreaks,
      main=paste("Genome size distribution for", n, "bacterial genomes"),
      xlab="Genome size [Kb]",
      ylab="Genome count",
      col="lightgrey")
      
    lines(x, y1, col="red", lwd=2)
    lines(x, y2, col="blue", lwd=2)

    text(x = max(sizeKb)/2, y = ymax, pos=4, "Maximum likelihood estimates:")

    text(x = max(sizeKb)/2, y = 0.95*ymax, col="red", pos = 4, cex=1.2,
    substitute(hat(p)[1] == e1~~hat(mu)[1] == e2~~hat(sigma)[1] == e3, 
    list(e1 = round(estimate[1],3), 
       e2 = round(estimate[2],1),
       e3 = round(estimate[3],1))))

    text(x = max(sizeKb)/2, y = 0.90*ymax, col="blue", pos = 4, cex=1.2,
    substitute(hat(p)[2] == q~~hat(mu)[2] == e4~~hat(sigma)[2] == e5, 
    list(q = round(1 - estimate[1],3), 
       e4 = round(estimate[4],1),
       e5 = round(estimate[5],1))))
       
    dst <- density(sizeKb)
    lines(x=dst$x, y=vscale*dst$y)
    legend(x = max(sizeKb)/2, y = 0.75*ymax, lty=1,
       "Gaussian kernel density estimation")
       
    mtext(paste("Source of data: GOLD (Genomes OnLine Database)",date()))
  }
##########################################
#
# Fitting a mixture of three normal distributions
#
###########################################
  if(fit == 3)
  {
    logvraineg <- function(param, obs) 
    {
      p <- param[1]
      m1 <- param[2]
      sd1 <- param[3]
      m2 <- param[4]
      sd2 <- param[5]
      p3 <- param[6]
      m3 <- param[7]
      sd3 <- param[8]

      -sum(log(p*dnorm(obs,m1,sd1)
             +(1-p-p3)*dnorm(obs,m2,sd2)
             +p3*dnorm(obs,m3,sd3)))
    }

    attach(nlm(logvraineg, c(p, m1, sd1, m2, sd2, p3, m3, sd3), obs=sizeKb))

    y1 <- vscale*estimate[1]*dnorm(x, estimate[2], estimate[3])
    y2 <- vscale*(1-estimate[1]-estimate[6])*dnorm(x, estimate[4], estimate[5])
    y3 <- vscale*estimate[6]*dnorm(x, estimate[7], estimate[8])

    hst <- hist(sizeKb, freq = TRUE, plot = FALSE, breaks = mybreaks)    
    ymax <- max(y1, y2, y3, hst$counts)

    hist(sizeKb, freq = TRUE, ylim=c(0,ymax),
      breaks = mybreaks,
      main=paste("Genome size distribution for", n, "bacterial genomes"),
      xlab="Genome size [Kb]",
      ylab="Genome count",
      col="lightgrey")
          
    lines(x, y1, col="red", lwd=2)
    lines(x, y2, col="blue", lwd=2)
    lines(x, y3, col="green3", lwd=2)
  
    text(x = max(sizeKb)/2, y = ymax, pos=4, "Maximum likelihood estimates:")

    text(x = max(sizeKb)/2, y = 0.95*ymax, col="red", pos = 4, cex=1.2,
    substitute(hat(p)[1] == e1~~hat(mu)[1] == e2~~hat(sigma)[1] == e3, 
    list(e1 = round(estimate[1],3), 
       e2 = round(estimate[2],1),
       e3 = round(estimate[3],1))))

    text(x = max(sizeKb)/2, y = 0.90*ymax, col="blue", pos = 4, cex=1.2,
    substitute(hat(p)[2] == q~~hat(mu)[2] == e4~~hat(sigma)[2] == e5, 
    list(q = round(1 - estimate[1]-estimate[6],3), 
       e4 = round(estimate[4],1),
       e5 = round(estimate[5],1))))
       
    text(x = max(sizeKb)/2, y = 0.85*ymax, col="green3", pos = 4, cex=1.2,
    substitute(hat(p)[3] == p3~~hat(mu)[3] == e7~~hat(sigma)[3] == e8, 
    list(p3 = round(estimate[6],3), 
       e7 = round(estimate[7],1),
       e8 = round(estimate[8],1))))     
       
    dst <- density(sizeKb)
    lines(x=dst$x, y=vscale*dst$y)
    legend(x = max(sizeKb)/2, y = 0.75*ymax, lty=1,
       "Gaussian kernel density estimation")
       
    mtext(paste("Source of data: GOLD (Genomes OnLine Database)",date()))
  }
#
# Return invisibly the dataset
#
  invisible(data)
}












