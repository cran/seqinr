#
# Genetic code table as in Text Books
#

tablecode <- function(numcode = 1, urn.rna = c("u","c","a","g"), dia = TRUE)
{
  aa1 <- c("*", "A", "C", "D", "E", "F", "G", "H", "I", "K", "L", "M", "N",
           "P", "Q", "R", "S", "T", "V", "W", "Y")
  aa3 <- c("Stp", "Ala", "Cys", "Asp", "Glu", "Phe", "Gly", "His", "Ile",
           "Lys", "Leu", "Met", "Asn", "Pro", "Gln", "Arg", "Ser", "Thr",
           "Val", "Trp", "Tyr")
  if( dia )
  {  
    op <- par(no.readonly = TRUE)
    par(bg = "blue")
    par(fg = "yellow")
    par(col = "yellow")
    par(col.axis = "yellow")
    par(col.lab = "yellow")
    par(col.main = "yellow")
    par(col.sub = "yellow")
  }

  plot.new()
  plot.window(xlim=c(0,100),ylim=c(0,100))

  segments( 0, 102, 100, 102, lwd = 2)
  segments( 0, 0, 100, 0, lwd = 2)
  segments( 0, 97, 100, 97)

  codename <- SEQINR.UTIL$CODES.NCBI[numcode, "ORGANISMES"]
  codename <- as.character(codename)

  text(x=0, y = 98.5, font = 2, adj = c(0, 0),
    lab = paste("Genetic code", numcode,":",codename))

  urn <- c("t","c","a","g") # internal
  for( i in 0:3 )
  {
    for( j in 0:3 )
    {
      for( k in 0:3 )
      {
        codon <- c(urn[i+1], urn[j+1], urn[k+1])

        text( x = 100*j/4, y = 95 - 100*i/4 -5*k, adj = c(-0.5,1.5),
        lab = urn.rna[i+1] )

        text( x = 100*j/4 + 3, y = 95 - 100*i/4 -5*k, adj = c(-0.5,1.5),
        lab = urn.rna[j+1] )

        text( x = 100*j/4 + 6, y = 95 - 100*i/4 -5*k, adj = c(-0.5,1.5),
        lab = urn.rna[k+1] )

        aminoacid <- aa3[which(aa1 == translate(codon, numcode = numcode))]
        text( x = 100*j/4 + 12, y = 95 - 100*i/4 -5*k, adj = c(-0.5,1.5),
        lab =  aminoacid )
      }
    }
  }
  if(dia)
    par(op)
}
