plotabif <- function(abifdata, 
                     chanel = 1, 
                     tmin = 1/tscale, 
                     tmax = abifdata$Data[["SCAN.1"]]/tscale, 
                     tscale = 1000, 
                     yscale = 1000, type = "l", las = 1, 
                     xlab = paste("Time", tscale, sep = "/"),
                     ylab = paste("RFU", yscale, sep = "/"), 
                     irange = (tmin*tscale):(tmax*tscale),
                     x = irange/tscale,
                     xlim = c(tmin, tmax),
                     chanel.names = c(1:4,105),
                     DATA = paste("DATA", chanel.names[chanel], sep = "."),
                     y = abifdata$Data[[DATA]][irange]/yscale,
                     ylim = c(min(y), max(y)),
                     dyn = abifdata$Data[[paste("DyeN", chanel, sep = ".")]],
                     main = paste(deparse(substitute(abifdata)), chanel, dyn, sep = " ; "),
                     calibr = NULL,
                     ladder.bp = NULL,
                     allele.names = "identifiler",
                     ladder.lab = TRUE,
                     ...){
    
    old.par <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(old.par))
    if(is.null(calibr)){
        graphics::plot(x, y, type = type, las = las, 
             xlab = xlab, ylab = ylab, xlim = xlim, ylim = ylim, main = main, ...)
    } else {
        x <- calibr(irange)
        xlim <- range(x)
        graphics::plot(x, y, type = type, las = las, 
             xlab = "Size [bp]", ylab = ylab, xlim = xlim, ylim = ylim, main = main, ...)
        tps <- pretty(irange)
        graphics::par(cex=0.5)
        graphics::axis(1, at = calibr(tps), tps/tscale, line = 0.4, col = grDevices::grey(0.5))
        graphics::par(cex=1)
        if(!is.null(ladder.bp)){ # Allelic ladder add
            utils::data(list = allele.names,envir=environment())
            tmp <- get(allele.names)[chanel]
            n <- length(ladder.bp)
            labels <- unlist(tmp)
            col <-  rep("black", n)
            col[grep("\\.", labels)] <- "red"
            graphics::abline(v = ladder.bp, col = col)
            if(ladder.lab){
                graphics::text(ladder.bp, y = graphics::par("usr")[4], labels, xpd = NA, 
                     pos = 3, srt = 45, col = col, cex = 0.8)
            }
        }
    }
    locpar <- graphics::par(no.readonly = TRUE)
    invisible(locpar)
}
