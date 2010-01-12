autplot1 <- function(x, chain=1, lag.max=NULL, partial=FALSE, col=mcmcplotsPalette(1), style=c("gray", "plain"), ...){
    style <- match.arg(style)
    if (partial){
        ylim <- NULL
        ylab <- "Partial Autocorrelation"
        xacf <-  pacf(as.ts.mcmc(x[[chain]]), lag.max = lag.max, plot = FALSE)
    } else {
        ylim <- c(-0.25, 1)
        ylab <- "Autocorrelation"
        xacf <-  acf(as.ts.mcmc(x[[chain]]), lag.max = lag.max, plot = FALSE)
    }
    for (j in 1:nvar(x)) {
        if (style=="gray"){
            plot(xacf$lag[, j, j], xacf$acf[, j, j], type = "n", ylab = ylab, xlab = "Lag", ylim = ylim, bty="n", xaxt="n", yaxt="n", ...)
            .graypr()
            lines(xacf$lag[, j, j], xacf$acf[, j, j], type="h", lwd=2, col=col)
        }
        if (style=="plain")
            plot(xacf$lag[, j, j], xacf$acf[, j, j], type = "h", ylab = ylab, xlab = "Lag", ylim = ylim, lwd=2, ...)
    }
}
