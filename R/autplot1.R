autplot1 <- function(x, chain=1, lag.max=NULL, col=mcmcplotsPalette(1), style=c("gray", "plain"), ...){
    style <- match.arg(style)
    xacf <-  acf(as.ts.mcmc(x[[chain]]), lag.max = lag.max, plot = FALSE)
    for (j in 1:nvar(x)) {
        if (style=="gray"){
            plot(xacf$lag[, j, j], xacf$acf[, j, j], type = "n", ylab = "Autocorrelation", xlab = "Lag", ylim = c(-1, 1), bty="n", xaxt="n", yaxt="n", ...)
            .graypr()
            lines(xacf$lag[, j, j], xacf$acf[, j, j], type="h", lwd=2)
        }
        if (style=="plain")
            plot(xacf$lag[, j, j], xacf$acf[, j, j], type = "h", ylab = "Autocorrelation", xlab = "Lag", ylim = c(-1, 1), lwd=2, ...)
    }
}
