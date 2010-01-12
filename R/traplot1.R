traplot1 <- function(x, col=mcmcplotsPalette(nchains), lty=1, style=c("gray", "plain"), ...){
    style <- match.arg(style)
    nchains <- nchain(x)
    xx <- as.vector(time(x))
    yy <- do.call("cbind", as.list(x))
    if (style=="plain"){
        matplot(xx, yy, type="l", col=col, lty=lty, ...)
    }
    if (style=="gray"){
        matplot(xx, yy, type="n",  xaxt="n", yaxt="n", bty="n", ...)
        .graypr()
        matlines(xx, yy, col=col, lty=lty)
    }
}
