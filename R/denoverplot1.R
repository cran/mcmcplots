denoverplot1 <- function(..., col=mcmcplotsPalette(n), lty=1, style=c("gray", "plain"), gpar=NULL){
    dat <- list(...)
    style <- match.arg(style)
    if (length(dat)==1 && is.list(dat[[1]])) dat <- dat[[1]]
    n <- length(dat)
    if (n==1 && is.list(dat[[1]])) {
        dat <- dat[[1]]
        n <- length(dat)
    }
    col <- rep(col, length=n)
    lty <- rep(lty, length=n)
    denout <- lapply(dat, density)
    xx <- sapply(denout, function(den) den$x)
    yy <- sapply(denout, function(den) den$y)
    if (style=="plain")
        do.call("matplot", c(list(x=xx, y=yy, col=col, lty=lty, type="l"), gpar))
    if (style=="gray"){
        do.call("matplot", c(list(x=xx, y=yy, type="n", bty="n", xaxt="n", yaxt="n"), gpar))
        .graypr()
        matlines(xx, yy, col=col, lty=lty)
    }
}
