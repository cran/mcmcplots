mcmcplot1 <- function(x, col=mcmcplotsPalette(n), lty=1, style=c("gray", "plain")){
    ## Convert input mcmcout to mcmc.list object
    if (!(is.mcmc(x) | is.mcmc.list(x)))
        x <- as.mcmc(x)
    if (!is.mcmc.list(x))
        x <- mcmc.list(x)
    style <- match.arg(style)
    n <- length(x)
    parname <- varnames(x)
    ## layout(matrix(c(1, 1, 2, 3), 2, 2, byrow=TRUE))
    opar <- par(mar=c(5, 4, 2, 1) + 0.2, oma=c(0, 0, 2, 0) + 0.1)
    on.exit(par(opar))
    layout(matrix(c(1, 2, 1, 3, 4, 4), 3, 2, byrow=TRUE))
    denoverplot1(x, col=col, lty=lty, style=style, gpar=list(ylab="Density", xlab=parname))
    autplot1(x, style=style)
    autplot1(x, style=style, partial=TRUE)
    traplot1(x, col=col, lty=lty, style=style, ylab=parname, xlab="Iteration")
    title(paste("Diagnostic Plots for ", parname, sep=""), cex.main=1.5, outer=TRUE)
}
