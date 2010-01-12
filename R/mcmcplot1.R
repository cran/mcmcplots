mcmcplot1 <- function(x, col=mcmcplotsPalette(n), lty=1, style=c("gray", "plain")){
    style <- match.arg(style)
    n <- length(x)
    parname <- varnames(x)
    layout(matrix(c(1, 1, 2, 3), 2, 2, byrow=TRUE))
    traplot1(x, col=col, lty=lty, style=style, ylab=parname, xlab="Iteration")
    title(paste("Diagnostic Plots for ", parname, sep=""))
    denoverplot1(x, col=col, lty=lty, style=style, gpar=list(ylab="Density", xlab=parname))
    autplot1(x, style=style)
}
