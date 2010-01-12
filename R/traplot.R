traplot <- function(mcmcout, parms=NULL, regex=NULL, random=NULL, ylim=NULL, auto.layout=TRUE, mar=c(2.0, 2.0, 1.5, 0.25) + 0.1, col=mcmcplotsPalette(nchains), lty=1, main=NULL, style=c("gray", "plain"), ...){
    style <- match.arg(style)
    if (!(is.mcmc(mcmcout)|is.mcmc.list(mcmcout))) mcmcout <- as.mcmc(mcmcout)
    if (!is.mcmc.list(mcmcout)) mcmcout <- mcmc.list(mcmcout)
    nchains <- length(mcmcout)
    parnames <- varnames(mcmcout)
    if (is.null(parnames)) stop("Argument 'mcmcout' must have valid variable names, chump!")
    parnames <- parms2plot(parnames, parms, regex, random)
    p <- length(parnames)
    if (auto.layout){
        op <- mult.fig(p, main=main, mar=mar)$old.par
        on.exit(par(op))
    }
    for (pmtr in parnames){
        traplot1(mcmcout[, pmtr, drop=FALSE], col=col, lty=lty, ylim=ylim, main=pmtr, xlab="", ylab="", style=style, ...)
    }
    return(invisible(parnames))
}
