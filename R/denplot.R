denplot <- function(mcmcout, parms=NULL, regex=NULL, random=NULL, xlim=NULL, ylim=NULL, auto.layout=TRUE, mar=c(2.0, 2.0, 1.5, 0.25)+0.1, col=mcmcplotsPalette(nchains), lty=1, xlab="", ylab="", main=NULL, collapse=FALSE, style=c("gray", "plain"), ...){
    gpar.args <- list(...)
    style <- match.arg(style)

    if (!(is.mcmc(mcmcout)|is.mcmc.list(mcmcout))) mcmcout <- as.mcmc(mcmcout)
    if (!is.mcmc.list(mcmcout)) mcmcout <- mcmc.list(mcmcout)
    if (collapse) mcmcout <- as.mcmc.list(as.mcmc(as.matrix(mcmcout)))
    nchains <- length(mcmcout)

    col <- rep(col, length=nchains)
    lty <- rep(lty, length=nchains)

    parnames <- varnames(mcmcout)
    if (is.null(parnames))
        stop("Argument 'mcmcout' must have valid variable names, chump!")
    parnames <- parms2plot(parnames, parms, regex, random)
    p <- length(parnames)

    if (auto.layout){
        op <- mult.fig(p, main=main, mar=mar)$old.par
        on.exit(par(op))
    }
    for (pmtr in parnames){
        gpar <- c(list(xlim=xlim, ylim=ylim, xlab=xlab, ylab=ylab, main=pmtr), gpar.args)
        denoverplot1(mcmcout[, pmtr, drop=FALSE], col=col, lty=lty, style=style, gpar=gpar)
    }
    return(invisible(parnames))
}
