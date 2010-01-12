caterplot <- function (mcmcout, parms=NULL, regex=NULL, random=NULL, quantiles=list(), collapse=TRUE, plot.labels="axis", cex.labels=NULL, xlim=NULL, lwd=c(1, 2), col=mcmcplotsPalette(nchains), style=c("gray", "plain"), ...){

    ## Utility functions ##
    is.odd <- function(x) return(x %% 2 != 0)

    get.offset <- function(val, tot, eps=0.1){
        if (is.odd(tot)){
            if (val==1)
                return(0)
            tot <- tot - 1
            val <- val - 1
        }
        out <- ifelse(is.odd(val), -ceiling(val/2)*eps/tot, ceiling(val/2)*eps/tot)
        return(out)
    }

    style <- match.arg(style)

    ## Convert to MCMC list object
    if (!(is.mcmc(mcmcout)|is.mcmc.list(mcmcout)))
        mcmcout <- as.mcmc(mcmcout)
    if (!is.mcmc.list(mcmcout))
        mcmcout <- mcmc.list(mcmcout)
    if (collapse)
        mcmcout <- as.mcmc.list(as.mcmc(as.matrix(mcmcout)))
    nchains <- length(mcmcout)

    parnames <- parms2plot(varnames(mcmcout), parms, regex, random)
    if (length(parnames)==0)
        stop("No parameters matched arguments 'parms' or 'regex'.")
    mcmcout <- mcmcout[, parnames, drop=FALSE]
    np <- length(parnames)

    ## Calculate points and lines to plot and medians for line plots
    q <- list(outer=c(0.025, 0.975), inner=c(0.16, 0.84))
    q[names(quantiles)] <- quantiles
    qout <- lapply(mcmcout, function(mat) apply(mat, 2, quantile, probs=q$outer))
    qin  <- lapply(mcmcout, function(mat) apply(mat, 2, quantile, probs=q$inner))
    mn   <- lapply(mcmcout, colMeans)
    med  <- lapply(mcmcout, function(mat) apply(mat, 2, median))

    if (is.null(xlim))
        x.lim <- range(unlist(qout))
    else
        x.lim <- xlim
    if(style=="gray"){
        plot(0, 0, ylim = c(0, np + 1), xlim=x.lim, type="n", ann=FALSE, xaxt="n", yaxt="n", bty="n", ...)
        .graypr(y.axis=FALSE, y.major=FALSE, x.minor=FALSE, y.minor=FALSE)
        abline(h=1:np, col=gray(0.95), lty=3)
    }
    if (style=="plain"){
        plot(0, 0, ylim = c(0, np + 1), xlim=x.lim, type="n", ann=FALSE, yaxt="n", ...)
    }
    yy <- rev(seq(np))
    lwd <- rep(lwd, length=2)
    for (i in seq(nchains)){
        yyoff <- yy + get.offset(i, nchains)
        matlines(qout[[i]], rbind(yyoff, yyoff), col=col[i], lwd=lwd[1], lty=1)
        matlines(qin[[i]], rbind(yyoff, yyoff), col=col[i], lwd=lwd[2], lty=1)
        points(med[[i]], yyoff, pch=19, col=col[i])
    }
    if (is.null(cex.labels))
        cex.labels <- 1/(log(np)/5 + 1)
    if (plot.labels=="axis")
        axis(2, at=yy, labels=parnames, tick=F, las=1, cex.axis=cex.labels)
    if (plot.labels=="above")
        text(med[[1]], yy, pos=3, labels=parnames, cex=cex.labels)

    return(invisible(parnames))
}

