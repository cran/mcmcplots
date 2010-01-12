caterplot <- function (mcmcout, parms=NULL, regex=NULL, random=NULL, quantiles=list(), collapse=TRUE, labels=NULL, labels.loc="axis", cex.labels=NULL, horizontal=TRUE, val.lim=NULL, lab.lim=NULL, lwd=c(1, 2), pch=19, col=mcmcplotsPalette(nchains), style=c("gray", "plain"), ...){

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

    if (is.null(val.lim))
        val.lim <- range(unlist(qout))
    if (is.null(lab.lim))
        lab.lim <- c(0, np + 1)

    if (horizontal){
        xlim <- val.lim
        ylim <- lab.lim

        y.axis <- FALSE
        y.major <- FALSE
        y.minor <- FALSE

        x.axis <- TRUE
        x.major <- TRUE
        x.minor <- FALSE

        xaxt <- NULL
        yaxt <- "n"

        axis.side <- 2
        las <- 1

        vv <- rev(seq(np))
    }
    else{
        ylim <- val.lim
        xlim <- lab.lim

        y.axis <- TRUE
        y.major <- TRUE
        y.minor <- FALSE

        x.axis <- FALSE
        x.major <- FALSE
        x.minor <- FALSE

        xaxt <- "n"
        yaxt <- NULL

        axis.side <- 1
        las <- 2

        vv <- seq(np)
    }
    if(style=="gray"){
        plot(0, 0, ylim = ylim, xlim=xlim, type="n", ann=FALSE, xaxt="n", yaxt="n", bty="n", ...)
        .graypr(x.axis=x.axis, x.major=x.major, x.minor=x.minor, y.axis=y.axis, y.major=y.major, y.minor=y.minor)
        if (horizontal) abline(h=1:np, col=gray(0.95), lty=3)
        else abline(v=1:np, col=gray(0.95), lty=3)
    }
    if (style=="plain"){
        plot(0, 0, ylim=ylim, xlim=xlim, type="n", ann=FALSE, yaxt=yaxt, xaxt=xaxt, ...)
    }
    lwd <- rep(lwd, length=2)
    if (horizontal){
        for (i in seq(nchains)){
            vvoff <- vv + get.offset(i, nchains)
            matlines(qout[[i]], rbind(vvoff, vvoff), col=col[i], lwd=lwd[1], lty=1)
            matlines(qin[[i]], rbind(vvoff, vvoff), col=col[i], lwd=lwd[2], lty=1)
            points(med[[i]], vvoff, pch=pch, col=col[i])
        }
    }
    else{
        for (i in seq(nchains)){
            vvoff <- vv + get.offset(i, nchains)
            matlines(rbind(vvoff, vvoff), qout[[i]], col=col[i], lwd=lwd[1], lty=1)
            matlines(rbind(vvoff, vvoff), qin[[i]], col=col[i], lwd=lwd[2], lty=1)
            points(vvoff, med[[i]], pch=pch, col=col[i])

        }
    }
    if (is.null(labels))
        labels <- parnames
    if (is.null(cex.labels))
        cex.labels <- 1/(log(np)/5 + 1)
    if (labels.loc=="axis")
        axis(axis.side, at=vv, labels=labels, tick=F, las=las, cex.axis=cex.labels)
    if (labels.loc=="above"){
        if (horizontal)
            text(med[[1]], vv, pos=3, labels=labels, cex=cex.labels)
        else
            text(vv, med[[1]], pos=3, labels=labels, cex=cex.labels)
    }

    return(invisible(parnames))
}

