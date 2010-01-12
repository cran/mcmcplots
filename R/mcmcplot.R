mcmcplot <- function(mcmcout, parms=NULL, regex=NULL, random=NULL, dir=tempdir(), filename="MCMCoutput", extension="html", title=NULL, heading=NULL, col=mcmcplotsPalette(nchains), lty=1, style=c("gray", "plain")){
  ## This must come before mcmcout is evaluated in any other expression
  if (is.null(title))
      title <- paste("MCMC Plots: ", deparse(substitute(mcmcout)), sep="")

  style <- match.arg(style)

  ## Turn off graphics device if interrupted in the middle of plotting
  current.devices <- dev.list()
  on.exit( sapply(dev.list(), function(dev) if(!(dev %in% current.devices)) dev.off(dev)) )

  ## Convert input mcmcout to mcmc.list object
  if (!(is.mcmc(mcmcout) | is.mcmc.list(mcmcout)))
      mcmcout <- as.mcmc(mcmcout)
  if (!is.mcmc.list(mcmcout))
      mcmcout <- mcmc.list(mcmcout)

  nchains <- length(mcmcout)
  htmlfile <- HTMLInitFile(dir, filename, extension, BackGroundColor="#736F6E", Title=title, useLaTeX=FALSE, useGrid=FALSE, CSSFile="")
  if (!is.null(heading))
      cat("<h1 align=center>", heading, "</h1>", sep="", file=htmlfile, append=TRUE)

  ## Select parameters for plotting
  parnames <- varnames(mcmcout)
  if (is.null(parnames))
      stop("Argument 'mcmcout' must have valid variable names. Sorry, chump.")
  parnames <- parms2plot(parnames, parms, regex, random)
  np <- length(parnames)

  for (p in parnames){
    pctdone <- round(100*which(p==parnames)/np)
    cat("\rPreparing plots for ", p, ".  ", pctdone, "% complete.", sep="")
    gname <- paste(p, ".png", sep="")
    png(file.path(dir, gname))
    mcmcplot1(mcmcout[, p, drop=FALSE], col=col, lty=lty, style=style)
    dev.off()
    HTMLInsertGraph(gname, file=htmlfile)
    }
  cat("\r")
  HTMLEndFile(htmlfile)
  full.name.path <- paste("file://", htmlfile, sep="")
  ##full.name.path <- file.path(dir, paste(filename, ".", extension, sep=""))
  ##full.name.path <- paste(dir, "\\", filename, ".", extension, sep="")
  browseURL(full.name.path)
  invisible(full.name.path)
  }

