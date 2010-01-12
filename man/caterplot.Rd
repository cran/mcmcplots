\name{caterplot}
\alias{caterplot}

\title{Caterpillar Plots of MCMC Output }

\description{
  Creates plots of credible intervals for parameters from an MCMC simulation.  Because these types of plots have been called "caterpillar" plots by other Bayesian software (like WinBUGS), this function is called \emph{cater}plot, where the "cat" is pronounced as in caterpillar and not as in the word "cater".)
}

\usage{
caterplot(mcmcout, parms = NULL, regex = NULL, random = NULL, quantiles = list(), collapse = TRUE, labels = NULL, labels.loc = "axis", cex.labels = NULL, horizontal=TRUE, val.lim = NULL, lab.lim = NULL, lwd = c(1, 2), pch = 19, col = mcmcplotsPalette(nchains), style=c("gray", "plain"), \dots)
}

\arguments{
  \item{mcmcout}{ a \code{matrix}, \code{bugs}, \code{mcmc}, or \code{mcmc.list} object.  All objects will be coerced to \code{mcmc.list}.}
  \item{parms}{ a vector of character strings that identifies which variables in \code{mcmcout} should be plotted. If \code{parms} and \code{regex} are both \code{NULL}, all parameter will be plotted.}
  \item{regex}{ a vector of character strings with regular expressions that identify which variables in \code{mcmcout} should be plotted.}
  \item{random}{ integer specifying how many parameters from each group will be randomly selected for plotting.  This argument is useful when \code{mcmcout} has a large number of parameters (e.g., from a hierarchical model).  If \code{NULL}, all parameters will be plotted.}
  \item{quantiles}{ list with two elements \code{outer} and \code{inner}. The \code{outer} element of the list should contain the quantiles of the posterior draws that will be plotted as the longer, thinner line.  The \code{inner} element of the list should contain the quantiles of posterior draws that will be plotted as the shorter, thicker line.  If missing, the default is to use \code{list(outer=c(0.025,0.975),inner=c(0.16,0.84))}, which corresponds to 95\% and 68\% credible intervals.}
  \item{collapse}{ if \code{TRUE}, all parallel chains are collapsed into one chain before plotting. If \code{FALSE}, parallel chains are plotted nearly on top of each other with colors as specified in \code{col}.}
  \item{labels}{ labels for the individual "caterpillars."  If \code{NULL}, parameter names in \code{mcmcout} are used.}
  \item{labels.loc}{ if 'axis,' then parameter labels (the names of the parameters) will be plotted on the axis.  If 'above,' then variable names will be plotted above the means for each 'caterplot.' If any other value, no names will be plotted.}
  \item{cex.labels}{ character expansion factor for the plot labels.  If names of parameters will not be plotted, this argument is ignored.}
  \item{horizontal}{ logical indicating whether intervals should be plotted parallel to the x-axis (so \emph{horizontal} lines) or parallel to the y axis.}
  \item{val.lim}{ a vector containing the upper and lower limits for the "value" axis (which is the x axis if \code{horizontal=TRUE}).  If \code{NULL}, the minimum and maximum values found by \code{outer} quantiles are used.}
  \item{lab.lim}{ a vector containing the upper and lower limits for the "label" axis (which is the y axis if \code{horizontal=TRUE}).  If \code{NULL}, limits are automatically selected to contain all the labels of the plotted intervals.}
  \item{col}{ a single value or a vector of values specifying the colors to be used in plotting.}
  \item{lwd}{ a vector of length 2 of line weights used for plotting the inner and outer caterpillars.}
  \item{pch}{ plot character to use in plotting the medians of the intervals.}
  \item{style}{ if "gray", then the plotting region is printed with a gray background, otherwise the default plotting region is used.}
  \item{\ldots}{ further arguments passed to the plotting function.}
}

\details{
  The \code{caterplot} function uses the internal function \code{parms2plot} to match the strings in the \code{parms} argument to the names of the variables in \code{mcmcout}. Quantiles, as specified in the \code{quantiles} argument, are computed for the posterior draws of each variable returned by the call to \code{parms2plot}.  The quantiles are then used to create plots of the posterior intervals of each matched variable. Medians are also plotted as points.

  This function produces a plot similar to the plots produced by the \code{coefplot} function in the R package \pkg{arm} and the caterpillar plots in the WinBUGS software.
}

\value{
  Invisibly returns a character vector with the names of the parameters that were plotted.  This can be useful when the option \code{random} is specified and not all of the parameters are plotted.  See \code{\link{caterpoints}} for an example of how to use the return value.
}

\references{
  None.
}

\author{
  S. McKay Curtis
}

\note{
  None.
}

\seealso{
  \code{\link{caterpoints}}, \code{\link{mcmcplot}}
}

\examples{
## Create fake MCMC output
nc <- 10; nr <- 1000
pnames <- c(paste("alpha[", 1:5, "]", sep=""), paste("gamma[", 1:5, "]", sep=""))
means <- rpois(10, 20)
fakemcmc <- as.mcmc.list(lapply(1:3, function(i) mcmc(matrix(rnorm(nc*nr, rep(means, each=nr)), nrow=nr, dimnames=list(NULL,pnames)))))

## caterplot plots of the fake MCMC output
par(mfrow=c(2,2))
caterplot(fakemcmc, "alpha", collapse=FALSE)
caterplot(fakemcmc, "gamma", collapse=FALSE)
caterplot(fakemcmc, "alpha", labels.loc="axis", col="blue")
caterplot(fakemcmc, "gamma", labels.loc="above", col="red")

caterplot(fakemcmc, "alpha", collapse=FALSE, style="plain")
caterplot(fakemcmc, "gamma", collapse=FALSE, style="plain")
caterplot(fakemcmc, "alpha", labels.loc="axis")
caterplot(fakemcmc, "gamma", labels.loc="above")

caterplot(fakemcmc, "alpha", horizontal=FALSE)
caterplot(fakemcmc, horizontal=FALSE)
caterpoints(rnorm(10, 21, 2), horizontal=FALSE, pch="x", col="red")

\dontrun{
## caterplot works on bugs objects too
library(R2WinBUGS)
example("openbugs", "R2WinBUGS")
## from the help file for openbugs:
schools.sim <- bugs(data, inits, parameters, model.file,
                    n.chains = 3, n.iter = 5000,
                    program = "openbugs", working.directory = NULL)
caterplot(schools.sim, "theta")
}
}

\keyword{hplot}
