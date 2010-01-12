\name{corplot}
\alias{corplot}
\title{ Plot a Correlation Matrix }

\description{
  Creates an image plot of a correlation matrix where colors of different shades represent differing levels of correlation.  One possible use of this function is to plot the correlation between posterior draws of an MCMC run.  Patterns in the plot can aid in constructing a more efficient blocking structure for an MCMC algorithm, where highly correlated parameters should be placed in the same MCMC update block.
}

\usage{
corplot(mat, col = gray(11:0/11), ...)
}

\arguments{
  \item{mat}{ correlation matrix.}
  \item{col}{ colors to be used in the plot.}
  \item{\dots}{ further arguments passed to the plotting function. }
}

\details{
  None.
}

\value{
  Creates a plot.
}

\references{
  None.
}

\author{ S. McKay Curtis }
\note{
  None.
}

\seealso{ \code{image} }

\examples{
Rho <- matrix(c(
 1.00,  0.35, -0.65, -0.66,  0.46,  0.42,
 0.35,  1.00, -0.69, -0.64,  0.40, -0.06,
-0.65, -0.69,  1.00,  0.70, -0.57, -0.11,
-0.66, -0.64,  0.70,  1.00, -0.15, -0.10,
 0.46,  0.40, -0.57, -0.15,  1.00,  0.18,
 0.42, -0.06, -0.11, -0.10,  0.18,  1.00), 6, 6)
dimnames(Rho) <- list(paste("vec[", 1:6, "]", sep=""), paste("vec[", 1:6, "]", sep=""))
corplot(Rho)
}
\keyword{ hplot }
