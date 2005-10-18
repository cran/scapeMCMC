\name{plotSplom}
\alias{plotSplom}
\title{Plot MCMC Scatterplot Matrix}
\description{
  Plot scatterplots of multiple Markov-chain Monte Carlo chains. This is
  a diagnostic plot for deciding whether parameters are confounded. When
  parameter estimates are highly dependent on each other, it may
  undermine conclusions based on MCMC results of that model.
}
\usage{
plotSplom(mcmc, axes=FALSE, between=0, div=1, log=FALSE, base=10, ...)
}
\arguments{
  \item{mcmc}{MCMC chains as a data frame or \code{mcmc} object.}
  \item{axes}{whether axis values should be plotted.}
  \item{between}{space between panels.}
  \item{div}{denominator to shorten values on the y axis.}
  \item{log}{whether values should be log-transformed.}
  \item{base}{logarithm base.}
  \item{\dots}{passed to \code{pairs()}.}
}
\value{Null, but a plot is drawn on the current graphics device.}
\note{
  The \code{Args} function from the \pkg{gdata} package is recommended
  for reviewing the arguments, instead of \code{args}.
}
\author{Arni Magnusson \email{arnima@u.washington.edu}.}
\seealso{
  \code{\link{pairs}}, \code{\link[lattice]{splom}}.

  \code{\link{scapeMCMC-package}} gives an overview of the package.
}
\examples{
plotSplom(xmcmc$P)
plotSplom(xproj$B$"0.25", axes=TRUE, between=1, div=1000,
          main="Future biomass", cex.labels=1.5)
}
% Graphics
\keyword{hplot}
% Statistics
\keyword{ts}
