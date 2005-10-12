\name{plotAuto}
\alias{plotAuto}
\title{Plot MCMC Autocorrelation}
\description{
  Plot Markov-chain Monte Carlo autocorrelation over a range of lag
  values. This is a diagnostic plot for deciding whether a chain needs
  further thinning.
}
\usage{
plotAuto(mcmc, thin=1, log=FALSE, base=10, main=NULL, xlab="Lag",
         ylab="Autocorrelation", lty=1, lwd=1, col="black", ...)
}
\arguments{
  \item{mcmc}{MCMC chain(s) as a vector, data frame or \code{mcmc}
    object.}
  \item{thin}{interval to subsample chain(s), or 1 to keep chain
    intact.}
  \item{log}{whether values should be log-transformed.}
  \item{base}{logarithm base.}
  \item{main}{main title.}
  \item{xlab}{x-axis label.}
  \item{ylab}{y-axis label.}
  \item{lty}{line type.}
  \item{lwd}{line width.}
  \item{col}{line colour.}
  \item{\dots}{passed to \code{autocorr.plot()}, \code{title()} and
    \code{axis()}.}
}
\value{Null, but a plot is drawn on the current graphics device.}
\note{
  The \code{Args} function from the \pkg{gdata} package is recommended
  for reviewing the arguments, instead of \code{args}.
}
\author{Arni Magnusson \email{arnima@u.washington.edu}.}
\seealso{
  This function conveniently combines \code{\link[coda]{autocorr.plot}}
  with \code{\link[coda]{mcmc}} and \code{\link[coda]{window.mcmc}},
  adding support for plain vectors and data frames.

  Diagnostic plots for MCMC chains include \code{\link{plotTrace}}
  (trends), \code{plotAuto} (thinning), \code{\link{plotCumu}}
  (convergence), \code{\link{plotSplom}} (confounding of parameters),
  \code{\link{plotDens}} (posterior), and \code{\link{plotQuant}}
  (multiple posteriors).
}
\examples{
plotAuto(xmcmc$P$R0)
plotAuto(xmcmc$P$R0, thin=10)
plotAuto(xmcmc$P, lag.max=50, ann=FALSE, axes=FALSE)
}
% Graphics
\keyword{hplot}
% Statistics
\keyword{ts}
