\name{plotQuant}
\alias{plotQuant}
\title{Plot MCMC Quantiles}
\description{
  Plot quantiles of multiple Markov-chain Monte Carlo chains, using
  bars, boxes, or lines.
}
\usage{
plotQuant(mcmc, style="boxes", probs=c(0.025,0.975), axes=TRUE,
names=NULL, ylim=NULL, yaxs="i", div=1, log=FALSE, base=10, main=NULL,
xlab=NULL, ylab=NULL, cex.axis=0.8, las=1, tck=-0.015, tick.number=8,
lty.median=1, lwd.median=1+2*(style!="boxes"), col.median="black",
lty.outer=1+2*(style=="lines"), lwd.outer=1, col.outer="darkgrey",
boxfill="darkgrey", boxwex=0.7, mai=c(0.8,1,1,0.6),
mgp=list(bottom=c(2,0.4,0),left=c(3,0.6,0),top=c(0,0.6,0),
right=c(0,0.6,0)), ...)
}
\arguments{
  \item{mcmc}{MCMC chains as a data frame or \code{mcmc} object.}
  \item{style}{how quantiles should be drawn: \code{"bars"},
    \code{"boxes"}, or \code{"lines"}.}
  \item{probs}{outer quantiles to draw, a vector of length 2.}
  \item{axes}{numeric vector indicating which axis labels should be
    drawn: 1=bottom, 2=left, 3=top, 4=right, or \code{TRUE} to display
    all (default).}
  \item{names}{x-axis labels.}
  \item{ylim}{y-axis limits.}
  \item{yaxs}{y-axis style: \code{"i"} to truncate exactly at limits
    (default) or \code{"r"} to extend the axis slightly beyond the
    limits.}
  \item{div}{denominator to shorten values on the y axis.}
  \item{log}{whether values should be log-transformed.}
  \item{base}{logarithm base.}
  \item{main}{main title.}
  \item{xlab}{x-axis label.}
  \item{ylab}{y-axis label.}
  \item{cex.axis}{size of tick labels.}
  \item{las}{orientation of tick labels: 0=parallel, 1=horizontal,
    2=perpendicular, 3=vertical.}
  \item{tck}{tick mark length.}
  \item{tick.number}{number of tick marks.}
  \item{lty.median}{line type of median.}
  \item{lwd.median}{line width of median.}
  \item{col.median}{colour of median.}
  \item{lty.outer}{line type of outer quantiles.}
  \item{lwd.outer}{line width of outer quantiles.}
  \item{col.outer}{colour of outer quantiles.}
  \item{boxfill}{colour of boxes.}
  \item{boxwex}{relative width of boxes.}
  \item{mai}{margins around plot as a vector of four numbers (bottom,
    left, top, right).}
  \item{mgp}{margins around axis titles, labels, and lines as a list of
    four vectors (bottom, left, top, right).}
  \item{\dots}{passed to \code{plot}, \code{bxp}, \code{plotCI},
    \code{lines}, \code{matplot}, \code{axis}, and \code{title}.}
}
\value{
  List containing:
  \item{x}{midpoint coordinates on the x axis.}
  \item{y}{quantile coordinates on the y axis.}
}
\note{
  The \code{Args} function from the \pkg{gdata} package is recommended
  for reviewing the arguments, instead of \code{args}.
}
\author{Arni Magnusson \email{arnima@u.washington.edu}.}
\seealso{
  \code{\link{bxp}}, \code{\link[gplots]{plotCI}},
  \code{\link{matplot}}, \code{\link{quantile}}.

  \code{\link{scapeMCMC-package}} gives an overview of the package.
}
\examples{
plotQuant(xmcmc$B, style="lines", div=1000, xlab="Year",
          ylab="Biomass age 4+ (1000 t)")
plotQuant(xmcmc$R, names=substring(names(xmcmc$R),3), div=1000,
          xlab="Year", ylab="Recruitment (million one-year-olds)")
}
% Graphics
\keyword{hplot}
% Statistics
\keyword{ts}
