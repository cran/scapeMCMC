\name{xmcmc}
\alias{xmcmc}
\docType{data}
\title{MCMC Results from Cod Assessment}
\description{
  Markov-chain Monte Carlo results from stock assessment of cod
  (\emph{Gadus morhua}) in Icelandic waters.
}
\usage{xmcmc}
\format{
  List containing four data frames:
  \tabular{ll}{
    \code{L}\tab likelihood components: \code{CAc} (commercial catch at
    age), \code{CAs} (survey catch at age), \code{Survey} (survey
    abundance index), \code{Prior} (penalty for priors), \code{Total}
    (sum of all likelihood components; the objective function).\cr
    \code{P}\tab estimated parameters: \code{R0} (average virgin
    recruitment), \code{Rinit} (initial recruitment scaler),
    \code{uinit} (initial harvest rate), \code{cSleft} (left-side slope
    of commercial selectivity curve), \code{cSfull} (age at full
    commercial selectivity), \code{sSleft} (left-side slope of survey
    selectivity curve), \code{sSfull} (age at full survey selectivity),
    \code{logq} (log-transformed survey catchability).\cr
    \code{B}\tab predicted biomass (age 4+) by year.\cr
    \code{R}\tab predicted recruitment by year.\cr
  }
}
\details{
  Some aspects of the model are described on the
  \code{\link[scape]{x.cod}} help page in the \pkg{scape} package.
}
\note{
  The list was imported from the files \file{mcmclike.out},
  \file{params.pst}, \file{spawbiom.pst} and \file{recruits.pst}, using
  the \code{importMCMC()} function. These files can be found in the
  \file{scapeMCMC/example} directory.

  The functions \code{ll} (package \pkg{gdata}) and \code{head}
  are recommended for browsing MCMC results, e.g.
  \code{ll(xmcmc); ll(xmcmc$P); head(xmcmc$P)}.
}
\references{
  Hilborn, R., M. Maunder, A. Parma, B. Ernst, J. Payne, and P. Starr.
  2003. \cite{Coleraine: A generalized age-structured stock assessment
    model.} User's manual version 2.0. University of Washington Report
  SAFS-UW-0116. Available at
  \url{http://fish.washington.edu/research/coleraine/coleraine.pdf}.

  Magnusson, A. 2003. \cite{Coleraine assessment of the Icelandic cod
    stock.} Report for the Icelandic Marine Research Institute.
  Available from the author.
}
\seealso{
  \code{\link{importMCMC}} was used to import the MCMC results.

  \code{xmcmc} and \code{\link{xproj}} are MCMC results and projections
  to explore.

  \code{\link{scapeMCMC-package}} gives an overview of the package.
}
\examples{
plotTrace(xmcmc$L)
plotAuto(xmcmc$P)
plotCumu(xmcmc$P)
plotSplom(xmcmc$P)
plotDens(xmcmc$P)
plotQuant(xmcmc$B)
}
\keyword{datasets}
