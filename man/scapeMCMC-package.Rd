\name{scapeMCMC-package}
\alias{scapeMCMC-package}
\alias{scapeMCMC}
\docType{package}
\title{MCMC Diagnostic Plots}
\description{
  Markov chain Monte Carlo diagnostic plots. The purpose of the package
  is to combine existing tools from the \pkg{coda} and \pkg{lattice}
  packages, and make it easy to adjust graphical details. It can be
  useful for anyone using MCMC analysis, regardless of the application.
}
\details{
  \emph{Import Coleraine MCMC results:}
  \tabular{ll}{
    \code{\link{importMCMC}} \tab traces of likelihoods, parameters,
    biomass and recruitment\cr
    \code{\link{importProj}} \tab future projections of biomass and
    catch
  }
  \emph{Diagnostic plots:}
  \tabular{ll}{
    \code{\link{plotTrace}} \tab trends\cr
    \code{\link{plotAuto}}  \tab thinning\cr
    \code{\link{plotCumu}}  \tab convergence\cr
    \code{\link{plotSplom}} \tab confounding of parameters
  }
  \emph{Posterior plots:}
  \tabular{ll}{
    \code{\link{plotDens}}  \tab posterior(s)\cr
    \code{\link{plotQuant}} \tab multiple posteriors on a common y axis
  }
  \emph{Examples:}
  \tabular{ll}{
    \code{\link{xmcmc}}, \code{\link{xproj}} \tab MCMC results and
    projections
  }
}
\note{
  \code{browseVignettes()} shows the vignettes. The main vignette
  \file{scapeMCMC/doc/dsc.pdf} covers \pkg{scapeMCMC} in Section 2.4.

  The plot functions assume that MCMC results are stored either as
  a plain vector (single chain) or in named columns (multiple chains).
  It should be easy for users to arrange their MCMC results in this way.
  The examples demonstrate how several data frames can be stored
  together in nested lists.

  The functions \code{Args} and \code{ll} (package \pkg{gdata}) can be
  useful for browsing unwieldy functions and objects.
}
\author{Arni Magnusson and Ian Stewart.}
\references{
  Magnusson, A. (2005) \cite{R goes fishing: Analyzing fisheries data
    using AD Model Builder and R.} Proceedings of the 5th International
  Workshop on Distributed Statistical Computing. Available at
  \url{http://www.hafro.is/~arnima/uw/s/pdf/dsc.pdf}.
}
\seealso{
  The \pkg{coda} package is a suite of diagnostic functions and plots
  for MCMC analysis, many of which are used in \pkg{scapeMCMC}.
}
% Graphics
\keyword{hplot}
% Statistics
\keyword{ts}
