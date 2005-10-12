\name{xproj}
\alias{xproj}
\docType{data}
\title{MCMC Projections from Cod Assessment}
\description{
  Markov-chain Monte Carlo projections from stock assessment of cod
  (\emph{Gadus morhua}) in Icelandic waters.
}
\usage{xproj}
\format{
  List containing two lists:
  \tabular{ll}{
    \code{B}\tab projected biomass by year, given a constant harvest
    rate policy: \code{"0"}, \code{"0.05"}, \code{...},
    \code{"0.50"}.\cr
    \code{Y}\tab projected catch by year, given a constant harvest rate
    policy: \code{"0"}, \code{"0.05"}, \code{...}, \code{"0.50"}.\cr
  }
}
\note{
  MCMC projections can be used to evaluate the short-term outcome of
  harvest policies (constant catch or constant harvest rate), given the
  uncertainty about parameter values and random future recruitment. Some
  aspects of the model are described on the \code{\link{xmcmc}} help
  page, and on the \code{\link[scape]{x.cod}} help page in the
  \pkg{scape} package.

  The list was imported from the files \file{strategy.out},
  \file{projspbm.out} and \file{procatch.out}, using the
  \code{importProj()} function. These files can be found in the
  \file{scapeMCMC/example} directory.

  The functions \code{ll} (package \pkg{gdata}) and \code{head}
  are recommended for browsing MCMC projections, e.g.
  \code{ll(xproj); ll(xproj$B); ll(xproj$B$"0.2"); head(xproj$B$"0.2")}.
}
\references{
  \cite{Hilborn, R., M. Maunder, A. Parma, B. Ernst, J. Payne, and P.
    Starr. 2003. Coleraine: A generalized age-structured stock
    assessment model. User's manual version 2.0. University of
    Washington Report SAFS--UW--0116.} Available at
  http://fish.washington.edu/research/coleraine/coleraine.pdf.

  \cite{Magnusson, A. 2003. Coleraine assessment of the Icelandic cod
    stock. Report for the Icelandic Marine Research Institute.}
  Available from the author.
}
\seealso{
  \code{\link{importMCMC}}, \code{\link{importProj}},
  \code{\link{xmcmc}}.
}
\examples{
plotAuto(xproj$B$"0.25")
plotCumu(xproj$B$"0.25")
plotDens(xproj$B$"0.25")
plotQuant(xproj$B$"0.25")
plotSplom(xproj$B$"0.25")
plotTrace(xproj$B$"0.25")
}
\keyword{datasets}
