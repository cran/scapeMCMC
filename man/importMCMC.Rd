\name{importMCMC}
\alias{importMCMC}
\title{Import Coleraine MCMC Results}
\description{
  Import Coleraine MCMC traces for likelihoods, parameters, spawning
  biomass, and recruitment.
}
\usage{
importMCMC(dir, info="", coda=FALSE, quiet=TRUE, pretty.labels=FALSE,
           l.choose=NULL, p.choose=NULL)
}
\arguments{
  \item{dir}{directory containing the files \file{mcmclike.out},
    \file{params.pst}, \file{spawbiom.pst} and \file{recruits.pst}.}
  \item{info}{optional string containing information to store with MCMC
    results.}
  \item{coda}{whether data frames should be coerced to class \code{mcmc}
    using the \pkg{coda} package.}
  \item{quiet}{whether to report progress while parsing files in
    directory.}
  \item{pretty.labels}{whether likelihood and parameter columns should
    be renamed}
  \item{l.choose}{vector of strings, indicating which likelihood
    components to import, or \code{NULL} to import all.}
  \item{p.choose}{vector of strings, indicating which parameters to
    import, or \code{NULL} to import all.}
}
\value{
  A list containing:
  \item{L}{likelihoods}
  \item{P}{parameters}
  \item{B}{biomass by year}
  \item{R}{recruitment by year}
  as data frames, or \code{mcmc} objects if \code{coda=TRUE}.
}
\note{
  The example dataset \code{xmcmc} was generated using
  \code{importMCMC}.

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
}
\seealso{
  \code{\link{read.table}}, \code{\link{readLines}}, and
  \code{\link{scan}} import any data.

  \code{importMCMC} and \code{\link{importProj}} import Coleraine MCMC
  results.

  \code{\link{xmcmc}} was created using \code{importMCMC}.

  \code{\link{scapeMCMC-package}} gives an overview of the package.
}
\examples{
\dontrun{
dir <- system.file("example", package="scapeMCMC")
xmcmc <- importMCMC(dir)  # or rename and select particular elements:
xmcmc <- importMCMC(dir, pretty.labels=TRUE,
                    l.choose=c("CAc","CAs","Survey","Prior","Total"),
                    p.choose=c("R0","Rinit","uinit","cSleft","cSfull",
                               "sSleft","sSfull","logq"))
}
}
% Programming
\keyword{file}
\keyword{interface}
