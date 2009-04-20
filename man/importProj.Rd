\name{importProj}
\alias{importProj}
\title{Import Coleraine MCMC Projections}
\description{
  Import Coleraine MCMC traces for spawning biomass and catch, projected
  into the near future.
}
\usage{
importProj(dir, info="", coda=FALSE, quiet=TRUE)
}
\arguments{
  \item{dir}{directory containing the files \file{strategy.out},
    \file{projspbm.pst} and \file{procatch.pst}.}
  \item{info}{optional string containing information to store with MCMC
    projections.}
  \item{coda}{whether data frames should be coerced to class \code{mcmc}
    using the \pkg{coda} package.}
  \item{quiet}{whether to report progress while parsing files in
    directory.}
}
\value{
  A list containing:
  \item{B}{biomass by catch policy and year}
  \item{Y}{catch by catch policy and year}
  as lists of data frames, or \code{mcmc} objects if \code{coda=TRUE}.
}
\note{
  MCMC projections can be used to evaluate the short-term outcome of
  harvest policies (constant catch or constant harvest rate), given the
  uncertainty about parameter values and random future recruitment.

  The example dataset \code{xproj} was generated using
  \code{importProj}.

  The functions \code{ll} (package \pkg{gdata}) and \code{head}
  are recommended for browsing MCMC projections, e.g.
  \code{ll(xproj); ll(xproj$B); ll(xproj$B$"0.2"); head(xproj$B$"0.2")}.
}
\author{Arni Magnusson \email{arnima@u.washington.edu}.}
\references{
  Hilborn, R., M. Maunder, A. Parma, B. Ernst, J. Payne, and P. Starr.
  2003. \cite{Coleraine: A generalized age-structured stock assessment
    model.} User's manual version 2.0. University of Washington Report
  SAFS-UW-0116. Available at
  http://fish.washington.edu/research/coleraine/coleraine.pdf.
}
\seealso{
  \code{\link{importMCMC}}, \code{\link{read.table}},
  \code{\link{readLines}}, \code{\link{scan}}, \code{\link{xproj}}.

  \code{\link{scapeMCMC-package}} gives an overview of the package.
}
\examples{
\dontrun{
path <- paste(.find.package("scapeMCMC"), "/example", sep="")
xproj <- importProj(path)
}
}
% Programming
\keyword{file}
\keyword{interface}
