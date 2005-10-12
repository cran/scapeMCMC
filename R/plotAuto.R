"plotAuto" <-
function(mcmc, thin=1, log=FALSE, base=10, main=NULL, xlab="Lag", ylab="Autocorrelation", lty=1, lwd=1, col="black", ...)
{
  owarn <- options(warn=-1)
  ellipsis <- as.list(substitute(list(...)))[-1]
  mcmc <- if(log) log(mcmc,base=base) else mcmc

  if(is.null(dim(mcmc)))  # vector
  {
    if(is.null(ellipsis$ann) && is.null(ellipsis$axes))    # -,-
      autocorr.plot(window(mcmc(mcmc),thin=thin), lty=lty, lwd=lwd, col=col, ask=FALSE, ann=FALSE, axes=FALSE, ...)
    if(is.null(ellipsis$ann) && !is.null(ellipsis$axes))   # -,axes
      autocorr.plot(window(mcmc(mcmc),thin=thin), lty=lty, lwd=lwd, col=col, ask=FALSE, ann=FALSE, ...)
    if(!is.null(ellipsis$ann) && is.null(ellipsis$axes))   # ann,-
      autocorr.plot(window(mcmc(mcmc),thin=thin), lty=lty, lwd=lwd, col=col, ask=FALSE, axes=FALSE, ...)
    if(!is.null(ellipsis$ann) && !is.null(ellipsis$axes))  # ann,axes
      autocorr.plot(window(mcmc(mcmc),thin=thin), lty=lty, lwd=lwd, col=col, ask=FALSE, ...)
    if(is.null(ellipsis$ann) || ellipsis$ann)
    {
      title(main=main, ...)
      title(xlab=xlab, ...)
      title(ylab=ylab, ...)
    }
    if(is.null(ellipsis$axes) || ellipsis$axes)
    {
      axis(1, ...)
      axis(2, ...)
      box()
    }
  }
  else  # data frame
  {
    autocorr.plot(window(mcmc(mcmc),thin=thin), lty=lty, lwd=lwd, col=col, ask=FALSE, ...)
  }
  options(owarn)

  invisible(NULL)
}

