"plotCumu" <-
function(mcmc, probs=c(0.025,0.975), div=1, log=FALSE, base=10, main=NULL, xlab="Iterations", ylab="Value", lty.median=1,
         lwd.median=2, col.median="black", lty.outer=2, lwd.outer=1, col.outer="black", ...)
{
  owarn <- options(warn=-1)
  ellipsis <- as.list(substitute(list(...)))[-1]
  mcmc <- if(log) log(mcmc/div,base=base) else mcmc/div
  probs <- c(min(probs), 0.5, max(probs))
  lty <- c(lty.outer, lty.median)
  lwd <- c(lwd.outer, lwd.median)
  col <- c(col.outer, col.median)

  if(is.null(dim(mcmc)))  # vector
  {
    if(is.null(ellipsis$ann) && is.null(ellipsis$axes))    # -,-
      cumuplot(mcmc(mcmc), probs=probs, ask=FALSE, lty=lty, lwd=lwd, col=col, ann=FALSE, axes=FALSE, ...)
    if(is.null(ellipsis$ann) && !is.null(ellipsis$axes))   # -,axes
      cumuplot(mcmc(mcmc), probs=probs, ask=FALSE, lty=lty, lwd=lwd, col=col, ann=FALSE, ...)
    if(!is.null(ellipsis$ann) && is.null(ellipsis$axes))  # ann,-
      cumuplot(mcmc(mcmc), probs=probs, ask=FALSE, lty=lty, lwd=lwd, col=col, axes=FALSE, ...)
    if(!is.null(ellipsis$ann) && !is.null(ellipsis$axes))  # ann,axes
      cumuplot(mcmc(mcmc), probs=probs, ask=FALSE, lty=lty, lwd=lwd, col=col, ...)
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
    cumuplot(mcmc(mcmc), probs=probs, ask=FALSE, xlab=xlab, ylab=ylab, ...)
  }
  options(owarn)

  invisible(NULL)
}

