"plotTrace" <-
function(mcmc, axes=FALSE, same.limits=FALSE, between=list(x=axes,y=axes), div=1, span=1/4, log=FALSE, base=10, main=NULL,
         xlab=NULL, ylab=NULL, cex.main=1.2, cex.lab=1, cex.strip=0.8, cex.axis=0.8, las=0, tck=0.5, tick.number=5,
         lty.trace=1, lwd.trace=1, col.trace="grey", lty.median=1, lwd.median=1, col.median="black", lty.loess=2,
         lwd.loess=1, col.loess="black", plot.it=TRUE, ...)
{
  ## 1 DEFINE FUNCTIONS
  panel.trace <- function(x, y, ...)
  {
    panel.xyplot(x, y, type="l", lty=lty.trace, lwd=lwd.trace, col=col.trace)
    if(any(is.finite(y)) && var(y)>0)
    {
      panel.xyplot(range(x), rep(median(y),2), type="l", lty=lty.median, lwd=lwd.median, col=col.median)
      suppressWarnings(panel.loess(x, y, span=span, lty=lty.loess, lwd=lwd.loess, col=col.loess, ...))  # k-d tree message
    }
  }

  ## 2 PARSE ARGS
  relation <- if(same.limits) "same" else "free"

  ## 3 PREPARE DATA (rearrange, transform)
  if(is.null(dim(mcmc)))  # vector or mcmc(vector)
  {
    mcmc.name <- rev(as.character(substitute(mcmc)))[1]
    mcmc <- matrix(mcmc, dimnames=list(NULL,mcmc.name))
  }
  mcmc <- if(log) log(mcmc/div,base=base) else mcmc/div
  mcmc <- as.data.frame(mcmc)
  n <- nrow(mcmc)
  p <- ncol(mcmc)
  x <- data.frame(Factor=ordered(rep(names(mcmc),each=n),names(mcmc)), Draw=rep(1:n,p), Value=as.vector(as.matrix(mcmc)))

  ## 4 PREPARE PLOT (check device, create list args)
  require(grid, quiet=TRUE, warn=FALSE)
  require(lattice, quiet=TRUE, warn=FALSE)
  if(trellis.par.get()$background$col == "#909090")
  {
    for(d in dev.list()) dev.off()
    trellis.device(color=FALSE)
  }
  mymain <- list(label=main, cex=cex.main)
  myxlab <- list(label=xlab, cex=cex.lab)
  myylab <- list(label=ylab, cex=cex.lab)
  myrot <- switch(as.character(las), "0"=90, "1"=0, "2"=0, "3"=90)
  myscales <- list(x=list(draw=FALSE),
                   y=list(draw=axes,relation=relation,cex=cex.axis,tck=tck,tick.number=tick.number,rot=myrot))
  mystrip <- list(cex=cex.strip)

  ## 5 CREATE TRELLIS OBJECT
  graph <- xyplot(Value~Draw|Factor, panel=panel.trace, data=x, as.table=TRUE, between=between,
                  main=mymain, xlab=myxlab, ylab=myylab, par.strip.text=mystrip,
                  scales=myscales, ...)

  ## 6 FINISH
  if(plot.it)
  {
    print(graph)
    invisible(x)
  }
  else
  {
    invisible(graph)
  }
}

