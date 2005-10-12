"plotSplom" <-
function(mcmc, axes=FALSE, between=0, div=1, log=FALSE, base=10, ...)
{
  ellipsis <- as.list(substitute(list(...)))[-1]
  if(is.null(dim(mcmc))) stop("Argument 'mcmc' must contain more than one chain, arranged in columns.")
  mcmc <- if(log) log(mcmc/div,base=base) else mcmc/div

  if(!axes && is.null(ellipsis$oma))
    pairs(mcmc, gap=between, oma=c(0,0,0,0), xaxt="n", yaxt="n", ...)
  else
    pairs(mcmc, gap=between, ...)

  invisible(NULL)
}

