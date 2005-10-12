"importProj" <-
function(dir, info="", coda=FALSE, quiet=TRUE)
{
  get.Policies <- function()
  {
    if(!quiet) cat("Policies  ")
    Policies <- read.table(paste(dir,"strategy.out",sep="/"), skip=1)
    if(!quiet) cat("file...")
    Policies <- unique(as.vector(as.matrix(Policies)))
    if(!quiet) cat("unique...OK\n")
    return(Policies)
  }

  get.Years <- function()
  {
    if(!quiet) cat("Years     ")
    Years <- read.table(paste(dir,"strategy.out",sep="/"), nrows=1)
    if(!quiet) cat("file...")
    Years <- unlist(strsplit(as.matrix(Years),"_"))
    if(!quiet) cat("labels...")
    Years <- unique(matrix(Years, nrow=3)[2,])
    if(!quiet) cat("unique...OK\n")
    return(Years)
  }

  get.B <- function(Policies, Years)
  {
    if(!quiet) cat("Biomass   ")
    B <- read.table(paste(dir,"projspbm.out",sep="/"), header=TRUE)[,-c(1,2)]
    if(!quiet) cat("file...")
    Blist <- list()
    for(p in 1:length(Policies))
    {
      from <- (p-1)*length(Years) + 1
      to <- p*length(Years)
      Blist[[p]] <- B[,from:to]
      names(Blist[[p]]) <- Years
    }
    names(Blist) <- Policies
    B <- Blist
    if(!quiet) cat("list...OK\n")
    return(B)
  }

  get.Y <- function(Policies, Years)
  {
    if(!quiet) cat("Landings  ")
    Y <- read.table(paste(dir,"procatch.out",sep="/"), header=TRUE)
    if(!quiet) cat("file...")
    Ylist <- list()
    for(p in 1:length(Policies))
    {
      from <- (p-1)*length(Years) + 1
      to <- p*length(Years)
      Ylist[[p]] <- Y[,from:to]
      names(Ylist[[p]]) <- Years
    }
    names(Ylist) <- Policies
    Y <- Ylist
    if(!quiet) cat("list...OK\n")
    return(Y)
  }

  files <- paste(dir, c("strategy.out","projspbm.out","procatch.out"), sep="/")
  sapply(files, function(f)  # verify that files are there
         if(!file.exists(f)) stop("File ",f," does not exist. Please check the 'dir' argument.",call.=FALSE))

  if(!quiet) cat("\nParsing files in directory ", dir, ":\n\n", sep="")
  Policies <- get.Policies()
  Years <- get.Years()
  B <- get.B(Policies, Years)
  Y <- get.Y(Policies, Years)
  if(!quiet) cat("\n")

  output <- list(B=B, Y=Y)
  if(coda)
  {
    require(coda, quiet=TRUE, warn=FALSE)
    output <- lapply(output, function(x) lapply(x,mcmc))
  }
  attr(output, "call") <- match.call()
  attr(output, "info") <- info

  return(output)
}

