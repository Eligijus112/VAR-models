mgsub <- function(x, pattern, replacement, ...) {
  if (length(pattern)!=length(replacement)) {
    stop("pattern and replacement do not have the same length.")
  }
  result <- x
  for (i in 1:length(pattern)) {
    result <- gsub(pattern[i], replacement[i], result, ...)
  }
  result
}

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
    if (is.null(layout)) {
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {

    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    for (i in 1:numPlots) {
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

legendary2 <- function(legend, col, line=0,side=3,adj=1, cex=0.8,presymbol = "-",...){
  
  legend <- paste(presymbol,legend)
  olegend <- legend
  
  for(i in 1:length(olegend)) {
    legend <- paste(legend[i:length(olegend)],collapse = "  ")
    mtext(line=line,side=side,text = legend,col=col[i],adj=adj,cex=cex,...)
    legend <- olegend
  }
}

L <- function(x, lag){
  lagged <- c(rep(NA, lag), x[1:(length(x) - lag)])
  return(lagged)
}

function (results2sls, results3sls) 
{
  result <- list()
  if (is.null(results2sls$restrict.regMat)) {
    result$q <- coef(results2sls) - coef(results3sls)
    result$qVar <- vcov(results2sls) - vcov(results3sls)
  }
  else {
    result$q <- coef(results2sls, modified.regMat = TRUE) - 
      coef(results3sls, modified.regMat = TRUE)
    result$qVar <- vcov(results2sls, modified.regMat = TRUE) - 
      vcov(results3sls, modified.regMat = TRUE)
  }
  result$statistic <- crossprod(result$q, solve(result$qVar, 
                                                result$q))
  names(result$statistic) <- "Hausman"
  result$parameter <- nrow(result$qVar)
  names(result$parameter) <- "df"
  result$p.value <- 1 - pchisq(result$statistic, result$parameter)
  result$method = paste("Hausman specification test for consistency of", 
                        "the 3SLS estimation")
  if ("data" %in% names(results2sls$call)) {
    result$data.name <- results2sls$call$data
  }
  else {
    result$data.name <- "unknown"
  }
  class(result) <- "htest"
  return(result)
}

