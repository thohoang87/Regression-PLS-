#Variables contribution
#' contribvar.PlsDA
#'
#' return a graph of contribution variables
#'
#' @param object An object of class Pls-DA
#'
#' @return
#' corrplot of contribution variables
#'
#' @export
#'
#' @examples
#' fit_launch : An object of class Pls-DA
#' contribvar.PlsDA(fit_launch)
#'
contribvar.PlsDA = function(object){

  #verify if the package is installed
  res <- require(corrplot)
  if (res == FALSE){
    install.packages("corrplot")
    res <- require(corplot)
  }

  res2 <- require(RColorBrewer)
  if (res2 == FALSE){
    install.packages("RColorBrewer")
    res2 <- require(RColorBrewer)
  }

  #verification
  if (res == FALSE |res2 == FALSE ){
    stop("Impossible to install the package")
  }

  scaled_df <- apply(object$X, 2, scale)

  arrests.cov <- cor(object$X)
  arrests.eigen <- eigen(arrests.cov)

  values = as.matrix(arrests.eigen$values)


  var.coord = var.coord(object)
  ctrvar = var.coord**2

  for (k in 1:object$n_components){
    ctrvar[,k] = ctrvar[,k]/values[k]
  }
  print(ctrvar)

  return(corrplot::corrplot(ctrvar,is.corr = F,col=brewer.pal(n=8, name="RdBu")))
}

