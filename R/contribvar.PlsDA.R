#Variables contribution
#' Plot contribution variable
#'
#' return a graph of contribution variables.
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
  res <- require(heatmaply)
  if (res == FALSE){
    install.packages("heatmaply")
    res <- require(heatmaply)
  }

  #verification
  if (res == FALSE){
    stop("Impossible to install the package")
  }

  scaled_df <- apply(object$X, 2, scale)

  arrests.cov <- cor(object$X)
  arrests.eigen <- eigen(arrests.cov)

  values = as.matrix(arrests.eigen$values) #eigen values


  var.coord = var.coord(object) #coordonates
  ctrvar = var.coord**2

  for (k in 1:object$n_components){
    ctrvar[,k] = ctrvar[,k]/values[k]
  }

  p  = heatmaply_cor(
    ctrvar,
    main = "Contribution variable",
    k_col = 2,
    k_row = 2
  )


  return(p)
}

