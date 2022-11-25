#function create coord of variables
#' Coordonates variables
#'
#' Create coordonate of variables.
#'
#' @param object An object of class Pls-DA
#'
#' @return
#' var.coord : coordinates of variables
#' @export
#'
#' @examples
#' fit_launch : an object of class Pls-DA
#'
#' var.coord(fit_launch)
#'
var.coord = function(object){

  scaled_df <- apply(object$X, 2, scale)

  arrests.cov <- cor(object$X)
  arrests.eigen <- eigen(arrests.cov)

  values = as.matrix(arrests.eigen$values)
  sdev= apply(values,1, sqrt)


  var.coord = matrix(data=0, nrow=nrow(object$calc$Loadings_X), ncol=ncol(object$calc$Loadings_X))
  for (j in 1:ncol(object$calc$Loadings_X)){
    var.coord[,j] = sdev[j] * object$calc$Loadings_X[,j]
  }
  rownames(var.coord) = colnames(object$X)
  nom <- paste("Comp.",1:object$n_components,sep = "")
  colnames(var.coord) = nom
  return(var.coord)
  }

