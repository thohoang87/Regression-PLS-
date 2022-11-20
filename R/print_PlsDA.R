#' Function print
#'
#' A function who is surchage by the class Pls-DA to print the classification function.
#'
#' @param objet An object of class Pls-DA
#'
#' @return
#'
#' classification : A dataframe
#'
#' @export
#'
#' @examples
#' fit_launch : an object of class Pls-DA
#' print.PLSDA(fit_launch)
#'
print.PLSDA=function(objet){
  # Affichage
  cat("Nombres de composantes : ",objet$n_components,"\n")
  cat("Classification functions","\n")
  colnames(objet$calc$Coefficients) = colnames(objet$calc$Y)
  rownames(objet$calc$Coefficients) = colnames(objet$calc$X)
  classification = rbind(objet$calc$Coefficients,objet$calc$Intercept)

  print(classification)
}


