#' Function print
#'
#' A function who is surchage by the class Pls-DA to print the classification function.
#'
#' @param objet An object of class Pls-DA
#'
#' @return
#'
#' classement : A dataframe
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
  temp=as.matrix(objet$calc$Coefficients)
  classement = sapply(1:length(objet$calc$Intercept),function(j){temp[,j]+objet$calc$Intercept[j]})
  colnames(classement)=paste(colnames(objet$calc$Y))
  print(classement)
}
