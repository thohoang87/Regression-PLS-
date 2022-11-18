#surcharge de summary
#' Title
#'
#' @param object
#'
#' @return
#' @export
#'
#' @examples
summary.plsDA <- function(object){
  cat("PLSDA fitted with the ",object$algorithm, "algorithm","\n")
  cat("Number of components : ",object$n_components,"\n")
  cat("-----------------------------------------------","\n")
  cat("Regression coefficients : ","\n")
  print(object$calc$Coefficients)
  cat("Intercept : ",object$calc$Intercept,"\n")
  cat("-----------------------------------------------","\n")
  cat("x_weights : ")
  print(object$calc$x_weights)
  cat("-----------------------------------------------","\n")
  cat("Loadings_X : ")
  print(object$calc$Loadings_X)
  cat("-----------------------------------------------","\n")
  cat("Scores_X : ")
  print(object$calc$Scores_X)
  cat("-----------------------------------------------","\n")
  cat("Loadings_Y : ")
  print(object$calc$Loadings_Y)
  cat("-----------------------------------------------","\n")
  cat("Scores_Y : ")
  print(object$calc$Scores_Y)
  cat("-----------------------------------------------","\n")
  cat("Explained variance of X : ")
  print(object$calc$R2X)
  cat("-----------------------------------------------","\n")
  cat("Explained variance of Y : ")
  print(object$calc$R2Y)
}

