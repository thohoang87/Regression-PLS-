# correlationplot function
#' Correlation plot
#'
#' A graphic to show correlation between variables
#'
#' @param object An object of class Pls-DA
#' @param usedcomp an integer for the number of componant
#'
#' @return
#' correlation plot of variables
#' @export
#'
#' @examples
#' fit_launch : an object of class Pls-DA
#' correlationplot_plsDA(fit_launch, usedcomp=1)
#'
correlationplot_plsDA <- function(object,usedcomp=1){

  #verify the component selected
  if (is.null(usedcomp) || usedcomp < 1 || usedcomp >ncol(object$calc$x_weights)){
    #correlation standard
    corrplot::corrplot(cor(object$X))
  } else
  {
    #sort variables based on correlation with the component selected
    ordre <- order(object$calc$Loadings_X[,usedcomp])

    heatmaply_cor(
      cor(object$X[ordre]),
      main = paste("Correlation matrix between the ",usedcomp," composant and others",sep = ""),
      k_col = 2,
      k_row = 2
    )
  }
} #end of correlationplot.plsDA function


