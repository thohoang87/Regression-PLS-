# correlationplot function
correlationplot_plsDA <- function(object,usedcomp=1){
  #verify if the package is installed
  res <- require(corrplot)
  if (res == FALSE){
    install.packages("corrplot")
    res <- require(corplot)
  }
  #verification
  if (res == FALSE){
    stop("Impossible to install the package")
  }
  
  #verify the component selected
  if (is.null(usedcomp) || usedcomp < 1 || usedcomp >ncol(object$calc$x_weights)){
    #correlation standard
    corrplot::corrplot(cor(object$X))
  } else
  {
    #sort variables based on correlation with the component selected
    ordre <- order(object$calc$Loadings_X[,usedcomp])
    #correlation plot
    corrplot::corrplot(cor(object$X[ordre]))
  }
} #end of correlationplot.plsDA function
