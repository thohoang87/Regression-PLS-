#' Individu plot
#'
#' Create the individu plot on plan with two componants.
#'
#' @param object An object of class Pls-DA
#' @param comp1 choose the first componant
#' @param comp2 choose the second componant
#'
#' @return
#' p : Plot of individu
#' @export
#'
#' @examples
#' fit_launch : an object of class Pls-DA
#' plotInd.PlsDA(fit_launch, comp1 = 1, comp2 = 2)
#'
plotInd.PlsDA = function(object, comp1 = 1, comp2 = 2){


  #verify if the package is installed
  res <- require(plotly)
  if (res == FALSE){
    install.packages("plotly")
    res <- require(plotly)
  }

  scaled_df <- apply(object$X, 2, scale)

  arrests.cov <- cov(scaled_df)
  arrests.eigen <- eigen(arrests.cov)
  PVE <- arrests.eigen$values / sum(arrests.eigen$values)
  PVE = PVE * 100

  dcoord = as.data.frame(object$calc$Scores_X)
  dcoord = cbind(dcoord,object$Y)
  colnames(dcoord) = c(paste("Comp",1:object$n_components,sep = ""),"labels")

  fig <- plot_ly(dcoord, x = ~dcoord[,comp1], y = ~dcoord[,comp2], color =object$Y, type = 'scatter', mode = 'markers')%>%
    layout(
      title = "Individu graph",
      legend=list(title=list(text="Individu colors")),
      xaxis = list(
        title = paste("Axis 1 (", round(PVE[comp1], 1), "%)", sep = ""),
        zerolinecolor = "#black",
        zerolinewidth = 2),
      yaxis = list(
        title = paste("Axis 2 (", round(PVE[comp2], 1), "%)", sep = ""),
        zerolinecolor = "#black",
        zerolinewidth = 2)
    )
  return(fig)
}

