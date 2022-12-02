#correlation circle of variables
#' Circle correlation plot
#'
#'Create a correlation circle with coordinate of variables.
#'
#' @param object An object of class Pls-DA
#' @param comp1 choose the first componant
#' @param comp2 choose the second componant
#'
#' @return
#' A correlation circle with coordinate of variables
#' @export
#'
#' @examples
#' fit_launch : an object of class Pls-DA
#' plotVar.PlsDA(fit_launch)
#'
plotVar.PlsDA = function(object, comp1 = 1, comp2 = 2){
  #plotting variables
  #package ggplot2

  #verify if the package is installed
  res <- require("plotly")
  if (res == FALSE){
    install.packages("plotly")
    res <- require("plotly")
  }

  scaled_df <- apply(object$X, 2, scale)

  arrests.cov <- cov(scaled_df)
  arrests.eigen <- eigen(arrests.cov)

  PVE <- round(arrests.eigen$values / sum(arrests.eigen$values),2) *100 #pourcentage variance explain

  var.coord = var.coord(object) #coordonate variables

  d  = cbind(var.coord[,comp1],var.coord[,comp2])

  layout <- list(
    title = "Correlation circle variables",
    xaxis = list(title = paste("Axis 1 (", round(PVE[comp1], 1), "%)", sep = "")),
    yaxis = list(title = paste("Axis 2 (", round(PVE[comp2], 1), "%)", sep = "")),
    width = 600,
    height = 300,
    shapes = list(
      list(
        x0 = -1,
        x1 = 1,
        y0 = -1,
        y1 = 1,
        type = "circle"
      )
    )
  )
  p <- plot_ly()

  p <- add_trace(p,type = 'scatter', mode="markers",name = rownames(object$calc$Loadings_X),type = 'scatter', x=var.coord[,comp1], y=var.coord[,comp2])
  p <- layout(p,autosize = T, title=layout$title, width=layout$width, xaxis=layout$xaxis, yaxis=layout$yaxis, shapes=layout$shapes)



  return(p)
}
