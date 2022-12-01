# screeplot function
#' Screeplot
#'
#' @param object  : An object of class Pls-DA
#' @param cololbar  : color of barplot
#' @param linecolor : colorline
#' @param marcker : color point line
#'
#' @return
#' PVEplot : a screeplot to determinded the number of components with the criteria of elbow method
#' @export
#'
#' @examples
#' fit_launche : An object of class Pls-DA
#' screeplot_plsDA(object,barfill="steelblue",linecolor = "black")
#'
screeplot_plsDA = function(object,colorbar = NULL,linecolor = "black",marcker = 'black'){
  #verify if the package is installed
  res <- require(plotly) && require(ggpubr)
  if (res == FALSE){
    install.packages("plotly")
    install.packages("ggpubr")
    res <- require(plotly) && require(ggpubr)
  }
  #verification
  if (res == FALSE){
    stop("Impossible to install the package")
  }


  scaled_df <- apply(object$X, 2, scale)

  scaled_df = scaled_df[,1:object$n_components]

  arrests.cov <- cov(scaled_df)
  arrests.eigen <- eigen(arrests.cov)
  bool = arrests.eigen$values > 1 #kaiser criteria
  compColor = ifelse(bool, "rgba( 240, 128, 128, 1 )","rgba( 173, 216, 230, 1 )")

  PVE <- round(arrests.eigen$values / sum(arrests.eigen$values),2) #variance explain
  data.plot <- cbind.data.frame(c(1:object$n_components), PVE)
  colnames(data.plot) <- c("Components","PVE")

  #data plor with variance explain
  data.plot<-data.plot %>% group_by(Components)%>%
    mutate(proportions = scales::percent(PVE, 0.1))
  #create plot
  fig <- plot_ly(data.plot , x = data.plot$Components, y = data.plot$PVE, type = 'bar', color = I(compColor), name = "")%>%
    layout(
      yaxis = list(
        range=c(0,1), title = 'PVE'
      ), xaxis = list(title = 'Components'), title = 'Pourcentage variance explain',showlegend = FALSE
    )%>%
    add_trace(data.plot , x =data.plot$Components, y = data.plot$PVE, type = 'scatter',  mode = "lm", text =data.plot$proportions, line = list(color = linecolor), marker = list(color = marcker), name = "")


  return(ggpubr::ggpar(fig))
}

