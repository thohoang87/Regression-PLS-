# screeplot function
#' screeplot_plsDA
#'
#' @param object  : An object of class Pls-DA
#' @param barfill : color barplot
#' @param linecolor : color line
#'
#' @return
#' PVEplot : a screeplot to determinded the number of componants with the criteria of elbow method
#' @export
#'
#' @examples
#' fit_launche : An object of class Pls-DA
#' screeplot_plsDA(object,barfill="steelblue",linecolor = "black")
#'
screeplot_plsDA = function(object,barfill="steelblue",linecolor = "black"){
  #verify if the package is installed
  res <- require(ggplot2) && require(ggpubr)
  if (res == FALSE){
    install.packages("ggplot2")
    install.packages("ggpubr")
    res <- require(ggplot2) && require(ggpubr)
  }
  #verification
  if (res == FALSE){
    stop("Impossible to install the package")
  }


  scaled_df <- apply(object$X, 2, scale)

  arrests.cov <- cov(scaled_df)
  arrests.eigen <- eigen(arrests.cov)

  PVE <- arrests.eigen$values / sum(arrests.eigen$values)
  data.plot <- cbind.data.frame(c(1:fit_launch$n_components), PVE)
  colnames(data.plot) <- c("Components","PVE")

  data.plot<-data.plot %>% group_by(Components)%>%
    mutate(proportions = scales::percent(PVE, 0.1))

  # PVE scree plot
  PVEplot <- ggplot(data.plot, aes(Components, PVE)) +
    geom_bar(stat="identity", fill= barfill, color = "black")  +
    geom_line(color=linecolor) + geom_point(color=linecolor)+
    geom_text(aes(label=proportions),vjust = -0.9) +
    xlab("Principal Component") +
    ylab("PVE") +
    ggtitle("Scree Plot") +
    ylim(0, 1)

  return(ggpubr::ggpar(PVEplot))
}
