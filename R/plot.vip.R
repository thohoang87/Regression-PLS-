# ObjectVIP = Objet résultat de la fonction VIP

#' Plot VIP
#'
#' @param ObjectVIP An object of class
#'
#' @return
#' A plot VIP to select variable in the modele
#'
#' @export
#'
#' @examples
#' vip = PlsDA.vip(X,Y,2)
#' plot.vip(vip)
#'
#'
#'
plot.vip<-function(ObjectVIP){

  vip_sorted = ObjectVIP$vip[order(-ObjectVIP$vip[,ObjectVIP$ncomp]),]


  bool = vip_sorted[,1]>=ObjectVIP$threshold
  compColor = ifelse(bool, "rgba( 240, 128, 128, 1 )","rgba( 173, 216, 230, 1 )")

  library(plotly)

  fig <- plot_ly(x = rownames(vip_sorted),y = vip_sorted[,1],name = "test",color = I(compColor),type = "bar")%>%
    layout(
      xaxis=list(title = 'Variables'),
      yaxis=list(title="VIP"),
      title="Variable selection with VIP",
      showlegend=F
        )%>%
    add_lines(y=ObjectVIP$threshold,color = I("rgba(0,0,0, 1 )"))

  return(fig)
}
