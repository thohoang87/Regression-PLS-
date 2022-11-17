#correlation circle of variables
#' Variables plot
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
  res <- require("ggplot2")
  if (res == FALSE){
    install.packages("ggplot2")
    res <- require("ggplot2")
  }

  var.coord = var.coord(object)

  dat <- circleFun(c(0,0),2,npoints = 100)

  d  = cbind(var.coord[,comp1],var.coord[,comp2])
  #geom_path will do open circles, geom_polygon will do filled circles
  p = ggplot(dat, aes(x,y)) + geom_path(size = 1)
  for (i in 1: nrow(d)){
    p = p + geom_segment(x = 0 ,y = 0, xend = d[i,comp1], yend = d[i,comp2],size = 1.0,
                         arrow = arrow(length = unit(0.5, "cm"))) +
      geom_text(x = d[i,comp1], y = d[i,comp2],
                label=rownames(object$calc$Loadings_X)[i], color = "blue", size  = 4.5,
                vjust = -1)
  }
  p  = p + geom_hline(yintercept = 0, alpha = 0.4,linetype = "longdash") +
    geom_vline(xintercept = 0, alpha = 0.4,linetype = "longdash") +
    theme(aspect.ratio = 1) + coord_cartesian(clip = "off") +
    xlab(paste0("Comp ", comp1)) +
    ylab(paste0("Comp ", comp2))

  return(p)
}
