#quality of representativity of variable
#' Cos2 graph
#'
#' @param object An object of class Pls-DA
#'
#' @return
#' Cos2 : quality representation of variables
#' @export
#'
#' @examples
#' fit_launch : An object of class Pls-DA
#' Cos2.PlsDA(fit_launch)
Cos2.PlsDA = function(object){

  res <- require(heatmaply)
  if (res == FALSE){
    install.packages("heatmaply")
    res <- require(heatmaply)
  }

  #call function var.coord
  var.coord = var.coord(object)
  cos2 = var.coord**2 #quality representation on the axis

  p  = heatmaply_cor(
    cos2,
    main = "Representation quality of variables",
    k_col = 2,
    k_row = 2
  )

  return(p)
}
