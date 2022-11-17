# return point to create circle
#' circleFunction
#'
#' @param center   : origine of circle
#' @param diameter : diameter of circle
#' @param npoints  : number of points to create circle
#'
#' @return
#' x = coordonate x of points
#' y = coordonate y of points
#' @export
#'
#' @examples
#' circleFun(center = c(0,0),diameter = 1, npoints = 100)
#'
circleFun <- function(center = c(0,0),diameter = 1, npoints = 100){
  r = diameter / 2
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}
