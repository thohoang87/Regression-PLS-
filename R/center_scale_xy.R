
#scale function
#' center_scale_xy
#'
#' @param x : dataframe
#'
#' Function return the scaled datafrme with the mean and sd.
#' @param scale : a boolean
#'
#' @return
#' mean : mean of dataframe x
#' sd   : sd of dataframe x
#' X    : the scaled dataframe
#' @export
#'
#' @examples
#' data(iris)
#' x = iris[,1:4]
#' center_scale_xy(x,scale = T)
#'
#'
center_scale_xy =  function(x, scale = T){
  if (scale == T){
    mean = sapply(x, mean)
    sd = sapply(x,sd)
    ifelse(sd ==0, 1,sd)
    X = (x - mean[col(x)])/ sd[col(x)]

  }
  else{
    sd_x = rep(0,times=length(x))
  }

  return(list("mean" = mean, "sd" = sd, "X" = X))
}#end function
