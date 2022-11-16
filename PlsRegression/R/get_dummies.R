
#' Function Get Dummies
#'
#' @param dataframe
#'
#' @return A dataframe of variables binarize
#' @export
#'
#' @examples
#' get_dummies(Y)
get_dummies = function(d){
  #verify if the package is installed
  res <- require(fastDummies)
  if (res == FALSE){
    install.packages("fastDummies")
    res <- require(fastDummies)
  }
  #verification
  if (res == FALSE){
    stop("Impossible to install the package")
  }
  d = as.factor(d)
  d_dummies = fastDummies::dummy_cols(d)
  d_dummies = d_dummies[,-1]
  colnames(d_dummies) = levels(d)

  return(d_dummies)
}

