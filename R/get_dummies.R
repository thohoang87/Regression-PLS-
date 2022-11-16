
#' Function Get Dummies
#'
#' It's a function who take a matrix or dataframe of one qualitative variable and to return variables binarize
#' @param X dataframe
#'
#' @return A dataframe of variables binarize
#' @export
#'
#' @examples
#' data(iris)
#' y = iris[,5]
#' y = get_dummies(y)
#' print(y)
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

