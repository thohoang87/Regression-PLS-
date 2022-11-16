#' get_first_singular_vectors_svd
#'
#' Return the first left and right singular vectors of X'Y
#'
#' @param X variables features
#' @param Y variable to explain
#'
#' @return u, v
#' @export
#'
#' @examples
#' data(iris)
#' X = iris[,1:4]
#' Y = iris[,5]
#' get_svd = get_first_singular_vectors_svd(X, Y)
#' get_svd$u
#' get_svd$v
#'
get_first_singular_vectors_svd = function(X, Y){

  "Return the first left and right singular vectors of X'Y."
  C = t(as.matrix(X))%*%as.matrix(Y)
  ans = svd(C)
  d = ans$d # containing the singular values
  u =ans$u # matrix whose columns contain the left singular vectors
  v = ans$v# matrix whose columns contain the right singular vectors
  return(list("U" = u[,1], "V" =v[1,]))
}
