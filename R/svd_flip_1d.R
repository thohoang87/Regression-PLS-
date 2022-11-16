#'svd_flip_1d
#'
#'svd_flip would force us to convert to 2d array and would also return 2d arrays.
#'
#'We don't want that.
#' @param u first left singular vector of X'Y
#' @param v first right singular vector of X'Y
#'
#' @return U, V modified
#' @export
#'
#' @examples
#' get_svd = svd_flip_1d(u, v)
#' u = get_svd$u
#' v = get_svd$v
#'
#' svd_flip_1d = function(u, v)
svd_flip_1d = function(u, v){

  biggest_abs_val_idx = which.max(abs(u))
  sign_= sign(u[biggest_abs_val_idx])
  u = as.vector(u) * sign_
  v = as.vector(v) * sign_

  return(list("u" = u,"v"=v))
}
