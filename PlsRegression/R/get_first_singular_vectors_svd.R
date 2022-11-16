get_first_singular_vectors_svd = function(X, Y){
  
  "Return the first left and right singular vectors of X'Y."
  C = t(as.matrix(X))%*%as.matrix(Y)
  ans = svd(C)
  d = ans$d # containing the singular values 
  u =ans$u # matrix whose columns contain the left singular vectors 
  v = ans$v# matrix whose columns contain the right singular vectors
  return(list("U" = u[,1], "V" =v[1,]))
}
