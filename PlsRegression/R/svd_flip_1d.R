svd_flip_1d = function(u, v){
  
  biggest_abs_val_idx = which.max(abs(u))
  sign_= sign(u[biggest_abs_val_idx])
  u = as.vector(u) * sign_
  v = as.vector(v) * sign_
  
  return(list("u" = u,"v"=v))
}