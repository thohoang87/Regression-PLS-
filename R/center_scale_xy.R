#scale function
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
