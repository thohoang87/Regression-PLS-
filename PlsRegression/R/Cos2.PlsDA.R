#quality of representativity of variable
Cos2.PlsDA = function(object){
  
  #verify if the package is installed
  res <- require(corrplot)
  if (res == FALSE){
    install.packages("corrplot")
    res <- require(corplot)
  }
  
  res2 <- require(RColorBrewer)
  if (res2 == FALSE){
    install.packages("RColorBrewer")
    res2 <- require(RColorBrewer)
  }
  
  
  
  #call function var.coord
  var.coord = var.coord(object)
  cos2 = var.coord**2
  
  return(corrplot::corrplot(cos2,is.corr = F,col=brewer.pal(n=8, name="RdBu")))  
}
