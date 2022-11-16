# softmax function
softmax <- function(newdata,object){
  n <- object$n_components
  # Yhat calculation
  nclass <- ncol(object$calc$Y)
  temp <- matrix(rep(object$calc$Intercept,each=nrow(newdata)),nrow(newdata),nclass)
  yhat <- as.matrix(newdata)%*%object$calc$Coefficients + temp
  
  # softmax probability
  res <- apply(yhat,1,exp)
  somme <- colSums(res)
  result <- t(res/somme)
  
  # classification
  class <- max.col(result) 
  noms <- colnames(object$calc$Y)
  for (i in 1:nclass) {
    class[class==i]<-noms[i]
  }
  return(list(Clusters=class,Probability=round(result,6)))
}