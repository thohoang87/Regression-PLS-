print.PLSDA=function(objet){
  # Affichage
  cat("Nombres de composantes : ",objet$n_components,"\n")
  cat("Classification functions","\n")
  temp=as.matrix(objet$calc$Coefficients)
  classement = sapply(1:length(objet$calc$Intercept),function(j){temp[,j]+objet$calc$Intercept[j]})
  colnames(classement)=paste(colnames(objet$calc$Y))
  print(classement)
}
