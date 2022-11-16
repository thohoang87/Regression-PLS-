plotInd.PlsDA = function(object, comp1 = 1, comp2 = 2, ellipse = TRUE, labels=TRUE){
  
  
  #verify if the package is installed
  res <- require(ggplot2)
  if (res == FALSE){
    install.packages("ggplot2")
    res <- require(ggplot2)
  }
  
  
  scaled_df <- apply(object$X, 2, scale)
  
  arrests.cov <- cov(scaled_df)
  arrests.eigen <- eigen(arrests.cov)
  PVE <- arrests.eigen$values / sum(arrests.eigen$values)
  PVE = PVE * 100
  
  dcoord = as.data.frame(object$calc$Scores_X)
  dcoord = cbind(dcoord,object$Y)
  colnames(dcoord) = c(paste("Comp",1:object$n_components,sep = ""),"labels")
  
  if (labels == T){
    
    p = ggplot(dcoord, aes(x=dcoord[,comp1], y=dcoord[,comp2],color = labels)) + geom_point()+
      geom_hline(yintercept = 0, alpha = 0.4) + geom_vline(xintercept = 0, alpha = 0.4) +
      geom_text(aes(label=rownames(dcoord)),vjust = -0.9)+
      ggtitle("Individu graph") + 
      xlab(paste("Axis 1 (", round(PVE[comp1], 1), "%)", sep = "")) +
      ylab(paste("Axis 2 (", round(PVE[comp2], 1), "%)", sep = ""))
  }
  else{
    p = ggplot(dcoord, aes(x=dcoord[,1], y=dcoord[,2])) + geom_point()+
      geom_hline(yintercept = 0, alpha = 0.4) + geom_vline(xintercept = 0, alpha = 0.4) +
      geom_text(aes(label=rownames(dcoord)),vjust = -0.9)+
      ggtitle("Individu graph") + 
      xlab(paste("Axis 1 (", round(PVE[comp1], 1), "%)", sep = "")) +
      ylab(paste("Axis 2 (", round(PVE[comp2], 1), "%)", sep = ""))
  }
  
  if (ellipse == T){
    
    p = p + stat_ellipse(aes(x=dcoord[,comp1], y=dcoord[,comp2],fill = labels,color = labels,alpha = 0.25),geom = "polygon")
  }
  
  return(p)
}