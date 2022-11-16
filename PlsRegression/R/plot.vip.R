# ObjectVIP = Objet résultat de la fonction VIP

plot.vip<-function(ObjectVIP){

  vip_sorted = ObjectVIP$vip[order(-ObjectVIP$vip[,ObjectVIP$ncomp]),]
  
  bp=barplot(vip_sorted[,ObjectVIP$ncomp],names.arg=rownames(ObjectVIP$vip))
  pas=0.5
  for (i in 1:length(ObjectVIP$vip[,ObjectVIP$ncomp])){
    if (vip_sorted[i,ObjectVIP$ncomp]>=ObjectVIP$threshold){
      rect(bp[i]-pas,0,bp[i]+pas,sort(ObjectVIP$vip[,ObjectVIP$ncomp],decreasing=T)[i],col="blue")
    }else{
      rect(bp[i]-pas,0,bp[i]+pas,sort(ObjectVIP$vip[,ObjectVIP$ncomp],decreasing=T)[i],col="red")
    }
  }
  abline(h=ObjectVIP$threshold,col="red")
}


