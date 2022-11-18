summary.PLSDA=function(objet){

  # Tableau des scores en fonction de la dimensions pour les individus

  scores <- matrix(0,nrow=10,ncol=objet$n_components*2)
  scores=as.data.frame(scores)
  k_ncomp=1
  for (j in seq(1,objet$n_components*2,by=2)) {
    for (i in 1:10){
      scores[i,j]=objet$calc$Scores_X[i,k_ncomp]
      colnames(scores)[j]=paste("Scores_X, Dim.",k_ncomp)
      scores[i,j+1]=objet$calc$Scores_Y[i,k_ncomp]
      colnames(scores)[j+1]=paste("Scores_Y, Dim.",k_ncomp)
    }
    k_ncomp=k_ncomp+1
  }

  # Tableau des VIP et R2 en fonction de la dimensions
  temp = matrix(0,nrow=length(rownames(objet$VIP$vip)),ncol=objet$n_components*2)
  vip_r2 = as.data.frame(temp)
  row.names(vip_r2)=rownames(objet$VIP$vip)
  k_ncomp=1
  for (j in seq(1,objet$n_components*2,by=2)) {
    for (i in 1:length(rownames(objet$VIP$vip))){
      vip_r2[i,j]=objet$calc$R2Var[i,k_ncomp]
      vip_r2[i,j+1]=objet$VIP$vip[i,k_ncomp]
    }
    colnames(vip_r2)[j]=paste("R2, Dim.",k_ncomp)
    colnames(vip_r2)[j+1]=paste("VIP, Dim.",k_ncomp)
    k_ncomp=k_ncomp+1
  }

    # Affichage
    cat("Score for individuals (10 first) : ","\n")
    print(scores)
    cat("\n","\n")
    cat("R2 and VIP for variables :","\n")
    print(vip_r2)
    cat("\n","\n")
    cat("Importantes variables : ",objet$VIP$important.variables)
  }

