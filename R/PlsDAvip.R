# fonction PlsDA.vip
#' PlsDA.vip
#'
#' @param X a dataframe with features
#' @param Y a dataframe with variable to explain
#' @param ncomp a integer
#' @param threshold a integer
#' @param algorithm Type of algorithm to choose for selection
#'
#' @return
#' VIP : A dataframe
#'
#' newX : A new dataframe with features choose
#'
#' important.variables : list of important variables
#'
#' threshold : An integer
#'
#' ncomp : An integer, number of componants
#' @export
#'
#' @examples
#' data(iris)
#' Y = iris[,5]
#' X = iris[,1:4]
#' PlsDA.vip(X, Y, ncomp = 2,algorithm="simpls")
#'
#'
PlsDA.vip = function(X,Y,ncomp,threshold = 1,algorithm="simpls" ){


  # On vérifie l'algorithme utilisé
  if (algorithm=="nipals") {
    objet=NIPALS(X=X,Y=Y,ncomp=ncomp)
  }else if (algorithm=="simpls"){
    objet=SIMPLS(X=X,Y=Y,n_components = ncomp)
  }else{
    stop("L'algorithme choisi est incorrect")
  }

  # On récupère les résultat qui sont stockés dans objet
  R2_varY=objet$R2Y
  explained_varY = objet$R2Y
  w = objet$x_weights

  # Matrice de corrélations
  for (i in 2:ncomp){
    explained_varY[i] <- R2_varY[i] - R2_varY[i-1]
  }

  R2mat <- matrix(0,nrow=ncomp,ncol=ncomp)
  for (j in 1:ncomp){
    R2mat[1:j,j] <- explained_varY[1:j]
  }

  vip=data.frame(sqrt((objet$x_weights^2) %*% R2mat %*% diag(ncol(X)/R2_varY)))
  rownames(vip)=colnames(X)
  colnames(vip)=paste("Dim.",1:ncomp,sep="")
  #détermination des variables importantes
  variable_importante=rownames(vip)[which(vip[,ncomp]>threshold)]



  # Si une seule variable importante, on sélectionne les 2 variables avec le plus haut VIP
  if (length(variable_importante)<2){
    vip_sorted = vip[order(-vip[,ncomp]),]
    variable_importante=rownames(vip_sorted)[1:2]
  }
  # Création d'un nouveau dataset avec uniquement les variables importantes
  newX = X[,variable_importante]


  # Création de l'objet à retourner
  obj_res <- list("vip"=vip,
                  "newX"=newX,
                  "important.variables"=variable_importante,
                  "threshold" = threshold,
                  "ncomp"=ncomp
  )

  #class(obj_res) <- "VIP"
  return(obj_res)
}# end of PlsDA.vip function
