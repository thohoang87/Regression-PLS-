#Function with SIMPLS's algorithm
#' SIMPLS algorithm
#'
#' @description
#' This function include simpls algorithm for pls regression.
#' Y must numeric
#'
#' @param X variables features
#' @param Y variable to explain
#' @param n_components number of composants
#'
#' @return
#' X : dataframe of variables features
#'
#' Y : datafrme of variable to explain
#'
#' classification : classification report (coefficient, intercept)
#'
#' Coefficients : coefficients of regression
#'
#' Intercept: intercept of regression
#'
#' x_weights : dataframe weights of X
#'
#' y_weights : dataframe weights of y
#'
#' Scores_X : dataframe scores of X
#'
#' Scores_Y : dataframe scores of X
#'
#' Loadings_X : dataframe loadings of X
#'
#' Loadings_Y : dataframe loadings of Y
#'
#' R2X : R square of X
#'
#' R2Y : R square of Y
#'
#' R2Var : R square of variables
#'
#' @export
#'
#' @examples
#' data(iris)
#' X = iris[,1:4]
#' Y = iris[,5]
#' n_components = 2
#' SIMPLS(X,Y,n_components)
#'
SIMPLS = function(X,Y,n_components = ncomp){


  # verify data: dataframe or matrix
  ok <- (is.data.frame(X) || is.matrix(X) || is.data.frame(Y) || is.matrix(Y))
  if (!ok){
    stop("X and Y should be a dataframe or a matrix")
  }

  # verify data: numeric
  nb_ok<- sum(sapply(X,function(x){is.numeric(x)})) + sum(sapply(Y,function(x){is.numeric(x)}))
  if (nb_ok < (ncol(X) + ncol(Y))){
    stop("Some of the elements inside X or Y are not numeric")
  }

  # check the number of components
  if (n_components>ncol(X)){
    cat("Number of components reduced to : ",ncol(X),"\n")
    n_components = ncol(X)
  }
  if (is.null(n_components) || n_components ==0){
    cat("Number of components is nul","\n")
  }

  #call function scale
  CS_x = center_scale_xy(X,scale = T)
  Xk = CS_x$X
  x_mean = CS_x$mean
  sd_x = CS_x$sd

  CS_y = center_scale_xy(Y,scale = T)
  Yk = CS_y$X
  Y_mean = CS_y$mean
  sd_y = CS_y$sd

  n = nrow(X)
  p = ncol(X)
  q = ncol(as.data.frame(Y))

  x_weights_ = matrix(data=0, nrow=n_components, ncol=p)#u
  y_weights_ = matrix(data=0, nrow=n_components, ncol=q)  # V
  x_scores_ = matrix(data=0, nrow=n, ncol=n_components) # Xi
  y_scores_ = matrix(data=0, nrow=n, ncol=n_components)  # Omega
  x_loadings_ = matrix(data=0, nrow=n_components,ncol=p)  # Gamma
  y_loadings_ = matrix(data=0, nrow=n_components, ncol=q)  # Delta
  res_r2X <- c() # R2 coefficients of X
  res_r2Y <- c() # R2 coefficients of Y
  res_r2Var <- c() # R2 coefficients of variables

  for (k in 1:n_components){

    svd = get_first_singular_vectors_svd(Xk,Yk)
    x_weights = svd$U
    y_weights = svd$V
    svd_flip = svd_flip_1d(x_weights, y_weights)
    x_weights = svd_flip$u
    y_weights = svd_flip$v

    x_scores  = as.matrix(Xk)%*% as.matrix(x_weights)
    y_ss = 1
    y_scores = (as.matrix(Yk)%*%as.matrix(y_weights)) / y_ss
    x_loadings = (as.vector(x_scores)%*%as.matrix(Xk)) / as.numeric((as.vector(x_scores)%*%as.vector(x_scores)))

    Xk = Xk - outer(as.matrix(x_scores), as.matrix(x_loadings))

    y_loadings = (as.vector(x_scores)%*%as.matrix(Yk)) / as.numeric((as.vector(x_scores)%*%as.vector(x_scores)))
    Yk = Yk - outer(as.matrix(x_scores), as.matrix(y_loadings))

    r2X <- 1 - (sum(apply(Xk,2,var))/ncol(Xk)) # R2 coefficients of X
    r2Y <- 1 - (sum(apply(Yk,2,var))/ncol(Yk)) # R2 coefficients of Y
    r2Var <- 1 - apply(Xk,2,var) # R2 coefficients of variables

    x_weights_[k,] = x_weights
    y_weights_[k,] = y_weights
    x_scores_[,k] = x_scores
    y_scores_[,k] = y_scores
    x_loadings_[k,] = as.vector(x_loadings)
    y_loadings_[k,] = as.vector(y_loadings)
    res_r2X <- c(res_r2X,r2X)
    res_r2Y <- c(res_r2Y,r2Y)
    res_r2Var <- cbind(res_r2Var,r2Var)

  }
  x_weights_ = t(x_weights_)
  y_weights_ = t(y_weights_)
  x_loadings_ = t(x_loadings_)
  y_loadings_ = t(y_loadings_)

  nom <- paste("Comp.",1:n_components,sep = "")

  rownames(x_weights_) = colnames(X)
  colnames(x_weights_) = nom
  rownames(x_loadings_) = colnames(X)
  colnames(x_loadings_) = nom
  rownames(y_weights_) = colnames(Y)
  colnames(y_weights_) = nom
  rownames(y_loadings_) = colnames(Y)
  colnames(y_loadings_) = nom
  rownames(res_r2Var) = colnames(X)
  colnames(res_r2Var) = nom
  colnames(x_scores_) = nom
  colnames(y_scores_) = nom

  x_rotations_ = x_weights_ %*% solve(t(x_loadings_)%*%x_weights_)

  coef_ = x_rotations_ %*% t(y_loadings_)
  coef_ = (coef_ * sd_y)
  colnames(coef_) = colnames(Y)
  rownames(coef_) = colnames(X)
  Intercept = Y_mean - (x_mean %*% coef_)
  rownames(Intercept) = "Intercept"
  classification = rbind(coef_,Intercept)

  return(list("X" = X,"Y" = Y,"classification" = classification,"Coefficients" = coef_, "Intercept" = Intercept,"x_weights"= x_weights_,"y_weights" = y_weights_,"Scores_X"= x_scores_, "Scores_Y" = y_scores_, "Loadings_X" = x_loadings_, "Loadings_Y" = y_loadings_,"R2X"=res_r2X, "R2Y"=res_r2Y,"R2Var" = res_r2Var))

}

