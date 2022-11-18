#Function with NIPALS's algorithm
#' Title
#'
#' @param X
#' @param Y
#' @param ncomp
#' @param threshold
#' @param iter.max
#'
#' @return
#' @export
#'
#' @examples
NIPALS <- function(X,Y, ncomp = NULL, threshold = 0.001, iter.max = 100){
  # verify data: dataframe or matrix
  ok <- (is.data.frame(X) | is.matrix(X)) & (is.data.frame(Y) | is.matrix(Y))
  if (!ok){
    stop("X and Y should be a dataframe or a matrix")
  }

  # verify data: numeric
  nb_ok<- sum(sapply(X,function(x){is.numeric(x)})) + sum(sapply(Y,function(x){is.numeric(x)}))
  if (nb_ok < (ncol(X) + ncol(Y))){
    stop("Some of the elements inside X or Y are not numeric")
  }

  # check the number of threshold
  if (is.null(threshold)){
    threshold <- 0.001
  }

  res_w <- c() # list of x-weights
  res_t <- c() # list of x-scores
  res_p <- c() # list of x-loadings
  res_c <- c() # list of y-loadings
  res_u <- c() # list of y-scores
  res_r2X <- c() # R2 coefficients of X
  res_r2Y <- c() # R2 coefficients of Y
  res_r2Var <- c() # R2 coefficients of variables

  #scaling X and Y
  Xscale <- as.matrix(scale(X))
  Yscale <- as.matrix(scale(Y))

  # columns' number of Y:
  q <- ncol(Y)

  #Loop to define weights, scores, loadings and regression coefficient
  for (i in 1:ncomp){ #begin loop for
    b <- TRUE
    iter<-1
    w_old = rep(1, ncol(X))
    u <- Yscale[,1] #initialization on the first column of Y

    while (b){ #begin loop while
      w <- (t(u) %*% Xscale)/as.numeric(t(u) %*% u) # x-weights
      w <- t(w/sqrt(as.numeric(w %*% t(w))))        # normalisation
      t <- Xscale %*% w                             # X_scores
      c <- t((t(t) %*% Yscale)/as.numeric(t(t) %*% t)) # loadings of Y
      u <- Yscale %*% c/as.numeric(t(c) %*% c)   # Y_scores

      # verify if w changes or not:
      w_dif <- w - w_old
      w_old <- w
      # condition to stop the loop: if w doesn't change, we stop
      ifelse(sum(w_dif^2)<threshold || iter==100, b <- FALSE, b <- TRUE)
      iter <- iter + 1
    } # end loop while

    #residual calculation on component i, then iteration on the remaining X and Y
    p <- t(t(t) %*% Xscale/(as.numeric(t(t) %*% t))) # X_loadings

    Xscale <- Xscale - t %*% t(p)                    # residuals of X on t
    Yscale <- Yscale - t %*% t(c)                    # residuals of Y on t
    r2X <- 1 - (sum(apply(Xscale,2,var))/ncol(Xscale)) # R2 coefficients of X
    r2Y <- 1 - (sum(apply(Yscale,2,var))/ncol(Yscale)) # R2 coefficients of Y
    r2Var <- 1 - apply(Xscale,2,var) # R2 coefficients of variables

    #store elements
    res_w <- cbind(res_w,w)
    res_t <- cbind(res_t,t)
    res_p <- cbind(res_p,p)
    res_c <- cbind(res_c,c)
    res_u <- cbind(res_u,u)
    res_r2X <- c(res_r2X,r2X)
    res_r2Y <- c(res_r2Y,r2Y)
    res_r2Var <- cbind(res_r2Var,r2Var)

    nom <- paste("Comp.",i,sep = "")
    colnames(res_w)[i]=nom
    colnames(res_t)[i]=nom
    colnames(res_c)[i]=nom
    colnames(res_p)[i]=nom
    colnames(res_u)[i]=nom
    colnames(res_r2Var)[i]=nom
  } # end loop for
  rownames(res_w) <- colnames(X)

  #calculation of std coefficients:
  wstar <- res_w %*% solve(t(res_p)%*%res_w) # modified weights of X
  Bh <- wstar %*% t(res_c) # std beta coeffs


  # regular coeffs
  if (q==1){
    # beta coeffs
    Bh <- as.vector(Bh)
    Br <- Bh * rep(apply(Y,2,sd),ncol(X))
    # intercept
    intercept <- as.vector(colMeans(Y) - Br %*% apply(X, 2, mean))
  } else{
    # beta coeffs
    Br <- Bh %*% diag(apply(Y,2,sd)) # regular coefficients
    # intercept
    intercept <- as.vector(apply(Y,2,mean) - apply(X,2,mean) %*%Br)
  }

  if (q==1){
    rownames(Y) <- rownames(X)
    rownames(res_u) <- rownames(X)
    rownames(residus) <- rownames(X)
  } else{
    colnames(Br) <- colnames(Bh)
    rownames(Br) <- rownames(Bh)
    rownames(Bh) <- colnames(X)
  }

  return(list(X=X,
              Y=Y,
              n_components=ncomp,
              Coefficients=Br,
              Intercept=intercept,
              x_weights=res_w,
              Loadings_X=res_p,
              Scores_X=res_t,
              y_weights=res_c,
              Loadings_Y=res_c,
              Scores_Y=res_u,
              R2X=res_r2X,
              R2Y=res_r2Y,
              R2Var=res_r2Var))
} # end of function NIPALS
