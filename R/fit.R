# fit function:
#' Classification function
#'
#' @param formula  an object of class "formula" (or one that can be coerced to that class): a symbolic description of the model to be fitted
#' @param data     an optional data frame, list or environment (or object coercible by as.data.frame to a data frame) containing the variables in the model
#' @param ncomp    number of componants
#' @param algorithm Choice algorithm between nipals and simpls
#' @param var.select an boolean
#' @param threshold.algorithm an numeric number
#' @param threshold.vip an integer number
#' @param iter.max an integer number
#' @param nfold number of folder, initialise NULL
#'
#'Partial least squares regression (PLS regression) is a statistical method that bears some relation to principal components regression; instead of finding hyperplanes of maximum variance between the response and independent variables, it finds a linear regression model by projecting the predicted variables and the observable variables to a new space
#'
#' @return
#' X : an dataframe
#'
#' Y : an dataframe
#'
#' algorithm : algorithm nipals ou simpls
#'
#' n_components : number of componant
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
#' select_var : list of variable selected
#'
#' @export
#'
#' @examples
#' data(iris)
#' fit(Species~., iris, ncomp = 2, algorithm = "simpls")
#'
fit = function(formula, data, ncomp=NULL, algorithm="simpls",var.select = F,threshold.algorithm = 0.001, threshold.vip=1,iter.max = 100,nfold=NULL){

  #check dataframe
  ok <- is.data.frame(data)
  if (!ok) {stop("This is not a dataframe")}

  #get the names of variables
  ylabel = toString(formula[[2]])
  xlabels = attributes(terms(formula, data = data))$term.labels

  #definition of X and Y
  Y = data[,ylabel]
  Y = sapply(Y, as.character)
  X = data[,xlabels]

  #transform to get_dummies
  Yclass = get_dummies(Y)

  # verify data: numeric
  nb_ok<- sum(sapply(X,function(x){is.numeric(x)}))
  if (nb_ok < ncol(X)){
    stop("Some of the elements inside X are not numeric")
  }

  # Check the number of components
  if (is.null(ncomp) || ncomp <= 0) {
    temp_var <- PlsDA.cv(X=X,Y=Yclass,threshold = threshold.algorithm, nfold=nfold)
    ncomp <- temp_var$ncomp
  }
  if (ncomp > ncol(X)) {
    cat("Number of components reduced to : ",ncol(X),"\n")
    ncomp <- ncol(X)
  }

  # selection of algorithm
  if (algorithm == "nipals"){
    temp = NIPALS(X=X,Y=Yclass,ncomp=ncomp,threshold=threshold.algorithm,iter.max=iter.max)
  }
  else{
    temp = SIMPLS(X=X,Y=Yclass,n_components=ncomp)
  }

  # check variables selection:
  if (var.select ==TRUE) {
    select_var <- PlsDAvip(X=X,Y=Yclass, ncomp=ncomp,algorithm=algorithm,threshold=threshold.vip)
  } else {
    select_var <- NULL
  }

  # S3 model
  instance <- list()
  instance$X <- X
  instance$Y <- Y
  instance$algorithm <- algorithm
  instance$n_components <- ncomp
  instance$calc <- temp
  instance$VIP <- select_var
  class(instance) <- "PLSDA"
  return(instance)

}
