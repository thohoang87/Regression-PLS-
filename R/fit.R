# fit function:
#' Classification function
#'
#' @param formula  an object of class "formula" (or one that can be coerced to that class): a symbolic description of the model to be fitted
#' @param data     an optional data frame, list or environment (or object coercible by as.data.frame to a data frame) containing the variables in the model
#' @param ncomp    number of componants
#' @param algorithm Choice algorithm between nipals and simpls
#' @param var.select an boolean
#' @param threshold.algorithm
#' @param threshold.vip
#' @param iter.max
#' @param nfold
#'
#' @return
#' @export
#'
#' @examples
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
    select_var <- PlsDA.vip(X=X,Y=Yclass, ncomp=ncomp,algorithm=algorithm,threshold=threshold.vip)
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

lm()
