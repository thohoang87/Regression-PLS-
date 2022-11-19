#predict function
#' Prediction features
#'
#' Function who atribute  class for individu with softmax method on the new new dataframe.
#'
#' @param object An object of class Pls-DA
#' @param newdata New dataframe for predict class
#' @param type choose if you want probabilites or class
#'
#' @return
#' Class : clusters of individu
#' Probability : The Probability to for the individu to go in the different class
#' @export
#'
#' @examples
#' fit_launch : an object of class Pls-DA
#' predict(fit_launch, iris[,1:4], type = "class")
#'
predict <- function(object,newdata,type = c("posterior", "class")){
  temp <- softmax(object=object,newdata=newdata)
  if (type=="class"){
    cat("Predicted class : ","\n")
    print(temp$Clusters)
  } else {
    cat("Probability of classification by observations and class : ","\n")
    print(temp$Probability)
  }
  return(list(Class=temp$Clusters,Probability=temp$Probability))
}
