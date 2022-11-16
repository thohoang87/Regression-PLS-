#predict function
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