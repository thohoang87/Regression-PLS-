
#' Function Get Dummies
#'
#' It's a function who take a matrix or dataframe of one qualitative variable and to return variables binarize
#' @param X dataframe
#'
#' @return A dataframe of variables binarize
#' @export
#'
#' @examples
#' data(iris)
#' y = iris[,5]
#' y = get_dummies(y)
#' print(y)
get_dummies = function(y){
    y <- as.factor(y) #transform into factor
    m <- matrix(data=NA,length(y),nlevels(y))
    for (i in (1:length(y))){
      for (j in (1:nlevels(y))){
        m[i,j] <- ifelse(y[i]==levels(y)[j],1,0)
      }
    }
    colnames(m)<-levels(y)
    m = as.data.frame(m)

    return(m)
}
