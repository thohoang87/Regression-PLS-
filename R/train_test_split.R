
#Train, test split datafrme function
#' train_test_split
#'
#'Split dataframe into Train and test dataset.
#'
#' @param dataframe A dataframe
#'
#' @return
#' Dataframe : A dataframe
#' Df_Train  : Dataframe split into train dataset
#' Df_Test   : Dataframe split into test dataset
#'
#' @export
#'
#' @examples
#' data(iris)
#' train_test_split(iris)
#'
train_test_split = function(dataframe){

  #verification
  if(is.data.frame(dataframe) == F){
    stop("Is not a dataframe")
  }

  #Split dataset
  sample <- sample(c(TRUE, FALSE), nrow(dataframe), replace=TRUE, prob=c(0.7,0.3))
  train  <- dataframe[sample, ]
  test   <- dataframe[!sample, ]

  return(list("Dataframe" = dataframe, "Df_Train" = train, "Df_Test" = test))
}
