#' K-nearest neighbors cross-validation predicting function
#' 
#' This function builds a k-nearest neighbors cross-validation predicting model.
#' 
#' @param train Input data frame.
#' @param cl True class value of the training data.
#' @param k_nn Integer representing the number of neighbors.
#' @param k_cv Integer representing the number of folds.
#'
#' @return A list with objects : class, a vector of the predicted class Ŷ i for
#'   all observations, and cv_err, a numeric with the cross-validation 
#'   misclassification error.
#'   
#' @examples
#' my_knn_cv(data, cl, 5, 5)
#' my_knn_cv(data, cl, 1, 10)
#' 
#' @export
my_knn_cv <- function(train, cl, k_nn, k_cv) {
  # Split data in k_cv parts, randomly
  inds <- sample(rep(1:k_cv, length = nrow(train)))
  # combine the inds into data
  train$inds <- inds
  # create variables to store
  class <- 0
  cv_err <- 0
  # go through all the data
  for (i in 1 : k_cv) {
    # filter the data used to train
    data_train <- train %>% 
      dplyr::filter(inds != i)
    # filter the data used to test
    data_test <- train %>% 
      dplyr::filter(inds == i)
    # create the cl
    cl <- sample(cl, nrow(data_train), replace = TRUE)
    # make predictions
    pred <- class::knn(data_train, data_test, cl, k = k_nn)
    # store the predictions
    class <- append(class, pred)
    # store the cv errors
    cv_err <- append(cv_err, mean(pred != cl))
  }
  # combine class and error these two outputs
  output <- list(class[-1], cv_err[-1])
  # return the results
  return(output)
}