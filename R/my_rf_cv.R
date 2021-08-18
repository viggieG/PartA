#' Random forest cross-validation function
#' 
#' This function will predict body_mass_g by building and using a random forest 
#'   cross-validation model, which uses covariates bill_length_mm, 
#'   bill_depth_mm, and flipper_length_mm.
#' 
#' @param k Integer representing the number of folds.
#'
#' @return A numeric with the cross-validation error.
#'   
#' @examples
#' my_rf_cv(5)
#' my_rf_cv(10)
#' 
#' @export
my_rf_cv <- function(k) {
  # create variable to store
  diff <- 0
  # Split data in k parts, randomly
  inds <- sample(rep(1:k, length = nrow(penguins_clean)))
  # combine the inds into data
  penguins_clean$inds <- inds
  # go through all the data
  for (i in 1:k) {
    # filter the data used to train
    data_train <- penguins_clean %>% 
      filter(inds != i)
    # filter the data used to test
    data_test <-  penguins_clean %>% 
      filter(inds == i)
    # build the predicting model
    model <- randomForest(body_mass_g ~ bill_length_mm + bill_depth_mm  + flipper_length_mm, data = data_train, ntree = 100)
    # predict the object
    pred <- predict(model, data_test[, -1])
    # store the MSE
    diff <- append(diff, mean((pred - penguins_clean$body_mass_g) ^ 2))
  }
  # return the MSE
  return(diff[-1])
}