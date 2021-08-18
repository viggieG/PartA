#' Linear model function
#' 
#' This function fits a linear model.
#' 
#' @param formula A formula class object : a symbolic description of the model 
#'   to be fitted.
#' @param data Input data frame.
#'
#' @return A coefficient table with rows for each coefficient (including the 
#'   (Intercept)!) and columns for the Estimate, Std. Error, t value, and 
#'   Pr(>|t|).
#'   
#' @examples
#' my_lm(y ~ x ^ 2, dataset)
#' my_lm(y ~ x, dataset)
#' 
#' @export
my_lm <- function(formula, data) {
  # to extract a model frame object
  dt_frame <- model.frame(formula, data) 
  # to extract the model matrix
  x <- model.matrix(formula, data) 
  # to extract the model response
  y <- model.response(dt_frame)
  
  # calculate the estimate
  esti <- solve(t(x) %*% x) %*% t(x) %*% y
  # calculate the df
  df <- nrow(x) - ncol(x)
  # calculate the variance
  va <- sum((y - x %*% esti)^ 2 / df)
  # calculate the standard error
  std_err <- sqrt(diag(va * solve(t(x) %*% x)))
  # calculate the t value
  t_val <- esti / std_err
  # calculate the p value
  p_val <- pt(abs(t_val), df, lower.tail = FALSE) * 2
  
  # construct a frame to store the values
  tab <- data_frame("Estimate" = esti,
                    "Std. Error" = std_err,
                    "t value" = t_val,
                    "Pr(>|t|)" = p_val)
  # make the frame into a matrix
  tab <- data.matrix(tab)
  # set the names for matrix rows
  rownames(tab) <- colnames(x)
  # return result matrix
  return(tab)
}