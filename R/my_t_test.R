#' T.test function
#' 
#' This function performs a one sample t-test.
#' 
#' @param x A numeric vector of data used to find its t-test results.
#' @param alternative A character string specifying the alternative hypothesis,
#'   and it only accepts "two.sided", "less", or "greater".
#' @param mu A number indicating the null hypothesis value of the mean.
#'
#' @return A list with elements test_stat, the numeric test statistic, df, the
#'   degrees of freedom, \code{alternative} and p_val, the numeric p-value.
#'   
#' @example
#' my_t.test(data, less, 2)
#' 
#' @export
my_t_test <- function(x, alternative, mu) {
  # filter the alternative. Limited to specific choice
  if (alternative == "two.sided" | alternative == "less" | 
         alternative == "greater") {
      # calculate the df
      df <- length(x) - 1
      # calculate the standard error
      std_error <- (sd(x) / sqrt(length(x)))
      # calculate the test stat
      test_stat <- ((mean(x) - mu) / std_error)
      
      # calculate the p value according to alternative
      if (alternative == "two.sided") {
        p_val <- pt(abs(test_stat), df, lower.tail = FALSE) * 2
      } else if (alternative == "less") {
        p_val <- pt(test_stat, df, lower.tail = TRUE)
      } else {
        p_val <- pt(test_stat, df, lower.tail = FALSE)
      }
      
      # set all the results into one list
      result <- list("test_stat" = test_stat,
                     "df" = df,
                     "alternative" = alternative,
                     "p_val" = p_val)
      # return the list
      return(result)
  # if the alternative is unacceptable
  } else {
      # throw warning when alternative is unacceptable
      stop("Alternative can only be two.sided, less, or greater!")
  }
}