library(PartA)
set.seed(1)

result <- my_t_test(my_gapminder$lifeExp, "less", 60)

test_that("application works", {
  expect_equal(my_t_test(my_gapminder$lifeExp, "less", 60), result)
})

test_that("error works", {
  expect_error(my_t_test(my_gapminder$lifeExp, "bigger", 60))
})
