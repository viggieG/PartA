library(PartA)
set.seed(1)

result <- my_lm(lifeExp ~ gdpPercap + continent, my_gapminder)

test_that("multiplication works", {
  expect_equal(my_lm(lifeExp ~ gdpPercap + continent, my_gapminder), result)
})
