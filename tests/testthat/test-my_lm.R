test_that("multiplication works", {
  expect_equal(my_lm(lifeExp ~ gdpPercap + continent, my_gapminder))
})
