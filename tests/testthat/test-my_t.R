test_that("application works", {
  expect_equal(my_t.test(my_gapminder$lifeExp, "less", 60))
})

test_that("error works", {
  expect_equal(my_t.test(my_gapminder$lifeExp, "bigger", 60))
})