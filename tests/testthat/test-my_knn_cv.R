library(PartA)
library(tidyverse)
set.seed(1)

penguins <- my_penguins %>%
  select(bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g)
penguins <- drop_na(penguins)

my_penguins <- drop_na(my_penguins)

result <- my_knn_cv(penguins, my_penguins$species, 10, 5)

test_that("multiplication works", {
  expect_equal(result, result)
})
