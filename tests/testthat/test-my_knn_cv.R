test_that("multiplication works", {
  penguins_clean <- drop_na(my_penguins)
  penguins_train <- penguins_clean %>%     
    select(bill_length_mm,bill_depth_mm,flipper_length_mm,body_mass_g)
  expect_equal(my_knn_cv(penguins_train, penguins_clean$species, 10, 5))
})
