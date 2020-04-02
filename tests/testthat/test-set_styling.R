test_that("set_styling works on basic vectors", {

  c_vctr <- color_vctr(1:5, text_color = "green")
  c_vctr_styled <- set_styling(1:5, text_color ="green")
  c_vctr_conditioned <- set_styling(1:5, 1:5 > 4, text_color = "green")

  expect_equal(c_vctr, c_vctr_styled)
  expect_false(identical(c_vctr_styled, c_vctr_conditioned))

})

test_that("set_styling updates color_vctr styling", {

  c_vctr <- color_vctr(1:5, text_color = "green")
  c_vctr_conditioned <- set_styling(c_vctr, 1:5 > 4, text_color = "red")
  c_vctr_idxed <- set_styling(c_vctr, 5, text_color = "red")

  expect_false(identical(c_vctr, c_vctr_conditioned))
  expect_equal(c_vctr_conditioned,c_vctr_idxed)

})

test_that("set_styling throw error when idx out of range", {

  expect_error(set_styling(1:5, rep(TRUE,6), text_color = "red"),
               "Length of index must be same as input vector")

  expect_error(set_styling(1:5, 12, text_color = "red"),
               "Indexes out of Range")

})

test_that("set_styling throw error when idx is not numeric/logical", {

  expect_error(set_styling(1:5, c("TRUE"), text_color = "red"),
               "Invalid Index. Must be of type Numeric or Logical")

})


test_that("set_styling throw warning when idx duplicated", {

  expect_warning(set_styling(1:5, c(1,1), text_color = "red"),
               "Duplicated indexes provided")

})
