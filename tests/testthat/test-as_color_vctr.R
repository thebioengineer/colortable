test_that("as_color_vctrs can be based on atomics or a color_vctr already", {
  char <- LETTERS
  num <- 1:26
  int <- as.integer(num)
  logic <- logical(26)
  c_vctr <- color_vctr(1)

  expect_false(inherits(try(as_color_vctr(char)),"try-error"))
  expect_false(inherits(try(as_color_vctr(num)),"try-error"))
  expect_false(inherits(try(as_color_vctr(int)),"try-error"))
  expect_false(inherits(try(as_color_vctr(logic)),"try-error"))
  expect_false(inherits(try(as_color_vctr(c_vctr)),"try-error"))
})

test_that("as_color_vctrs throws an error when it is not an atomic", {

  expect_error(as_color_vctr(list(45)),"cannot coerce")
  expect_error(as_color_vctr(data.frame(x=45)),"cannot coerce")

})

