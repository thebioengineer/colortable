test_that("New color vectors by default have no styling", {
  new_vect <- color_vctr(1)
  expect_equal(field(new_vect,".text_color"), NA_character_)
  expect_equal(field(new_vect,".background"), NA_character_)
  expect_equal(field(new_vect,".style"), NA_character_)
})

test_that("New color vectors can be of any length", {
  expect_false(inherits(try(color_vctr(1)),"try-error"))
  expect_false(inherits(try(color_vctr(runif(20))), "try-error"))
})


test_that("New color vectors can only be based on an atomic vector or another color vector", {
  expect_error(color_vctr(list(45,45,54)))
  expect_error(color_vctr(data.frame(x = c(1,2,3))))

})


test_that("New color vectors can be built from other color vectors", {
  expect_equal(
    vctrs::vec_c(color_vctr(1), color_vctr(2)),
    color_vctr(c(1,2))
  )

  expect_equal(
    vctrs::vec_c(color_vctr(c(1,2,3)), color_vctr(4)),
    color_vctr(c(1,2,3,4))
  )
})

test_that("Text_color provided must be either length 1 or length of input vector",{

  expect_false(
    inherits(try(color_vctr(1, text_color = "blue")),"try-error")
  )

  expect_false(
    inherits(try(color_vctr(c(1,2,3,4), text_color = "blue")),"try-error")
  )

  expect_false(
    inherits(try(color_vctr(c(1,2,3,4), text_color = c("blue","yellow","magenta","cyan"))),"try-error")
  )

  expect_error(
    color_vctr(1,2,3,4, text_color = c("blue","yellow","magenta"))
  )

})

test_that("Background provided must be either length 1 or length of input vector",{

  expect_false(
    inherits(try(color_vctr(1, background = "blue")),"try-error")
  )

  expect_false(
    inherits(try(color_vctr(c(1,2,3,4), background = "blue")),"try-error")
  )

  expect_false(
    inherits(try(color_vctr(c(1,2,3,4), background = c("blue","yellow","magenta","cyan"))),"try-error")
  )

  expect_error(
    color_vctr(c(1,2,3,4), background = c("blue","yellow","magenta"))
  )

})

test_that("Style provided must be either length 1 or length of input vector",{

  expect_false(
    inherits(try(color_vctr(1, style = "underline")),"try-error")
  )

  expect_false(
    inherits(try(color_vctr(c(1,2,3,4), style = "underline")),"try-error")
  )

  expect_false(
    inherits(try(color_vctr(c(1,2,3,4), background = c("underline","invert","strikethrough","bold"))),"try-error")
  )

  expect_error(
    color_vctr(c(1,2,3,4), style = c("underline","invert","strikethrough"))
  )

})
