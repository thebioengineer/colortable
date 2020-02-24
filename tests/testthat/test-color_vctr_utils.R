test_that("Subsetting the vector results in a smaller color_vctr", {

  example_color_vctr <- color_vctr(
    LETTERS,
    style = c("underline", rep("bold",25)),
    text_color = c("blue","cyan",rep("magenta", 24)),
    background = c(NA, NA, NA, "blue","cyan",rep("red",21))
    )

  subset_color_vctr <- example_color_vctr[1:6]

  expect_equal(
    subset_color_vctr,
    color_vctr(
      LETTERS[1:6],
      style = c("underline", rep("bold",5)),
      text_color = c("blue","cyan",rep("magenta", 4)),
      background = c(NA, NA, NA, "blue","cyan","red")
    )
  )

})

test_that("Subsetting outside the range of the vector results NA's", {
  example_color_vctr <- color_vctr(
    1,
    style = "underline",
    text_color = "blue"
  )

  subset_color_vctr <- example_color_vctr[1:3]

  expect_equal(
    subset_color_vctr,
    color_vctr(
      c(1,NA,NA),
      style = c("underline", NA, NA),
      text_color = c("blue", NA, NA)
    )
  )
})

test_that("Assignment of a color_vctr preserves the styling", {
  example_color_vctr <- color_vctr(
    1,
    style = "underline",
    text_color = "blue"
  )

  example_color_vctr[2] <- color_vctr(
    2,
    style = "strikethrough",
    text_color = "magenta"
  )

  expect_equal(
    example_color_vctr,
    color_vctr(
      c(1,2),
      style = c("underline", "strikethrough"),
      text_color = c("blue", "magenta")
    )
  )
})

test_that("Assignment of a color_vctr fills with NA when necessary", {
  example_color_vctr <- color_vctr(
    1,
    style = "underline",
    text_color = "blue"
  )

  example_color_vctr[4] <- color_vctr(
    2,
    style = "strikethrough",
    text_color = "magenta"
  )

  expect_equal(
    example_color_vctr,
    color_vctr(
      c(1,NA, NA, 2),
      style = c("underline", NA, NA, "strikethrough"),
      text_color = c("blue", NA, NA, "magenta")
    )
  )
})

test_that("Assignment of a color_vctr can be with matching atomics as well", {
  example_color_vctr <- color_vctr(
    1,
    style = "underline",
    text_color = "blue"
  )

  example_color_vctr[2] <- 2

  expect_equal(
    example_color_vctr,
    color_vctr(
      c(1, 2),
      style = c("underline", NA ),
      text_color = c("blue", NA )
    )
  )

})


test_that("If assignment of a color_vctr is larger then vector being added, it will use a subset of the input data", {
  example_color_vctr <- color_vctr(
    1,
    style = "underline",
    text_color = "blue"
  )

  expect_warning({
    example_color_vctr[2] <- color_vctr(
      2,3,
      style = "strikethrough",
      text_color = "magenta"
    )},
    "number of items to replace is not a multiple of replacement length")

  expect_equal(
    example_color_vctr,
    color_vctr(
      c(1,2),
      style = c("underline", "strikethrough"),
      text_color = c("blue", "magenta")
    )
  )
})
