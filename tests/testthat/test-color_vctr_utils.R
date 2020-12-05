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
