test_that("`valid_text_color()` returns the list of valid text coloring", {

  expect_equal(
    valid_text_color(method = "console"),
    color_key$Name
  )
  expect_equal(
    valid_text_color(method = "html"),
    c("black", "red", "green", "yellow", "blue", "magenta", "cyan", "white", "silver")
  )
  expect_equal(
    valid_text_color(method = "latex"),
    c("black", "red", "green", "yellow", "blue", "magenta", "cyan", "white", "silver")
  )

})

test_that("`valid_background()` returns the list of valid background coloring", {

  expect_equal(
    valid_background(method = "console"),
    color_key$Name
  )
  expect_equal(
    valid_background(method = "html"),
    c("black", "red", "green", "yellow", "blue", "magenta", "cyan", "white")
  )
  expect_equal(
    valid_background(method = "latex"),
    c("black", "red", "green", "yellow", "blue", "magenta", "cyan", "white")
  )
})

test_that("`valid_style()` returns the list of valid styling options", {

  expect_equal(
    valid_style(method = "console"),
    c("bold", "italic", "underline", "inverse", "hidden", "strikethrough")
  )
  expect_equal(
    valid_style(method = "html"),
    c("bold", "italic", "underline", "inverse", "hidden", "strikethrough")
  )
  expect_equal(
    valid_style(method = "latex"),
    c("bold", "italic", "underline", "strikethrough")
  )
})
