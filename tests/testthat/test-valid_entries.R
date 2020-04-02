test_that("`valid_colors()` returns the list of valid text coloring", {

  expect_equal(
    valid_colors(method = "console"),
    setNames(color_key_console[c("Name","hex")],c("Color Name","Hex Code"))
  )

  expect_equal(
    valid_colors(method = "html"),
    setNames(color_key_html[c("Name","hex")],c("Color Name","Hex Code"))
  )
  expect_equal(
    valid_colors(method = "latex"),
    setNames(color_key_latex[c("Name","hex")],c("Color Name","Hex Code"))
  )

})


test_that("`valid_style()` returns the list of valid styling options", {

  expect_equal(
    valid_style(method = "console"),
    c("bold", "italic", "underline", "inverse", "hidden", "strikethrough")
  )
  expect_equal(
    valid_style(method = "html"),
    c("bold", "italic", "underline", "inverse", "hidden", "strikethrough","outline")
  )
  expect_equal(
    valid_style(method = "latex"),
    c("bold", "italic", "underline", "strikethrough","outline")
  )
})
