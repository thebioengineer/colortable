test_that("format appends ansi styling based on console output", {

  c_vctr <- color_vctr(1, text_color = "blue", background = "yellow", style = "underline")
  console_output <- format(c_vctr, method = "console")

  expect_equal(
    class(console_output),
    c("color_vctr_output","character")
  )
  expect_equal(
    as.character(console_output),
    "\033[34m\033[43m\033[4m1\033[24m\033[49m\033[39m"
  )

})

test_that("format appends html styling based on html output", {

  c_vctr <- color_vctr(1, text_color = "blue", background = "yellow", style = "underline")
  html_output <- format(c_vctr, method = "html")

  expect_equal(
    class(html_output),
    c("color_vctr_output", "character")
  )

  expect_equal(
    as.character(html_output),
    "<span style='text-decoration:underline;color:blue;background:yellow;'>1</span>"
  )
})

test_that("format appends tex styling based on latex output", {

  c_vctr <- color_vctr(1, text_color = "blue", background = "yellow", style = "underline")
  tex_output <- format(c_vctr, method = "latex")

  expect_equal(
    class(tex_output),
    c("color_vctr_output","character")
  )
  expect_equal(
    as.character(tex_output),
    "\\colorbox{yellow}{\\underline{\\textcolor{blue}{1}}}"
  )

})

test_that("Invalid styling return warnings", {

  c_vctr <- color_vctr(1, text_color = "invalid", background = "invalid", style = "invalid")

  expect_warning(
    format(c_vctr, method = "console"),
    "has not been implemented"
  )

  expect_warning(
    format(c_vctr, method = "html"),
    "has not been implemented"
  )

  expect_warning(
    format(c_vctr, method = "latex"),
    "has not been implemented"
  )

})

