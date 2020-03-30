test_that("format appends ansi styling based on console output", {

  c_vctr <- color_vctr(1, text_color = "blue", background = "yellow", style = "underline")
  console_output <- format(c_vctr, method = "console")

  expect_equal(
    class(console_output),
    c("color_vctr_output","character")
  )
  expect_equal(
    as.character(console_output),
    "\033[38;5;12m\033[48;5;11m\033[4m1\033[24m\033[0m\033[0m"
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

test_that("Invalid styling silently returns the values", {

  c_vctr <- color_vctr(1, text_color = "invalid", background = "invalid", style = "invalid")

  expect_equivalent(
    format(c_vctr, method = "console"),
    structure("1",class = c("color_vctr_output", "character"))
  )

  expect_equivalent(
    format(c_vctr, method = "html"),
    structure("<span style=''>1</span>",class = c("color_vctr_output", "character"))
  )

  expect_equivalent(
    format(c_vctr, method = "latex"),
    structure("1",class = c("color_vctr_output", "character"))
  )

})

