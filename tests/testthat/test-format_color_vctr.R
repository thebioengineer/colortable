test_that("format appends ansi styling based on console output", {

  c_vctr <- color_vctr(1, text_color = "blue", background = "yellow", style = "underline")
  console_output <- format(c_vctr, method = "console")

  expect_equal(
    class(console_output),
    c("character")
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
    c("character")
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
    c("character")
  )
  expect_equal(
    as.character(tex_output),
    "\\colorbox[rgb]{1.0,1.0,0.0}{\\underline{\\textcolor[rgb]{0.0,0.0,1.0}{1}}}"
  )

})

test_that("Invalid styling silently returns the values", {

  c_vctr <- color_vctr(1, text_color = "invalid", background = "invalid", style = "invalid")

  expect_equivalent(
    format(c_vctr, method = "console"),
    "1"
  )

  expect_equivalent(
    format(c_vctr, method = "html"),
    "<span style=''>1</span>"
  )

  expect_equivalent(
    format(c_vctr, method = "latex"),
    "1"
  )

})


test_that("NA styling silently returns the values", {

  c_vctr <- color_vctr(1, text_color = NA, background = NA, style = NA)

  expect_equivalent(
    format(c_vctr, method = "console"),
    "1"
  )

  expect_equivalent(
    format(c_vctr, method = "html"),
    "<span style=''>1</span>"
  )

  expect_equivalent(
    format(c_vctr, method = "latex"),
    "1"
  )

})

test_that("Hex-code coloring works", {
  c_vctr <- color_vctr(1, text_color = "#000000", background = NA, style = NA)

  expect_equivalent(
    format(c_vctr, method = "console"),
    "\033[38;5;0m1\033[0m"
  )

  expect_equivalent(
    format(c_vctr, method = "html"),
    "<span style='color:#000000;'>1</span>"
  )

  expect_equivalent(
    format(c_vctr, method = "latex"),
    "\\textcolor[rgb]{0.0,0.0,0.0}{1}"
  )
})

test_that("colors not existing within the respective print will be identified ", {
  #tiffanyblue is not a named color for console or html
  c_vctr_console_html <- color_vctr(1, text_color = "tiffanyblue", background = NA, style = NA)

  #blueviolet is not a named color for latex
  c_vctr_latex <- color_vctr(1, text_color = "blueviolet", background = NA, style = NA)

  expect_equivalent(
    format(c_vctr_console_html, method = "console"),
    "\033[38;5;37m1\033[0m"
  )

  expect_equivalent(
    format(c_vctr_console_html, method = "html"),
    "<span style='color:#0ABAB5;'>1</span>"
  )

  expect_equivalent(
    format(c_vctr_latex, method = "latex"),
    "\\textcolor[rgb]{0.54,0.17,0.89}{1}"
  )
})

test_that("input colors can be functions", {
  #tiffanyblue is not a named color for console or html
  c_vctr <-
    color_vctr(
      1,
      text_color = color_scale("Blues"),
      background = color_scale("Blues"),
      style = NA
    )

  expect_equivalent(
    format(c_vctr, method = "console"),
    "\033[38;5;74m\033[48;5;74m1\033[0m\033[0m"
  )

  expect_equivalent(
    format(c_vctr, method = "html"),
    "<span style='color:#6BAED6;background:#6BAED6;'>1</span>"
  )

  expect_equivalent(
    format(c_vctr, method = "latex"),
    "\\colorbox[rgb]{0.44,0.65,0.82}{\\textcolor[rgb]{0.44,0.65,0.82}{1}}"
  )
})


test_that("NA's are left as NA's", {
  #tiffanyblue is not a named color for console or html
  c_vctr <-
    color_vctr(
      NA,
      text_color = "blue",
      background = "green",
      style = "italic"
    )

  expect_equivalent(
    format(c_vctr, method = "console"),
    NA
  )

  expect_equivalent(
    format(c_vctr, method = "html"),
    NA
  )

  expect_equivalent(
    format(c_vctr, method = "latex"),
    NA
  )
})
