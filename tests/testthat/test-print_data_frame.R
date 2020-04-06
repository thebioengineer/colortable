
capture_print <- function(x, method){
  capture.output(print(x, method = method))
}

sample_df <- data.frame(
  x = 1:4,
  c_vctr = color_vctr(c("A","B","C","D"), text_color = "blue")
)

test_that("console data.frame printing is formatted properly", {
  expect_equal(
    capture_print(sample_df, method = "console"),
    c("  x c_vctr",
      "1 1      \033[38;5;12mA\033[0m",
      "2 2      \033[38;5;12mB\033[0m",
      "3 3      \033[38;5;12mC\033[0m",
      "4 4      \033[38;5;12mD\033[0m"
    )
  )
})

test_that("html data.frame printing is formatted properly", {
  expect_equal(
    capture_print(sample_df, method = "html"),
    c("  x c_vctr",
      "1 1      <span style='color:blue;'>A</span>",
      "2 2      <span style='color:blue;'>B</span>",
      "3 3      <span style='color:blue;'>C</span>",
      "4 4      <span style='color:blue;'>D</span>"
    )
  )
})

test_that("latex data.frame printing is formatted properly", {
  expect_equal(
    capture_print(sample_df, method = "latex"),
    c("  x c_vctr",
      "1 1      \\textcolor{blue}{A}",
      "2 2      \\textcolor{blue}{B}",
      "3 3      \\textcolor{blue}{C}",
      "4 4      \\textcolor{blue}{D}"
    )
  )
})
