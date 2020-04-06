
capture_print <- function(x, method){
  capture.output(print(x,console_width = 80, method = method))
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
