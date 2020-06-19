
capture_print <- function(x, method, ...){
  capture.output(print(x, method = method, ...))
}

sample_df <- data.frame(
  x = 1:4,
  c_vctr = color_vctr(c("A","B","C","D"), text_color = "blue")
)

sample_df2 <- data.frame(
  x = 1:4,
  c_vctr = color_vctr(8:11, text_color = "blue")
)

sample_df3 <- data.frame(
  x = color_vctr(8:11, text_color = "blue"),
  chars = c("AA","AAAA","ABBBAB","kebab")
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

  expect_equal(
    capture_print(sample_df2, method = "console"),
    c("  x c_vctr",
      "1 1     \033[38;5;12m 8\033[0m",
      "2 2     \033[38;5;12m 9\033[0m",
      "3 3     \033[38;5;12m10\033[0m",
      "4 4     \033[38;5;12m11\033[0m"
    )
  )

  expect_equal(
    capture_print(sample_df3, method = "console"),
    c("   x  chars",
      "1 \033[38;5;12m 8\033[0m     AA",
      "2 \033[38;5;12m 9\033[0m   AAAA",
      "3 \033[38;5;12m10\033[0m ABBBAB",
      "4 \033[38;5;12m11\033[0m  kebab"
    )
  )

})

test_that("console data.frame printing is longer than max truncates", {
  expect_equal(
    capture_print(sample_df, method = "console", max = 2),
    c("  x c_vctr",
      "1 1      \033[38;5;12mA\033[0m",
      "2 2      \033[38;5;12mB\033[0m",
      " [ reached 'max' / getOption(\"max.print\") -- omitted 2 rows ]"
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
      "1 1      \\textcolor[rgb]{0.0,0.0,1.0}{A}",
      "2 2      \\textcolor[rgb]{0.0,0.0,1.0}{B}",
      "3 3      \\textcolor[rgb]{0.0,0.0,1.0}{C}",
      "4 4      \\textcolor[rgb]{0.0,0.0,1.0}{D}"
    )  )
})


