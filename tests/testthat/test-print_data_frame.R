
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

sample_df4 <- data.frame(
  x = color_vctr(1:4, text_color = "blue"),
  chars = c("AA","AAAA","ABBBAB","kebab"),
  z = "123456789"
)

sample_df5 <- data.frame(
  x = 1:4,
  chars = c("AA","AAAA","ABBBAB","kebab"),
  z = "123456789"
)

sample_df6 <- data.frame(
  x = 1:4,
  chars = color_vctr(
    factor(c("AA","AAAA","ABBBAB","kebab")),
    text_color = "blue"),
  NA_col = NA
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

  expect_equal(
    capture_print(sample_df6, method = "console"),
    c("  x  chars NA_col",
      "1 1     \033[38;5;12mAA\033[0m     NA",
      "2 2   \033[38;5;12mAAAA\033[0m     NA",
      "3 3 \033[38;5;12mABBBAB\033[0m     NA",
      "4 4  \033[38;5;12mkebab\033[0m     NA"
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


test_that("wide data.frames printing correctly jumps down to the next section", {
  expect_equal(
    capture_print(sample_df4, method = "console", width = 15),
    c("  x  chars",
      "1 \033[38;5;12m1\033[0m     AA",
      "2 \033[38;5;12m2\033[0m   AAAA",
      "3 \033[38;5;12m3\033[0m ABBBAB",
      "4 \033[38;5;12m4\033[0m  kebab",
      "          z",
      "1 123456789",
      "2 123456789",
      "3 123456789",
      "4 123456789"
    ))

  expect_equal(
    capture_print(sample_df4, method = "html", width = 15),
    c("  x  chars",
      "1 <span style='color:blue;'>1</span>     AA",
      "2 <span style='color:blue;'>2</span>   AAAA",
      "3 <span style='color:blue;'>3</span> ABBBAB",
      "4 <span style='color:blue;'>4</span>  kebab",
      "          z",
      "1 123456789",
      "2 123456789",
      "3 123456789",
      "4 123456789"
    ))

  expect_equal(
    capture_print(sample_df4, method = "latex", width = 15),
    c("  x  chars",
      "1 \\textcolor[rgb]{0.0,0.0,1.0}{1}     AA",
      "2 \\textcolor[rgb]{0.0,0.0,1.0}{2}   AAAA",
      "3 \\textcolor[rgb]{0.0,0.0,1.0}{3} ABBBAB",
      "4 \\textcolor[rgb]{0.0,0.0,1.0}{4}  kebab",
      "          z",
      "1 123456789",
      "2 123456789",
      "3 123456789",
      "4 123456789"
    ))
})


test_that("wide data.frames printing preserves truncation", {
  expect_equal(
    capture_print(sample_df4, method = "console", width = 15, max = 2),
    c("  x chars",
      "1 \033[38;5;12m1\033[0m    AA",
      "2 \033[38;5;12m2\033[0m  AAAA",
      "          z",
      "1 123456789",
      "2 123456789",
      " [ reached 'max' / getOption(\"max.print\") -- omitted 2 rows ]"

    ))

  expect_equal(
    capture_print(sample_df4, method = "html", width = 15, max = 2),
    c("  x chars",
      "1 <span style='color:blue;'>1</span>    AA",
      "2 <span style='color:blue;'>2</span>  AAAA",
      "          z",
      "1 123456789",
      "2 123456789",
      " [ reached 'max' / getOption(\"max.print\") -- omitted 2 rows ]"

    ))

  expect_equal(
    capture_print(sample_df4, method = "latex", width = 15, max = 2),
    c("  x chars",
      "1 \\textcolor[rgb]{0.0,0.0,1.0}{1}    AA",
      "2 \\textcolor[rgb]{0.0,0.0,1.0}{2}  AAAA",
      "          z",
      "1 123456789",
      "2 123456789",
      " [ reached 'max' / getOption(\"max.print\") -- omitted 2 rows ]"
    ))
})


test_that("data.frames with no rows don't throw errors",{

  expect_equal(
    capture_print(data.frame(x = color_vctr()), method = "console"),
    c("[1] x",
      "<0 rows> (or 0-length row.names)")
  )

})

test_that("data.frames with no color_vectors print using the base method",{

  expect_equal(
    capture_print(data.frame(x = 1), method = "console"),
    capture.output(base::print(data.frame(x = 1)))
  )

})

