
base_color_vctr <- color_vctr(c(1,2,3), text_color = "blue",style = "underline")

test_that("knit_print works to html", {

  knit_print_output <- knit_print(base_color_vctr,method = "html")

  expect_s3_class(knit_print_output, "knit_asis")
  expect_equivalent(
    as.character(knit_print_output),
    c("<pre>","<code class = \"hljs\">",
      "<span>## [1] <span style='text-decoration:underline;color:blue;'>1</span> <span style='text-decoration:underline;color:blue;'>2</span> <span style='text-decoration:underline;color:blue;'>3</span> </span><br>",
      "</code>","</pre>")
  )

})

test_that("knit_print works to tex", {

  knit_print_output <- knit_print(base_color_vctr,method = "latex")

  expect_s3_class(knit_print_output, "knit_asis")
  expect_equivalent(
    as.character(knit_print_output),
    c("\\begin{Verbatim}[commandchars=\\\\\\{\\}]\n",
      "## [1] \\underline{\\textcolor{blue}{1}} \\underline{\\textcolor{blue}{2}} \\underline{\\textcolor{blue}{3}}\n",
      "\\end{Verbatim}\n")
  )

})

test_that("knit_print does not work for undefined outputs", {

  expect_error(knit_print(base_color_vctr,method = "undefined"),
               "Method for .* not implemented yet")

})
