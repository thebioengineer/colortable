
base_color_vctr <- color_vctr(c(1,2,3), text_color = "blue",style = "underline")

test_that("knit_print works to html", {

  knit_print_output <- knit_print(base_color_vctr, method = "html")
  knit_print_output_inline <- knit_print(base_color_vctr,method = "html",inline = TRUE)


  expect_s3_class(knit_print_output, "knit_asis")
  expect_equivalent(
    as.character(knit_print_output),
    c("<pre>","<code class = \"hljs\">",
      "<div class='remark-code-line'><span>## [1] <span style='text-decoration:underline;color:blue;'>1</span> <span style='text-decoration:underline;color:blue;'>2</span> <span style='text-decoration:underline;color:blue;'>3</span> </span></div>",
      "</code>","</pre>")
  )
  expect_equivalent(class(knit_print_output_inline), "character")
  expect_equivalent(
    as.character(knit_print_output_inline),
    c("<span style='text-decoration:underline;color:blue;'>1</span>",
      "<span style='text-decoration:underline;color:blue;'>2</span>",
      "<span style='text-decoration:underline;color:blue;'>3</span>")
  )

})

test_that("knit_print works to tex", {

  knit_print_output <- knit_print(base_color_vctr,method = "latex")
  knit_print_output_inline <- knit_print(base_color_vctr,method = "latex",inline = TRUE)

  expect_s3_class(knit_print_output, "knit_asis")
  expect_equivalent(
    as.character(knit_print_output),
    c("\\texttt{\\#\\#\\ [1]\\ \\underline{\\textcolor[rgb]{0.0,0.0,1.0}{1}}\\ \\underline{\\textcolor[rgb]{0.0,0.0,1.0}{2}}\\ \\underline{\\textcolor[rgb]{0.0,0.0,1.0}{3}}}\\newline\n")
  )

  expect_equivalent(class(knit_print_output_inline), "character")
  expect_equivalent(
    as.character(knit_print_output_inline),
    c("\\underline{\\textcolor[rgb]{0.0,0.0,1.0}{1}}",
      "\\underline{\\textcolor[rgb]{0.0,0.0,1.0}{2}}",
      "\\underline{\\textcolor[rgb]{0.0,0.0,1.0}{3}}")
  )


})

test_that("knit_print works to docx", {

  knit_print_output <- knit_print(base_color_vctr,method = "docx")
  knit_print_output_inline <- knit_print(base_color_vctr,method = "docx", inline = TRUE)

  expect_s3_class(knit_print_output, "knit_asis")
  expect_equivalent(
    as.character(knit_print_output),
    "```{=openxml}\n<w:p><w:pPr><w:pStyle w:val=\"SourceCode\" /></w:pPr><w:r><w:rPr><w:rStyle w:val=\"NormalTok\" /></w:rPr><w:t xml:space=\"preserve\">## [1]</w:t></w:r><w:r><w:t xml:space=\"preserve\"> </w:t></w:r><w:r><w:rPr><w:u/><w:color w:val=\"#0000FF\"/></w:rPr><w:t xml:space=\"preserve\">1</w:t></w:r><w:r><w:t xml:space=\"preserve\"> </w:t></w:r><w:r><w:rPr><w:u/><w:color w:val=\"#0000FF\"/></w:rPr><w:t xml:space=\"preserve\">2</w:t></w:r><w:r><w:t xml:space=\"preserve\"> </w:t></w:r><w:r><w:rPr><w:u/><w:color w:val=\"#0000FF\"/></w:rPr><w:t xml:space=\"preserve\">3</w:t></w:r></w:p>\n```"
  )

  expect_equivalent(class(knit_print_output_inline), "character")
  expect_equivalent(
    as.character(knit_print_output_inline),
    c("`<w:r><w:rPr><w:u/><w:color w:val=\"#0000FF\"/></w:rPr><w:t xml:space=\"preserve\">1</w:t></w:r>`{=openxml}",
      "`<w:r><w:rPr><w:u/><w:color w:val=\"#0000FF\"/></w:rPr><w:t xml:space=\"preserve\">2</w:t></w:r>`{=openxml}",
      "`<w:r><w:rPr><w:u/><w:color w:val=\"#0000FF\"/></w:rPr><w:t xml:space=\"preserve\">3</w:t></w:r>`{=openxml}")
  )

})

test_that("knit_print does not work for undefined outputs", {

  expect_error(knit_print(base_color_vctr,method = "undefined"),
               "Method for .* not implemented yet")

})
