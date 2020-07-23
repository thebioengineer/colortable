capture_print <- function(x){
  capture.output(print(x,console_width = 80, method = "docx"))
}

test_that("vector printing to docx - empty", {
  expect_equal(
    capture_print(color_vctr(character(),text_color = "blue")),
    "``{=openxml}"
  )
})

test_that("vector printing to docx - NA", {
  expect_equal(
    capture_print(color_vctr(NA,text_color = "blue")),
    "`NA`{=openxml}"
  )
})

test_that("vector printing to docx - numeric", {
  styled_vect <-
    color_vctr(c(1, 2, 0.05, 20),
               text_color = c("red", "blue", "green", NA),
               style = c("bold","italic","strikethrough",NA),
               background = c("teal",NA,"yellow",NA))

  expect_equal(
    capture_print(styled_vect),
    c("`<w:r><w:rPr><w:highlight w:val=\"darkCyan\"/><w:b/><w:color w:val=\"#FF0000\"/></w:rPr><w:t xml:space=\"preserve\"> 1.00</w:t></w:r>`{=openxml}",
      "`<w:r><w:rPr><w:i/><w:color w:val=\"#0000FF\"/></w:rPr><w:t xml:space=\"preserve\"> 2.00</w:t></w:r>`{=openxml}",
      "`<w:r><w:rPr><w:highlight w:val=\"yellow\"/><w:strike/><w:color w:val=\"#00FF00\"/></w:rPr><w:t xml:space=\"preserve\"> 0.05</w:t></w:r>`{=openxml}",
      "`<w:r><w:rPr></w:rPr><w:t xml:space=\"preserve\">20.00</w:t></w:r>`{=openxml}"  )
  )

})

test_that("vector printing to docx - integer", {
  styled_vect <-
    color_vctr(as.integer(c(1, 2, 3, 20)),
               text_color = c("red", "blue", "green", NA),
               style = c("bold","italic","strikethrough",NA),
               background = c("teal",NA,"yellow",NA))

  expect_equal(
    capture_print(styled_vect),
    c("`<w:r><w:rPr><w:highlight w:val=\"darkCyan\"/><w:b/><w:color w:val=\"#FF0000\"/></w:rPr><w:t xml:space=\"preserve\"> 1</w:t></w:r>`{=openxml}",
      "`<w:r><w:rPr><w:i/><w:color w:val=\"#0000FF\"/></w:rPr><w:t xml:space=\"preserve\"> 2</w:t></w:r>`{=openxml}",
      "`<w:r><w:rPr><w:highlight w:val=\"yellow\"/><w:strike/><w:color w:val=\"#00FF00\"/></w:rPr><w:t xml:space=\"preserve\"> 3</w:t></w:r>`{=openxml}",
      "`<w:r><w:rPr></w:rPr><w:t xml:space=\"preserve\">20</w:t></w:r>`{=openxml}")
  )
})

test_that("vector printing to docx - character", {
  styled_vect <-
    color_vctr(c("A", "B", "C", "Long Character"),
               text_color = c("red", "blue", "green", NA),
               style = c("bold","italic","strikethrough",NA),
               background = c("teal",NA,"yellow",NA))


  expect_equal(
    capture_print(styled_vect),
    c("`<w:r><w:rPr><w:highlight w:val=\"darkCyan\"/><w:b/><w:color w:val=\"#FF0000\"/></w:rPr><w:t xml:space=\"preserve\">A             </w:t></w:r>`{=openxml}",
      "`<w:r><w:rPr><w:i/><w:color w:val=\"#0000FF\"/></w:rPr><w:t xml:space=\"preserve\">B             </w:t></w:r>`{=openxml}",
      "`<w:r><w:rPr><w:highlight w:val=\"yellow\"/><w:strike/><w:color w:val=\"#00FF00\"/></w:rPr><w:t xml:space=\"preserve\">C             </w:t></w:r>`{=openxml}",
      "`<w:r><w:rPr></w:rPr><w:t xml:space=\"preserve\">Long Character</w:t></w:r>`{=openxml}")
  )

})

test_that("vector printing to docx - factor", {
  styled_vect <-
    color_vctr(factor(c("A", "B", "C", "Long Character")),
               text_color = c("red", "blue", "green", NA),
               style = c("bold","italic","strikethrough",NA),
               background = c("teal",NA,"yellow",NA))

  expect_equal(
    capture_print(styled_vect),
    c("`<w:r><w:rPr><w:highlight w:val=\"darkCyan\"/><w:b/><w:color w:val=\"#FF0000\"/></w:rPr><w:t xml:space=\"preserve\">A             </w:t></w:r>`{=openxml}",
      "`<w:r><w:rPr><w:i/><w:color w:val=\"#0000FF\"/></w:rPr><w:t xml:space=\"preserve\">B             </w:t></w:r>`{=openxml}",
      "`<w:r><w:rPr><w:highlight w:val=\"yellow\"/><w:strike/><w:color w:val=\"#00FF00\"/></w:rPr><w:t xml:space=\"preserve\">C             </w:t></w:r>`{=openxml}",
      "`<w:r><w:rPr></w:rPr><w:t xml:space=\"preserve\">Long Character</w:t></w:r>`{=openxml}" )
  )
})

test_that("vector printing to docx - character", {
  styled_vect <-
    color_vctr(c(TRUE, FALSE, TRUE, TRUE),
               text_color = c("red", "blue", "green", NA),
               style = c("bold","italic","strikethrough",NA),
               background = c("teal",NA,"yellow",NA))

  expect_equal(
    capture_print(styled_vect),
    c("`<w:r><w:rPr><w:highlight w:val=\"darkCyan\"/><w:b/><w:color w:val=\"#FF0000\"/></w:rPr><w:t xml:space=\"preserve\"> TRUE</w:t></w:r>`{=openxml}",
      "`<w:r><w:rPr><w:i/><w:color w:val=\"#0000FF\"/></w:rPr><w:t xml:space=\"preserve\">FALSE</w:t></w:r>`{=openxml}",
      "`<w:r><w:rPr><w:highlight w:val=\"yellow\"/><w:strike/><w:color w:val=\"#00FF00\"/></w:rPr><w:t xml:space=\"preserve\"> TRUE</w:t></w:r>`{=openxml}",
      "`<w:r><w:rPr></w:rPr><w:t xml:space=\"preserve\"> TRUE</w:t></w:r>`{=openxml}")
  )
})

test_that("vector printing to docx - dates", {
  styled_vect <-
    color_vctr(as.Date(c("1970-01-01","1970-01-02","1970-01-03","1970-01-04")),
               text_color = c("red", "blue", "green", NA),
               style = c("bold","italic","strikethrough",NA),
               background = c("teal",NA,"yellow",NA))

  expect_equal(
    capture_print(styled_vect),
    c("`<w:r><w:rPr><w:highlight w:val=\"darkCyan\"/><w:b/><w:color w:val=\"#FF0000\"/></w:rPr><w:t xml:space=\"preserve\">1970-01-01</w:t></w:r>`{=openxml}",
      "`<w:r><w:rPr><w:i/><w:color w:val=\"#0000FF\"/></w:rPr><w:t xml:space=\"preserve\">1970-01-02</w:t></w:r>`{=openxml}",
      "`<w:r><w:rPr><w:highlight w:val=\"yellow\"/><w:strike/><w:color w:val=\"#00FF00\"/></w:rPr><w:t xml:space=\"preserve\">1970-01-03</w:t></w:r>`{=openxml}",
      "`<w:r><w:rPr></w:rPr><w:t xml:space=\"preserve\">1970-01-04</w:t></w:r>`{=openxml}")
  )

})

test_that("Hex Code conversion works",{

  expect_equal(as_hex_codes("blue"),"#0000FF")
  expect_equal(as_hex_codes("periwinkle"),"#CCCCFF")
  expect_equal(as_hex_codes("amethyst"),"#9966CC")
  expect_equal(as_hex_codes("peach"),"#FFE5B4")
  expect_equal(as_hex_codes("purplemountainmajesty"),"#9678B6")

  expect_equal(as_hex_codes("#0000FF"),"#0000FF")
  expect_equal(as_hex_codes("#CCCCFF"),"#CCCCFF")
  expect_equal(as_hex_codes("#9966CC"),"#9966CC")
  expect_equal(as_hex_codes("#FFE5B4"),"#FFE5B4")
  expect_equal(as_hex_codes("#9678B6"),"#9678B6")

  expect_error(as_hex_codes("persimon"),"Invalid Color Name")

})

test_that("Highligher conversion works",{

    expect_equal(as_docx_highlighter("blue"),"blue")
    expect_equal(as_docx_highlighter("#0000FF"),"blue")

    expect_equal(as_docx_highlighter("periwinkle"),"lightGray")
    expect_equal(as_docx_highlighter("#CCCCFF"),"lightGray")

    expect_equal(as_docx_highlighter("amethyst"),"darkGray")
    expect_equal(as_docx_highlighter("#9966CC"),"darkGray")

    expect_equal(as_docx_highlighter("peach"),"lightGray")
    expect_equal(as_docx_highlighter("#FFE5B4"),"lightGray")

    expect_equal(as_docx_highlighter("purplemountainmajesty"),"darkGray")
    expect_equal(as_docx_highlighter("#9678B6"),"darkGray")

})

