capture_print <- function(x){
  capture.output(print(x,console_width = 80, method = "docx"))
}

test_that("vector printing to console - numeric", {
  styled_vect <-
    color_vctr(c(1, 2, 0.05, 20), text_color = c("red", "blue", "green", NA))

  expect_equal(
    capture_print(styled_vect),
    c("`<w:r><w:rPr xmlns:w14=\"http://schemas.microsoft.com/office/word/2010/wordml\"><w:color w:val=\"#FF0000\"/></w:rPr><w:t xml:space=\"preserve\"> 1.00</w:t></w:r>`{=openxml}",
      "`<w:r><w:rPr xmlns:w14=\"http://schemas.microsoft.com/office/word/2010/wordml\"><w:color w:val=\"#0000FF\"/></w:rPr><w:t xml:space=\"preserve\"> 2.00</w:t></w:r>`{=openxml}",
      "`<w:r><w:rPr xmlns:w14=\"http://schemas.microsoft.com/office/word/2010/wordml\"><w:color w:val=\"#00FF00\"/></w:rPr><w:t xml:space=\"preserve\"> 0.05</w:t></w:r>`{=openxml}",
      "`<w:r><w:rPr xmlns:w14=\"http://schemas.microsoft.com/office/word/2010/wordml\"></w:rPr><w:t xml:space=\"preserve\">20.00</w:t></w:r>`{=openxml}" )
  )

})

test_that("vector printing to console - integer", {
  styled_vect <-
    color_vctr(as.integer(c(1, 2, 3, 20)), text_color = c("red", "blue", "green", NA))

  expect_equal(
    capture_print(styled_vect),
    c("`<w:r><w:rPr xmlns:w14=\"http://schemas.microsoft.com/office/word/2010/wordml\"><w:color w:val=\"#FF0000\"/></w:rPr><w:t xml:space=\"preserve\"> 1</w:t></w:r>`{=openxml}",
      "`<w:r><w:rPr xmlns:w14=\"http://schemas.microsoft.com/office/word/2010/wordml\"><w:color w:val=\"#0000FF\"/></w:rPr><w:t xml:space=\"preserve\"> 2</w:t></w:r>`{=openxml}",
      "`<w:r><w:rPr xmlns:w14=\"http://schemas.microsoft.com/office/word/2010/wordml\"><w:color w:val=\"#00FF00\"/></w:rPr><w:t xml:space=\"preserve\"> 3</w:t></w:r>`{=openxml}",
      "`<w:r><w:rPr xmlns:w14=\"http://schemas.microsoft.com/office/word/2010/wordml\"></w:rPr><w:t xml:space=\"preserve\">20</w:t></w:r>`{=openxml}")
  )
})

test_that("vector printing to console - character", {
  styled_vect <-
    color_vctr(c("A", "B", "C", "Long Character"), text_color = c("red", "blue", "green", NA))


  expect_equal(
    capture_print(styled_vect),
    c("`<w:r><w:rPr xmlns:w14=\"http://schemas.microsoft.com/office/word/2010/wordml\"><w:color w:val=\"#FF0000\"/></w:rPr><w:t xml:space=\"preserve\">A             </w:t></w:r>`{=openxml}",
      "`<w:r><w:rPr xmlns:w14=\"http://schemas.microsoft.com/office/word/2010/wordml\"><w:color w:val=\"#0000FF\"/></w:rPr><w:t xml:space=\"preserve\">B             </w:t></w:r>`{=openxml}",
      "`<w:r><w:rPr xmlns:w14=\"http://schemas.microsoft.com/office/word/2010/wordml\"><w:color w:val=\"#00FF00\"/></w:rPr><w:t xml:space=\"preserve\">C             </w:t></w:r>`{=openxml}",
      "`<w:r><w:rPr xmlns:w14=\"http://schemas.microsoft.com/office/word/2010/wordml\"></w:rPr><w:t xml:space=\"preserve\">Long Character</w:t></w:r>`{=openxml}")
  )

})

test_that("vector printing to console - factor", {
  styled_vect <-
    color_vctr(factor(c("A", "B", "C", "Long Character")), text_color = c("red", "blue", "green", NA))

  expect_equal(
    capture_print(styled_vect),
    c("`<w:r><w:rPr xmlns:w14=\"http://schemas.microsoft.com/office/word/2010/wordml\"><w:color w:val=\"#FF0000\"/></w:rPr><w:t xml:space=\"preserve\">A             </w:t></w:r>`{=openxml}",
      "`<w:r><w:rPr xmlns:w14=\"http://schemas.microsoft.com/office/word/2010/wordml\"><w:color w:val=\"#0000FF\"/></w:rPr><w:t xml:space=\"preserve\">B             </w:t></w:r>`{=openxml}",
      "`<w:r><w:rPr xmlns:w14=\"http://schemas.microsoft.com/office/word/2010/wordml\"><w:color w:val=\"#00FF00\"/></w:rPr><w:t xml:space=\"preserve\">C             </w:t></w:r>`{=openxml}",
      "`<w:r><w:rPr xmlns:w14=\"http://schemas.microsoft.com/office/word/2010/wordml\"></w:rPr><w:t xml:space=\"preserve\">Long Character</w:t></w:r>`{=openxml}" )
  )
})

test_that("vector printing to console - character", {
  styled_vect <-
    color_vctr(c(TRUE, FALSE, TRUE, TRUE), text_color = c("red", "blue", "green", NA))

  expect_equal(
    capture_print(styled_vect),
    c("`<w:r><w:rPr xmlns:w14=\"http://schemas.microsoft.com/office/word/2010/wordml\"><w:color w:val=\"#FF0000\"/></w:rPr><w:t xml:space=\"preserve\"> TRUE</w:t></w:r>`{=openxml}",
      "`<w:r><w:rPr xmlns:w14=\"http://schemas.microsoft.com/office/word/2010/wordml\"><w:color w:val=\"#0000FF\"/></w:rPr><w:t xml:space=\"preserve\">FALSE</w:t></w:r>`{=openxml}",
      "`<w:r><w:rPr xmlns:w14=\"http://schemas.microsoft.com/office/word/2010/wordml\"><w:color w:val=\"#00FF00\"/></w:rPr><w:t xml:space=\"preserve\"> TRUE</w:t></w:r>`{=openxml}",
      "`<w:r><w:rPr xmlns:w14=\"http://schemas.microsoft.com/office/word/2010/wordml\"></w:rPr><w:t xml:space=\"preserve\"> TRUE</w:t></w:r>`{=openxml}")
  )
})

test_that("vector printing to console - dates", {
  styled_vect <-
    color_vctr(as.Date(c("1970-01-01","1970-01-02","1970-01-03","1970-01-04")),
               text_color = c("red", "blue", "green", NA))

  expect_equal(
    capture_print(styled_vect),
    c("`<w:r><w:rPr xmlns:w14=\"http://schemas.microsoft.com/office/word/2010/wordml\"><w:color w:val=\"#FF0000\"/></w:rPr><w:t xml:space=\"preserve\">1970-01-01</w:t></w:r>`{=openxml}",
      "`<w:r><w:rPr xmlns:w14=\"http://schemas.microsoft.com/office/word/2010/wordml\"><w:color w:val=\"#0000FF\"/></w:rPr><w:t xml:space=\"preserve\">1970-01-02</w:t></w:r>`{=openxml}",
      "`<w:r><w:rPr xmlns:w14=\"http://schemas.microsoft.com/office/word/2010/wordml\"><w:color w:val=\"#00FF00\"/></w:rPr><w:t xml:space=\"preserve\">1970-01-03</w:t></w:r>`{=openxml}",
      "`<w:r><w:rPr xmlns:w14=\"http://schemas.microsoft.com/office/word/2010/wordml\"></w:rPr><w:t xml:space=\"preserve\">1970-01-04</w:t></w:r>`{=openxml}")
  )

})


