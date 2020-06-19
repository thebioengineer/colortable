capture_print <- function(x){
  capture.output(print(x,console_width = 80, method = "html"))
}


test_that("vector printing to console - empty", {
  expect_equal(
    capture_print(color_vctr(character(),text_color = "blue")),
    ""
  )
})

test_that("vector printing to console - numeric", {
  styled_vect <-
    color_vctr(c(1, 2, 0.05, 20), text_color = c("red", "blue", "green", NA))

  expect_equal(
    capture_print(styled_vect),
    c("<span style='color:red;'> 1.00</span>",  "<span style='color:blue;'> 2.00</span>",
    "<span style='color:green;'> 0.05</span>", "<span style=''>20.00</span>")
  )

})

test_that("vector printing to console - integer", {
  styled_vect <-
    color_vctr(as.integer(c(1, 2, 3, 20)), text_color = c("red", "blue", "green", NA))

  expect_equal(
    capture_print(styled_vect),
    c("<span style='color:red;'> 1</span>",  "<span style='color:blue;'> 2</span>",
      "<span style='color:green;'> 3</span>", "<span style=''>20</span>")
  )
})

test_that("vector printing to console - character", {
  styled_vect <-
    color_vctr(c("A", "B", "C", "Long Character"), text_color = c("red", "blue", "green", NA))


  expect_equal(
    capture_print(styled_vect),
    c("<span style='color:red;'>A             </span>",  "<span style='color:blue;'>B             </span>",
      "<span style='color:green;'>C             </span>", "<span style=''>Long Character</span>")
  )

})

test_that("vector printing to console - factor", {
  styled_vect <-
    color_vctr(factor(c("A", "B", "C", "Long Character")), text_color = c("red", "blue", "green", NA))

  expect_equal(
    capture_print(styled_vect),
    c("<span style='color:red;'>A             </span>",  "<span style='color:blue;'>B             </span>",
      "<span style='color:green;'>C             </span>", "<span style=''>Long Character</span>")
  )
})

test_that("vector printing to console - character", {
  styled_vect <-
    color_vctr(c(TRUE, FALSE, TRUE, TRUE), text_color = c("red", "blue", "green", NA))

  expect_equal(
    capture_print(styled_vect),
    c("<span style='color:red;'> TRUE</span>",  "<span style='color:blue;'>FALSE</span>",
      "<span style='color:green;'> TRUE</span>", "<span style=''> TRUE</span>")
  )
})

test_that("vector printing to console - dates", {
  styled_vect <-
    color_vctr(as.Date(c("1970-01-01","1970-01-02","1970-01-03","1970-01-04")),
               text_color = c("red", "blue", "green", NA))

  expect_equal(
    capture_print(styled_vect),
    c("<span style='color:red;'>1970-01-01</span>",  "<span style='color:blue;'>1970-01-02</span>",
      "<span style='color:green;'>1970-01-03</span>", "<span style=''>1970-01-04</span>")
  )

})


