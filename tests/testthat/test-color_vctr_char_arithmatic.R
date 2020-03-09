
c_vect <- color_vctr(
    LETTERS[1:6],
    text_color = "yellow",
    background = "blue",
    style = "strikethrough"
  )

# "+", "-", "/", "*", "^", "%%", "%/%", "!", "&", "|"

test_that("Arithmatic does not work with char", {

  expect_error(1+c_vect,"non-numeric argument to binary operator")
  expect_error(1-c_vect,"non-numeric argument to binary operator")
  expect_error(1/c_vect,"non-numeric argument to binary operator")
  expect_error(1*c_vect,"non-numeric argument to binary operator")
  expect_error(1^c_vect,"non-numeric argument to binary operator")
  expect_error(1%%c_vect,"non-numeric argument to binary operator")
  expect_error(1%/%c_vect,"non-numeric argument to binary operator")
  expect_error(!c_vect,"invalid argument type")

})
