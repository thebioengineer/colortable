test_that("getting internal color_vctr class works", {
  num <- color_vctr(1)
  chr <- color_vctr("a")
  int <- color_vctr(1L)

  expect_equal(
    color_vctr_class(num),
    "dbl"
  )
  expect_equal(
    color_vctr_class(chr),
    "chr"
  )
  expect_equal(
    color_vctr_class(int),
    "int"
  )
})

test_that("getting color_vctr class - abbreviated", {
  num <- color_vctr(1)
  chr <- color_vctr("a")
  int <- color_vctr(1L)

  expect_equal(
    vec_ptype_abbr(num),
    "c_vctr<dbl>"
  )
  expect_equal(
    vec_ptype_abbr(chr),
    "c_vctr<chr>"
  )
  expect_equal(
    vec_ptype_abbr(int),
    "c_vctr<int>"
  )
})


test_that("getting color_vctr class - full", {
  num <- color_vctr(1)
  chr <- color_vctr("a")
  int <- color_vctr(1L)

  expect_equal(
    vec_ptype_full(num),
    "color_vctr<dbl>"
  )
  expect_equal(
    vec_ptype_full(chr),
    "color_vctr<chr>"
  )
  expect_equal(
    vec_ptype_full(int),
    "color_vctr<int>"
  )
})
