

base_rgb <- c(255, 128, 128)

sampleMat <- matrix(
  c(
    c(255,127,127),
    c(127,255,127),
    c(127,127,255)
  ),
  ncol = 3,
  byrow = TRUE
)


test_that("color matching - euclidian", {
  expect_equal(
    which_closest_color.euclidian(base_rgb,sampleMat),
    1
  )
})

test_that("color matching - weighted", {
  expect_equal(
    which_closest_color.weighted(base_rgb,sampleMat),
    1
  )
})
