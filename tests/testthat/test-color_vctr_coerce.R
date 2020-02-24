test_that("as.data.frame", {
  sample_vect <- color_vctr(
    LETTERS,
    text_color = "blue",
    style = "underline"
  )

  expect_false(
    inherits(try(color_df <- as.data.frame(sample_vect)),"try-error")
  )
  expect_equal(
    attr(color_df$sample_vect,".text_color"),
    attr(sample_vect,".text_color")
    )
  expect_equal(
    attr(color_df$sample_vect,".background"),
    attr(sample_vect,".background")
  )
  expect_equal(
    attr(color_df$sample_vect,".style"),
    attr(sample_vect,".style")
  )

})


test_that("as.list",{
  sample_vect <- color_vctr(
    LETTERS,
    text_color = "blue",
    style = "underline"
  )

  expect_false(
    inherits(try(color_list <- as.list(sample_vect)),"try-error")
  )
  expect_equal(
    sapply(color_list, attr, ".text_color"),
    attr(sample_vect,".text_color")
  )
  expect_equal(
    sapply(color_list, attr, ".background"),
    attr(sample_vect,".background")
  )
  expect_equal(
    sapply(color_list, attr, ".style"),
    attr(sample_vect,".style")
  )

})

