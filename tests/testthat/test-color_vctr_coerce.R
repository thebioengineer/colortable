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
    field(color_df$sample_vect,".text_color"),
    field(sample_vect,".text_color")
    )
  expect_equal(
    field(color_df$sample_vect,".background"),
    field(sample_vect,".background")
  )
  expect_equal(
    field(color_df$sample_vect,".style"),
    field(sample_vect,".style")
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
    sapply(color_list, field, ".text_color"),
    field(sample_vect,".text_color")
  )
  expect_equal(
    sapply(color_list, field, ".background"),
    field(sample_vect,".background")
  )
  expect_equal(
    sapply(color_list, field, ".style"),
    field(sample_vect,".style")
  )

})

