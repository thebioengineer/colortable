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
    vctrs::field(color_df$sample_vect,".text_color"),
    vctrs::field(sample_vect,".text_color")
    )
  expect_equal(
    vctrs::field(color_df$sample_vect,".background"),
    vctrs::field(sample_vect,".background")
  )
  expect_equal(
    vctrs::field(color_df$sample_vect,".style"),
    vctrs::field(sample_vect,".style")
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
    lapply(color_list,vctrs::field,"vctr"),
    as.list(LETTERS)
  )

  expect_equal(
    sapply(color_list, vctrs::field, ".text_color"),
    vctrs::field(sample_vect,".text_color")
  )
  expect_equal(
    sapply(color_list, vctrs::field, ".background"),
    vctrs::field(sample_vect,".background")
  )
  expect_equal(
    sapply(color_list, vctrs::field, ".style"),
    vctrs::field(sample_vect,".style")
  )

})

