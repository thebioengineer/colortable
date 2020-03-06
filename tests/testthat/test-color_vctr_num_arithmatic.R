
c_vect <- color_vctr(
    c(1, 2, 3, 4, 5, 6),
    text_color = "yellow",
    background = "blue",
    style = "strikethrough"
  )


test_that("multiplication works with numeric and preserves styling", {

  mult_c_vect_1    <- 1*c_vect
  mult_c_vect_2    <- 2*c_vect
  mult_c_vect_neg  <- -1*c_vect

  mult_1_c_vect    <- c_vect*1
  mult_2_c_vect    <- c_vect*2
  mult_neg_c_vect  <- c_vect*-1

  expect_equal( field(mult_c_vect_1,  "vctr"), c(1,2,3,4,5,6)   )
  expect_equal( field(mult_c_vect_2,  "vctr"), c(1,2,3,4,5,6)*2 )
  expect_equal( field(mult_c_vect_neg,"vctr"), -c(1,2,3,4,5,6)  )
  expect_equal( field(mult_1_c_vect,  "vctr"), c(1,2,3,4,5,6)   )
  expect_equal( field(mult_2_c_vect,  "vctr"), c(1,2,3,4,5,6)*2 )
  expect_equal( field(mult_neg_c_vect,"vctr"), -c(1,2,3,4,5,6)  )

  expect_equal( field(mult_c_vect_1,  ".text_color"), rep("yellow",6))
  expect_equal( field(mult_c_vect_2,  ".text_color"), rep("yellow",6))
  expect_equal( field(mult_c_vect_neg,".text_color"), rep("yellow",6))
  expect_equal( field(mult_1_c_vect,  ".text_color"), rep("yellow",6))
  expect_equal( field(mult_2_c_vect,  ".text_color"), rep("yellow",6))
  expect_equal( field(mult_neg_c_vect,".text_color"), rep("yellow",6))

  expect_equal( field(mult_c_vect_1,  ".background"), rep("blue",6))
  expect_equal( field(mult_c_vect_2,  ".background"), rep("blue",6))
  expect_equal( field(mult_c_vect_neg,".background"), rep("blue",6))
  expect_equal( field(mult_1_c_vect,  ".background"), rep("blue",6))
  expect_equal( field(mult_2_c_vect,  ".background"), rep("blue",6))
  expect_equal( field(mult_neg_c_vect,".background"), rep("blue",6))

  expect_equal( field(mult_c_vect_1,  ".style"), rep("strikethrough",6))
  expect_equal( field(mult_c_vect_2,  ".style"), rep("strikethrough",6))
  expect_equal( field(mult_c_vect_neg,".style"), rep("strikethrough",6))
  expect_equal( field(mult_1_c_vect,  ".style"), rep("strikethrough",6))
  expect_equal( field(mult_2_c_vect,  ".style"), rep("strikethrough",6))
  expect_equal( field(mult_neg_c_vect,".style"), rep("strikethrough",6))

})

test_that("multiplication works with numeric and preserves styling", {

  mult_c_vect     <- 1*c_vect
  mult_c_vect2    <- 2*c_vect
  mult_c_vect_neg <- -1*c_vect

  expect_equal( field(mult_c_vect,    "vctr"), c(1,2,3,4,5,6)   )
  expect_equal( field(mult_c_vect2,   "vctr"), c(1,2,3,4,5,6)*2 )
  expect_equal( field(mult_c_vect_neg,"vctr"), -c(1,2,3,4,5,6)  )

  expect_equal( field(mult_c_vect,    ".text_color"), rep("yellow",6))
  expect_equal( field(mult_c_vect2,   ".text_color"), rep("yellow",6))
  expect_equal( field(mult_c_vect_neg,".text_color"), rep("yellow",6))

  expect_equal( field(mult_c_vect,    ".background"), rep("blue",6))
  expect_equal( field(mult_c_vect2,   ".background"), rep("blue",6))
  expect_equal( field(mult_c_vect_neg,".background"), rep("blue",6))

  expect_equal( field(mult_c_vect,    ".style"), rep("strikethrough",6))
  expect_equal( field(mult_c_vect2,   ".style"), rep("strikethrough",6))
  expect_equal( field(mult_c_vect_neg,".style"), rep("strikethrough",6))

})

