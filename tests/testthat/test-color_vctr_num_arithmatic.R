
c_vect <- color_vctr(
    c(1, 2, 3, 4, 5, 6),
    text_color = "yellow",
    background = "blue",
    style = "strikethrough"
  )

# "+", "-", "/", "*", "^", "%%", "%/%", "!", "&", "|"

test_that("subtraction works with numeric and preserves styling", {

  subtraction_c_vect_1    <- 1-c_vect
  subtraction_c_vect_2    <- 2-c_vect
  subtraction_c_vect_neg  <- -1-c_vect

  subtraction_1_c_vect    <- c_vect-1
  subtraction_2_c_vect    <- c_vect-2
  subtraction_neg_c_vect  <- c_vect--1

  expect_equal( field(subtraction_c_vect_1,  "vctr"), 1-c(1,2,3,4,5,6)   )
  expect_equal( field(subtraction_c_vect_2,  "vctr"), 2-c(1,2,3,4,5,6) )
  expect_equal( field(subtraction_c_vect_neg,"vctr"), -1-c(1,2,3,4,5,6)  )
  expect_equal( field(subtraction_1_c_vect,  "vctr"), c(1,2,3,4,5,6)-1   )
  expect_equal( field(subtraction_2_c_vect,  "vctr"), c(1,2,3,4,5,6)-2 )
  expect_equal( field(subtraction_neg_c_vect,"vctr"), c(1,2,3,4,5,6)--1  )

  expect_equal( field(subtraction_c_vect_1,  ".text_color"), rep("yellow",6))
  expect_equal( field(subtraction_c_vect_2,  ".text_color"), rep("yellow",6))
  expect_equal( field(subtraction_c_vect_neg,".text_color"), rep("yellow",6))
  expect_equal( field(subtraction_1_c_vect,  ".text_color"), rep("yellow",6))
  expect_equal( field(subtraction_2_c_vect,  ".text_color"), rep("yellow",6))
  expect_equal( field(subtraction_neg_c_vect,".text_color"), rep("yellow",6))

  expect_equal( field(subtraction_c_vect_1,  ".background"), rep("blue",6))
  expect_equal( field(subtraction_c_vect_2,  ".background"), rep("blue",6))
  expect_equal( field(subtraction_c_vect_neg,".background"), rep("blue",6))
  expect_equal( field(subtraction_1_c_vect,  ".background"), rep("blue",6))
  expect_equal( field(subtraction_2_c_vect,  ".background"), rep("blue",6))
  expect_equal( field(subtraction_neg_c_vect,".background"), rep("blue",6))

  expect_equal( field(subtraction_c_vect_1,  ".style"), rep("strikethrough",6))
  expect_equal( field(subtraction_c_vect_2,  ".style"), rep("strikethrough",6))
  expect_equal( field(subtraction_c_vect_neg,".style"), rep("strikethrough",6))
  expect_equal( field(subtraction_1_c_vect,  ".style"), rep("strikethrough",6))
  expect_equal( field(subtraction_2_c_vect,  ".style"), rep("strikethrough",6))
  expect_equal( field(subtraction_neg_c_vect,".style"), rep("strikethrough",6))

})

test_that("addition works with numeric and preserves styling", {

  add_c_vect_1    <- 1+c_vect
  add_c_vect_2    <- 2+c_vect
  add_c_vect_neg  <- -1+c_vect

  add_1_c_vect    <- c_vect+1
  add_2_c_vect    <- c_vect+2
  add_neg_c_vect  <- c_vect+-1

  expect_equal( field(add_c_vect_1,  "vctr"), 1+c(1,2,3,4,5,6)   )
  expect_equal( field(add_c_vect_2,  "vctr"), 2+c(1,2,3,4,5,6) )
  expect_equal( field(add_c_vect_neg,"vctr"), -1+c(1,2,3,4,5,6)  )
  expect_equal( field(add_1_c_vect,  "vctr"), c(1,2,3,4,5,6)+1   )
  expect_equal( field(add_2_c_vect,  "vctr"), c(1,2,3,4,5,6)+2 )
  expect_equal( field(add_neg_c_vect,"vctr"), c(1,2,3,4,5,6)+-1  )

  expect_equal( field(add_c_vect_1,  ".text_color"), rep("yellow",6))
  expect_equal( field(add_c_vect_2,  ".text_color"), rep("yellow",6))
  expect_equal( field(add_c_vect_neg,".text_color"), rep("yellow",6))
  expect_equal( field(add_1_c_vect,  ".text_color"), rep("yellow",6))
  expect_equal( field(add_2_c_vect,  ".text_color"), rep("yellow",6))
  expect_equal( field(add_neg_c_vect,".text_color"), rep("yellow",6))

  expect_equal( field(add_c_vect_1,  ".background"), rep("blue",6))
  expect_equal( field(add_c_vect_2,  ".background"), rep("blue",6))
  expect_equal( field(add_c_vect_neg,".background"), rep("blue",6))
  expect_equal( field(add_1_c_vect,  ".background"), rep("blue",6))
  expect_equal( field(add_2_c_vect,  ".background"), rep("blue",6))
  expect_equal( field(add_neg_c_vect,".background"), rep("blue",6))

  expect_equal( field(add_c_vect_1,  ".style"), rep("strikethrough",6))
  expect_equal( field(add_c_vect_2,  ".style"), rep("strikethrough",6))
  expect_equal( field(add_c_vect_neg,".style"), rep("strikethrough",6))
  expect_equal( field(add_1_c_vect,  ".style"), rep("strikethrough",6))
  expect_equal( field(add_2_c_vect,  ".style"), rep("strikethrough",6))
  expect_equal( field(add_neg_c_vect,".style"), rep("strikethrough",6))

})

test_that("division works with numeric and preserves styling", {

  div_c_vect_1    <- 1/c_vect
  div_c_vect_2    <- 2/c_vect
  div_c_vect_neg  <- -1/c_vect

  div_1_c_vect    <- c_vect/1
  div_2_c_vect    <- c_vect/2
  div_neg_c_vect  <- c_vect/-1

  expect_equal( field(div_c_vect_1,  "vctr"), 1/c(1,2,3,4,5,6)   )
  expect_equal( field(div_c_vect_2,  "vctr"), 2/c(1,2,3,4,5,6) )
  expect_equal( field(div_c_vect_neg,"vctr"), -1/c(1,2,3,4,5,6)  )
  expect_equal( field(div_1_c_vect,  "vctr"), c(1,2,3,4,5,6)   )
  expect_equal( field(div_2_c_vect,  "vctr"), c(1,2,3,4,5,6)/2 )
  expect_equal( field(div_neg_c_vect,"vctr"), -c(1,2,3,4,5,6)  )

  expect_equal( field(div_c_vect_1,  ".text_color"), rep("yellow",6))
  expect_equal( field(div_c_vect_2,  ".text_color"), rep("yellow",6))
  expect_equal( field(div_c_vect_neg,".text_color"), rep("yellow",6))
  expect_equal( field(div_1_c_vect,  ".text_color"), rep("yellow",6))
  expect_equal( field(div_2_c_vect,  ".text_color"), rep("yellow",6))
  expect_equal( field(div_neg_c_vect,".text_color"), rep("yellow",6))

  expect_equal( field(div_c_vect_1,  ".background"), rep("blue",6))
  expect_equal( field(div_c_vect_2,  ".background"), rep("blue",6))
  expect_equal( field(div_c_vect_neg,".background"), rep("blue",6))
  expect_equal( field(div_1_c_vect,  ".background"), rep("blue",6))
  expect_equal( field(div_2_c_vect,  ".background"), rep("blue",6))
  expect_equal( field(div_neg_c_vect,".background"), rep("blue",6))

  expect_equal( field(div_c_vect_1,  ".style"), rep("strikethrough",6))
  expect_equal( field(div_c_vect_2,  ".style"), rep("strikethrough",6))
  expect_equal( field(div_c_vect_neg,".style"), rep("strikethrough",6))
  expect_equal( field(div_1_c_vect,  ".style"), rep("strikethrough",6))
  expect_equal( field(div_2_c_vect,  ".style"), rep("strikethrough",6))
  expect_equal( field(div_neg_c_vect,".style"), rep("strikethrough",6))

})

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

test_that("exponents work with numeric and preserves styling", {

  exp_c_vect_1    <- 1^c_vect
  exp_c_vect_2    <- 2^c_vect
  exp_c_vect_neg  <- -1^c_vect

  exp_1_c_vect    <- c_vect^1
  exp_2_c_vect    <- c_vect^2
  exp_neg_c_vect  <- c_vect^-1

  expect_equal( field(exp_c_vect_1,  "vctr"), 1^c(1,2,3,4,5,6)   )
  expect_equal( field(exp_c_vect_2,  "vctr"), 2^c(1,2,3,4,5,6) )
  expect_equal( field(exp_c_vect_neg,"vctr"), -1^c(1,2,3,4,5,6)  )
  expect_equal( field(exp_1_c_vect,  "vctr"), c(1,2,3,4,5,6)   )
  expect_equal( field(exp_2_c_vect,  "vctr"), c(1,2,3,4,5,6)^2 )
  expect_equal( field(exp_neg_c_vect,"vctr"), c(1,2,3,4,5,6)^-1  )

  expect_equal( field(exp_c_vect_1,  ".text_color"), rep("yellow",6))
  expect_equal( field(exp_c_vect_2,  ".text_color"), rep("yellow",6))
  expect_equal( field(exp_c_vect_neg,".text_color"), rep("yellow",6))
  expect_equal( field(exp_1_c_vect,  ".text_color"), rep("yellow",6))
  expect_equal( field(exp_2_c_vect,  ".text_color"), rep("yellow",6))
  expect_equal( field(exp_neg_c_vect,".text_color"), rep("yellow",6))

  expect_equal( field(exp_c_vect_1,  ".background"), rep("blue",6))
  expect_equal( field(exp_c_vect_2,  ".background"), rep("blue",6))
  expect_equal( field(exp_c_vect_neg,".background"), rep("blue",6))
  expect_equal( field(exp_1_c_vect,  ".background"), rep("blue",6))
  expect_equal( field(exp_2_c_vect,  ".background"), rep("blue",6))
  expect_equal( field(exp_neg_c_vect,".background"), rep("blue",6))

  expect_equal( field(exp_c_vect_1,  ".style"), rep("strikethrough",6))
  expect_equal( field(exp_c_vect_2,  ".style"), rep("strikethrough",6))
  expect_equal( field(exp_c_vect_neg,".style"), rep("strikethrough",6))
  expect_equal( field(exp_1_c_vect,  ".style"), rep("strikethrough",6))
  expect_equal( field(exp_2_c_vect,  ".style"), rep("strikethrough",6))
  expect_equal( field(exp_neg_c_vect,".style"), rep("strikethrough",6))

})

test_that("modulo work with numeric and preserves styling", {

  modulo_c_vect_1    <- 1%%c_vect
  modulo_c_vect_2    <- 2%%c_vect
  modulo_c_vect_neg  <- -1%%c_vect

  modulo_1_c_vect    <- c_vect%%1
  modulo_2_c_vect    <- c_vect%%2
  modulo_neg_c_vect  <- c_vect%%-1

  expect_equal( field(modulo_c_vect_1,  "vctr"), 1%%c(1,2,3,4,5,6)   )
  expect_equal( field(modulo_c_vect_2,  "vctr"), 2%%c(1,2,3,4,5,6) )
  expect_equal( field(modulo_c_vect_neg,"vctr"), -1%%c(1,2,3,4,5,6)  )
  expect_equal( field(modulo_1_c_vect,  "vctr"), c(1,2,3,4,5,6)%%1   )
  expect_equal( field(modulo_2_c_vect,  "vctr"), c(1,2,3,4,5,6)%%2 )
  expect_equal( field(modulo_neg_c_vect,"vctr"), c(1,2,3,4,5,6)%%-1  )

  expect_equal( field(modulo_c_vect_1,  ".text_color"), rep("yellow",6))
  expect_equal( field(modulo_c_vect_2,  ".text_color"), rep("yellow",6))
  expect_equal( field(modulo_c_vect_neg,".text_color"), rep("yellow",6))
  expect_equal( field(modulo_1_c_vect,  ".text_color"), rep("yellow",6))
  expect_equal( field(modulo_2_c_vect,  ".text_color"), rep("yellow",6))
  expect_equal( field(modulo_neg_c_vect,".text_color"), rep("yellow",6))

  expect_equal( field(modulo_c_vect_1,  ".background"), rep("blue",6))
  expect_equal( field(modulo_c_vect_2,  ".background"), rep("blue",6))
  expect_equal( field(modulo_c_vect_neg,".background"), rep("blue",6))
  expect_equal( field(modulo_1_c_vect,  ".background"), rep("blue",6))
  expect_equal( field(modulo_2_c_vect,  ".background"), rep("blue",6))
  expect_equal( field(modulo_neg_c_vect,".background"), rep("blue",6))

  expect_equal( field(modulo_c_vect_1,  ".style"), rep("strikethrough",6))
  expect_equal( field(modulo_c_vect_2,  ".style"), rep("strikethrough",6))
  expect_equal( field(modulo_c_vect_neg,".style"), rep("strikethrough",6))
  expect_equal( field(modulo_1_c_vect,  ".style"), rep("strikethrough",6))
  expect_equal( field(modulo_2_c_vect,  ".style"), rep("strikethrough",6))
  expect_equal( field(modulo_neg_c_vect,".style"), rep("strikethrough",6))

})

test_that("integer division work with numeric and preserves styling", {

  int_div_c_vect_1    <- 1%/%c_vect
  int_div_c_vect_2    <- 2%/%c_vect
  int_div_c_vect_neg  <- -1%/%c_vect

  int_div_1_c_vect    <- c_vect%/%1
  int_div_2_c_vect    <- c_vect%/%2
  int_div_neg_c_vect  <- c_vect%/%-1

  expect_equal( field(int_div_c_vect_1,  "vctr"), 1%/%c(1,2,3,4,5,6)   )
  expect_equal( field(int_div_c_vect_2,  "vctr"), 2%/%c(1,2,3,4,5,6) )
  expect_equal( field(int_div_c_vect_neg,"vctr"), -1%/%c(1,2,3,4,5,6)  )
  expect_equal( field(int_div_1_c_vect,  "vctr"), c(1,2,3,4,5,6)%/%1   )
  expect_equal( field(int_div_2_c_vect,  "vctr"), c(1,2,3,4,5,6)%/%2 )
  expect_equal( field(int_div_neg_c_vect,"vctr"), c(1,2,3,4,5,6)%/%-1  )

  expect_equal( field(int_div_c_vect_1,  ".text_color"), rep("yellow",6))
  expect_equal( field(int_div_c_vect_2,  ".text_color"), rep("yellow",6))
  expect_equal( field(int_div_c_vect_neg,".text_color"), rep("yellow",6))
  expect_equal( field(int_div_1_c_vect,  ".text_color"), rep("yellow",6))
  expect_equal( field(int_div_2_c_vect,  ".text_color"), rep("yellow",6))
  expect_equal( field(int_div_neg_c_vect,".text_color"), rep("yellow",6))

  expect_equal( field(int_div_c_vect_1,  ".background"), rep("blue",6))
  expect_equal( field(int_div_c_vect_2,  ".background"), rep("blue",6))
  expect_equal( field(int_div_c_vect_neg,".background"), rep("blue",6))
  expect_equal( field(int_div_1_c_vect,  ".background"), rep("blue",6))
  expect_equal( field(int_div_2_c_vect,  ".background"), rep("blue",6))
  expect_equal( field(int_div_neg_c_vect,".background"), rep("blue",6))

  expect_equal( field(int_div_c_vect_1,  ".style"), rep("strikethrough",6))
  expect_equal( field(int_div_c_vect_2,  ".style"), rep("strikethrough",6))
  expect_equal( field(int_div_c_vect_neg,".style"), rep("strikethrough",6))
  expect_equal( field(int_div_1_c_vect,  ".style"), rep("strikethrough",6))
  expect_equal( field(int_div_2_c_vect,  ".style"), rep("strikethrough",6))
  expect_equal( field(int_div_neg_c_vect,".style"), rep("strikethrough",6))

})

test_that("NOT(!) works with numeric and preserves styling", {

  NOT_c_vect    <- !c_vect

  expect_equal( field(NOT_c_vect,  "vctr"), !c(1,2,3,4,5,6)   )

  expect_equal( field(NOT_c_vect,  ".text_color"), rep("yellow",6))

  expect_equal( field(NOT_c_vect,  ".background"), rep("blue",6))

  expect_equal( field(NOT_c_vect,  ".style"), rep("strikethrough",6))

})

test_that("AND(&) works with numeric and preserves styling", {

  AND_c_vect_1    <- 1%/%c_vect
  AND_c_vect_2    <- 2%/%c_vect
  AND_c_vect_neg  <- -1%/%c_vect

  AND_1_c_vect    <- c_vect%/%1
  AND_2_c_vect    <- c_vect%/%2
  AND_neg_c_vect  <- c_vect%/%-1

  expect_equal( field(AND_c_vect_1,  "vctr"), 1%/%c(1,2,3,4,5,6)   )
  expect_equal( field(AND_c_vect_2,  "vctr"), 2%/%c(1,2,3,4,5,6) )
  expect_equal( field(AND_c_vect_neg,"vctr"), -1%/%c(1,2,3,4,5,6)  )
  expect_equal( field(AND_1_c_vect,  "vctr"), c(1,2,3,4,5,6)%/%1   )
  expect_equal( field(AND_2_c_vect,  "vctr"), c(1,2,3,4,5,6)%/%2 )
  expect_equal( field(AND_neg_c_vect,"vctr"), c(1,2,3,4,5,6)%/%-1  )

  expect_equal( field(AND_c_vect_1,  ".text_color"), rep("yellow",6))
  expect_equal( field(AND_c_vect_2,  ".text_color"), rep("yellow",6))
  expect_equal( field(AND_c_vect_neg,".text_color"), rep("yellow",6))
  expect_equal( field(AND_1_c_vect,  ".text_color"), rep("yellow",6))
  expect_equal( field(AND_2_c_vect,  ".text_color"), rep("yellow",6))
  expect_equal( field(AND_neg_c_vect,".text_color"), rep("yellow",6))

  expect_equal( field(AND_c_vect_1,  ".background"), rep("blue",6))
  expect_equal( field(AND_c_vect_2,  ".background"), rep("blue",6))
  expect_equal( field(AND_c_vect_neg,".background"), rep("blue",6))
  expect_equal( field(AND_1_c_vect,  ".background"), rep("blue",6))
  expect_equal( field(AND_2_c_vect,  ".background"), rep("blue",6))
  expect_equal( field(AND_neg_c_vect,".background"), rep("blue",6))

  expect_equal( field(AND_c_vect_1,  ".style"), rep("strikethrough",6))
  expect_equal( field(AND_c_vect_2,  ".style"), rep("strikethrough",6))
  expect_equal( field(AND_c_vect_neg,".style"), rep("strikethrough",6))
  expect_equal( field(AND_1_c_vect,  ".style"), rep("strikethrough",6))
  expect_equal( field(AND_2_c_vect,  ".style"), rep("strikethrough",6))
  expect_equal( field(AND_neg_c_vect,".style"), rep("strikethrough",6))

})

test_that("OR(|) works with numeric and preserves styling", {

  OR_c_vect_1    <- 1|c_vect
  OR_c_vect_2    <- 2|c_vect
  OR_c_vect_neg  <- -1|c_vect

  OR_1_c_vect    <- c_vect|1
  OR_2_c_vect    <- c_vect|2
  OR_neg_c_vect  <- c_vect|-1

  expect_equal( field(OR_c_vect_1,  "vctr"), 1|c(1,2,3,4,5,6)   )
  expect_equal( field(OR_c_vect_2,  "vctr"), 2|c(1,2,3,4,5,6) )
  expect_equal( field(OR_c_vect_neg,"vctr"), -1|c(1,2,3,4,5,6)  )
  expect_equal( field(OR_1_c_vect,  "vctr"), c(1,2,3,4,5,6)|1   )
  expect_equal( field(OR_2_c_vect,  "vctr"), c(1,2,3,4,5,6)|2 )
  expect_equal( field(OR_neg_c_vect,"vctr"), c(1,2,3,4,5,6)|-1  )

  expect_equal( field(OR_c_vect_1,  ".text_color"), rep("yellow",6))
  expect_equal( field(OR_c_vect_2,  ".text_color"), rep("yellow",6))
  expect_equal( field(OR_c_vect_neg,".text_color"), rep("yellow",6))
  expect_equal( field(OR_1_c_vect,  ".text_color"), rep("yellow",6))
  expect_equal( field(OR_2_c_vect,  ".text_color"), rep("yellow",6))
  expect_equal( field(OR_neg_c_vect,".text_color"), rep("yellow",6))

  expect_equal( field(OR_c_vect_1,  ".background"), rep("blue",6))
  expect_equal( field(OR_c_vect_2,  ".background"), rep("blue",6))
  expect_equal( field(OR_c_vect_neg,".background"), rep("blue",6))
  expect_equal( field(OR_1_c_vect,  ".background"), rep("blue",6))
  expect_equal( field(OR_2_c_vect,  ".background"), rep("blue",6))
  expect_equal( field(OR_neg_c_vect,".background"), rep("blue",6))

  expect_equal( field(OR_c_vect_1,  ".style"), rep("strikethrough",6))
  expect_equal( field(OR_c_vect_2,  ".style"), rep("strikethrough",6))
  expect_equal( field(OR_c_vect_neg,".style"), rep("strikethrough",6))
  expect_equal( field(OR_1_c_vect,  ".style"), rep("strikethrough",6))
  expect_equal( field(OR_2_c_vect,  ".style"), rep("strikethrough",6))
  expect_equal( field(OR_neg_c_vect,".style"), rep("strikethrough",6))

})
