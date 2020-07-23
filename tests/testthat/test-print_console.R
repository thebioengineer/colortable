capture_print <- function(x, console_width = 80, ...){
  capture.output(print(x,console_width = console_width, method = "console", ...))
}

test_that("vector printing to console - empty", {
  expect_equal(
    capture_print(color_vctr(character(),text_color = "blue")),
    "color_vctr<character>(0)"
  )
})



test_that("vector printing to console - numeric", {
  styled_vect <-
    color_vctr(c(1, 2, 0.05, 20), text_color = c("red", "blue", "green", NA))
  styled_vect_long <-
    color_vctr(
      c(
        0.720903896,
        0.875773193,
        0.760982328,
        0.886124566,
        0.45648096,
        0.166371785,
        0.325095387,
        0.509224336,
        0.727705254,
        0.989736938,
        0.034535435,
        0.152373490,
        0.735684952,
        0.001136587,
        0.391203335,
        0.462494654,
        0.388143982,
        0.402485142,
        0.178963585,
        0.951658754
      ),
      text_color = c(
        "green",
        "blue"  ,
        "blue" ,
        "red"  ,
        "blue"  ,
        "green",
        "red"  ,
        "blue"  ,
        "red"  ,
        "blue",
        "green",
        "blue",
        "blue",
        "green",
        "red" ,
        "red" ,
        "green",
        "blue",
        "red" ,
        "red"
      )
    )


c("green", "blue"  ,"blue" ,
"red"  , "blue"  ,"green",
"red"  , "blue"  ,"red"  ,
"blue",  "green", "blue",
"blue",  "green", "red" ,
"red" ,  "green", "blue",
"red" ,  "red")

  expect_equal(
    capture_print(styled_vect),
    "[1] \033[38;5;9m 1.00\033[0m \033[38;5;12m 2.00\033[0m \033[38;5;10m 0.05\033[0m 20.00"
  )

  expect_equal(
    capture_print(styled_vect_long),
    c(" [1] \033[38;5;10m0.720903896\033[0m \033[38;5;12m0.875773193\033[0m \033[38;5;12m0.760982328\033[0m \033[38;5;9m0.886124566\033[0m \033[38;5;12m0.456480960\033[0m \033[38;5;10m0.166371785\033[0m",
      " [7] \033[38;5;9m0.325095387\033[0m \033[38;5;12m0.509224336\033[0m \033[38;5;9m0.727705254\033[0m \033[38;5;12m0.989736938\033[0m \033[38;5;10m0.034535435\033[0m \033[38;5;12m0.152373490\033[0m" ,
      "[13] \033[38;5;12m0.735684952\033[0m \033[38;5;10m0.001136587\033[0m \033[38;5;9m0.391203335\033[0m \033[38;5;9m0.462494654\033[0m \033[38;5;10m0.388143982\033[0m \033[38;5;12m0.402485142\033[0m" ,
      "[19] \033[38;5;9m0.178963585\033[0m \033[38;5;9m0.951658754\033[0m")
  )

})

test_that("vector printing to console - integer", {
  set.seed(12345)
  styled_vect <-
    color_vctr(as.integer(c(1, 2, 3, 20)), text_color = c("red", "blue", "green", NA))
  styled_vect_long <-
    color_vctr(
      1:50,
      text_color =    c(
        "blue" ,
        "green",
        "blue" ,
        "blue" ,
        "red"  ,
        "green",
        "blue" ,
        "blue" ,
        "green",
        "blue" ,
        "green",
        "blue" ,
        "blue" ,
        "red"  ,
        "green",
        "blue" ,
        "blue" ,
        "red"  ,
        "blue" ,
        "green",
        "red"  ,
        "blue" ,
        "red"  ,
        "blue" ,
        "green",
        "blue" ,
        "blue" ,
        "green",
        "red"  ,
        "red"  ,
        "green",
        "blue" ,
        "red"  ,
        "red"  ,
        "green",
        "green",
        "red"  ,
        "green",
        "red"  ,
        "red"  ,
        "blue" ,
        "blue" ,
        "green",
        "green",
        "red"  ,
        "green",
        "green",
        "red"  ,
        "blue" ,
        "green"
      )
    )

  expect_equal(
    capture_print(styled_vect),
    "[1] \033[38;5;9m 1\033[0m \033[38;5;12m 2\033[0m \033[38;5;10m 3\033[0m 20"
  )

  expect_equal(
    capture_print(styled_vect_long),
    c(" [1] \033[38;5;12m 1\033[0m \033[38;5;10m 2\033[0m \033[38;5;12m 3\033[0m \033[38;5;12m 4\033[0m \033[38;5;9m 5\033[0m \033[38;5;10m 6\033[0m \033[38;5;12m 7\033[0m \033[38;5;12m 8\033[0m \033[38;5;10m 9\033[0m \033[38;5;12m10\033[0m \033[38;5;10m11\033[0m \033[38;5;12m12\033[0m \033[38;5;12m13\033[0m \033[38;5;9m14\033[0m \033[38;5;10m15\033[0m \033[38;5;12m16\033[0m \033[38;5;12m17\033[0m \033[38;5;9m18\033[0m \033[38;5;12m19\033[0m \033[38;5;10m20\033[0m \033[38;5;9m21\033[0m \033[38;5;12m22\033[0m \033[38;5;9m23\033[0m \033[38;5;12m24\033[0m \033[38;5;10m25\033[0m",
      "[26] \033[38;5;12m26\033[0m \033[38;5;12m27\033[0m \033[38;5;10m28\033[0m \033[38;5;9m29\033[0m \033[38;5;9m30\033[0m \033[38;5;10m31\033[0m \033[38;5;12m32\033[0m \033[38;5;9m33\033[0m \033[38;5;9m34\033[0m \033[38;5;10m35\033[0m \033[38;5;10m36\033[0m \033[38;5;9m37\033[0m \033[38;5;10m38\033[0m \033[38;5;9m39\033[0m \033[38;5;9m40\033[0m \033[38;5;12m41\033[0m \033[38;5;12m42\033[0m \033[38;5;10m43\033[0m \033[38;5;10m44\033[0m \033[38;5;9m45\033[0m \033[38;5;10m46\033[0m \033[38;5;10m47\033[0m \033[38;5;9m48\033[0m \033[38;5;12m49\033[0m \033[38;5;10m50\033[0m")
  )

})

test_that("vector printing to console - character", {
  styled_vect <-
    color_vctr(c("A", "B", "C", "Long Character"), text_color = c("red", "blue", "green", NA))
  styled_vect_long <-
    color_vctr(
      rep(c("A", "B", "C", "Long Character"), 3),
      text_color =   c(
        "blue",
        "green",
        "blue" ,
        "blue",
        "red"  ,
        "green",
        "blue",
        "blue" ,
        "green",
        "blue",
        "green",
        "blue"
      )
    )

  expect_equal(
    capture_print(styled_vect),
    "[1] \033[38;5;9mA             \033[0m \033[38;5;12mB             \033[0m \033[38;5;10mC             \033[0m Long Character"
  )

  expect_equal(
    capture_print(styled_vect_long),
    c(" [1] \033[38;5;12mA             \033[0m \033[38;5;10mB             \033[0m \033[38;5;12mC             \033[0m \033[38;5;12mLong Character\033[0m \033[38;5;9mA             \033[0m",
      " [6] \033[38;5;10mB             \033[0m \033[38;5;12mC             \033[0m \033[38;5;12mLong Character\033[0m \033[38;5;10mA             \033[0m \033[38;5;12mB             \033[0m",
      "[11] \033[38;5;10mC             \033[0m \033[38;5;12mLong Character\033[0m")
  )
})

test_that("vector printing to console - factor", {
  set.seed(12345)
  styled_vect <-
    color_vctr(factor(c("A", "B", "C", "Long Character")), text_color = c("red", "blue", "green", NA))
  styled_vect_long <-
    color_vctr(
      factor(rep(c(
        "A", "B", "C", "Long Character"
      ), 3)),
      text_color =   c(
        "blue",
        "green",
        "blue",
        "blue"  ,
        "red"   ,
        "green",
        "blue"  ,
        "blue"  ,
        "green",
        "blue"  ,
        "green" ,
        "blue"
      )
    )

  expect_equal(
    capture_print(styled_vect),
    c("[1] \033[38;5;9mA             \033[0m \033[38;5;12mB             \033[0m \033[38;5;10mC             \033[0m Long Character",
      "Levels: A B C Long Character" )
  )

  expect_equal(
    capture_print(styled_vect_long),
    c(" [1] \033[38;5;12mA             \033[0m \033[38;5;10mB             \033[0m \033[38;5;12mC             \033[0m \033[38;5;12mLong Character\033[0m \033[38;5;9mA             \033[0m",
      " [6] \033[38;5;10mB             \033[0m \033[38;5;12mC             \033[0m \033[38;5;12mLong Character\033[0m \033[38;5;10mA             \033[0m \033[38;5;12mB             \033[0m",
      "[11] \033[38;5;10mC             \033[0m \033[38;5;12mLong Character\033[0m",
      "Levels: A B C Long Character")
  )

  expect_equal(
    capture_print(styled_vect_long),
    c(" [1] \033[38;5;12mA             \033[0m \033[38;5;10mB             \033[0m \033[38;5;12mC             \033[0m \033[38;5;12mLong Character\033[0m \033[38;5;9mA             \033[0m",
      " [6] \033[38;5;10mB             \033[0m \033[38;5;12mC             \033[0m \033[38;5;12mLong Character\033[0m \033[38;5;10mA             \033[0m \033[38;5;12mB             \033[0m",
      "[11] \033[38;5;10mC             \033[0m \033[38;5;12mLong Character\033[0m",
      "Levels: A B C Long Character")
  )

  expect_equal(
    capture_print(styled_vect, console_width = 10),
    c("[1] \033[38;5;9mA             \033[0m",
    "[2] \033[38;5;12mB             \033[0m",
    "[3] \033[38;5;10mC             \033[0m",
    "[4] Long Character",
    "4  Levels: A ... ")
  )
})

test_that("vector printing to console - character", {
  set.seed(12345)
  styled_vect <-
    color_vctr(c(TRUE, FALSE, TRUE, TRUE), text_color = c("red", "blue", "green", NA))
  styled_vect_long <-
    color_vctr(
      rep(c(TRUE, FALSE, TRUE, TRUE), 6),
      text_color =    c(
        "blue",
        "green",
        "blue" ,
        "blue",
        "red"  ,
        "green",
        "blue",
        "blue" ,
        "green",
        "blue",
        "green",
        "blue" ,
        "blue",
        "red"  ,
        "green",
        "blue",
        "blue" ,
        "red"  ,
        "blue",
        "green",
        "red"  ,
        "blue",
        "red"  ,
        "blue"
      )
    )

  expect_equal(
    capture_print(styled_vect),
    "[1] \033[38;5;9m TRUE\033[0m \033[38;5;12mFALSE\033[0m \033[38;5;10m TRUE\033[0m  TRUE"
  )

  expect_equal(
    capture_print(styled_vect_long),
    c(" [1] \033[38;5;12m TRUE\033[0m \033[38;5;10mFALSE\033[0m \033[38;5;12m TRUE\033[0m \033[38;5;12m TRUE\033[0m \033[38;5;9m TRUE\033[0m \033[38;5;10mFALSE\033[0m \033[38;5;12m TRUE\033[0m \033[38;5;12m TRUE\033[0m \033[38;5;10m TRUE\033[0m \033[38;5;12mFALSE\033[0m \033[38;5;10m TRUE\033[0m \033[38;5;12m TRUE\033[0m",
      "[13] \033[38;5;12m TRUE\033[0m \033[38;5;9mFALSE\033[0m \033[38;5;10m TRUE\033[0m \033[38;5;12m TRUE\033[0m \033[38;5;12m TRUE\033[0m \033[38;5;9mFALSE\033[0m \033[38;5;12m TRUE\033[0m \033[38;5;10m TRUE\033[0m \033[38;5;9m TRUE\033[0m \033[38;5;12mFALSE\033[0m \033[38;5;9m TRUE\033[0m \033[38;5;12m TRUE\033[0m")
  )
})

test_that("vector printing to console - dates", {
  set.seed(12345)
  styled_vect <-
    color_vctr(as.Date(c("1970-01-01","1970-01-02","1970-01-03","1970-01-04")),
               text_color = c("red", "blue", "green", NA))
  styled_vect_long <-
    color_vctr(rep(as.Date(c("1970-01-01","1970-01-02","1970-01-03","1970-01-04")),6),
               text_color = c(
                 "blue",
                 "green",
                 "blue" ,
                 "blue",
                 "red"  ,
                 "green",
                 "blue",
                 "blue" ,
                 "green",
                 "blue",
                 "green",
                 "blue" ,
                 "blue",
                 "red"  ,
                 "green",
                 "blue",
                 "blue" ,
                 "red"  ,
                 "blue",
                 "green",
                 "red"  ,
                 "blue",
                 "red"  ,
                 "blue"
               ))

  expect_equal(
    capture_print(styled_vect),
    "[1] \033[38;5;9m1970-01-01\033[0m \033[38;5;12m1970-01-02\033[0m \033[38;5;10m1970-01-03\033[0m 1970-01-04"
  )

  expect_equal(
    capture_print(styled_vect_long),
    c(" [1] \033[38;5;12m1970-01-01\033[0m \033[38;5;10m1970-01-02\033[0m \033[38;5;12m1970-01-03\033[0m \033[38;5;12m1970-01-04\033[0m \033[38;5;9m1970-01-01\033[0m \033[38;5;10m1970-01-02\033[0m" ,
      " [7] \033[38;5;12m1970-01-03\033[0m \033[38;5;12m1970-01-04\033[0m \033[38;5;10m1970-01-01\033[0m \033[38;5;12m1970-01-02\033[0m \033[38;5;10m1970-01-03\033[0m \033[38;5;12m1970-01-04\033[0m",
      "[13] \033[38;5;12m1970-01-01\033[0m \033[38;5;9m1970-01-02\033[0m \033[38;5;10m1970-01-03\033[0m \033[38;5;12m1970-01-04\033[0m \033[38;5;12m1970-01-01\033[0m \033[38;5;9m1970-01-02\033[0m" ,
      "[19] \033[38;5;12m1970-01-03\033[0m \033[38;5;10m1970-01-04\033[0m \033[38;5;9m1970-01-01\033[0m \033[38;5;12m1970-01-02\033[0m \033[38;5;9m1970-01-03\033[0m \033[38;5;12m1970-01-04\033[0m"  )
  )
})

