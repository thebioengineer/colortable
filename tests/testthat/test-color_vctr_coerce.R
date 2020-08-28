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

test_that("pivoting",{

  x <- tibble::tibble(
    col = rep(c("A","B"), times = 3),
    row = rep(c("A","B","C"), each = 2),
    c_vctr = color_vctr(1:6, text_color = c("blue","yellow","orange","green","pink","brown"))
  )

  x_wide <- x %>%
    tidyr::pivot_wider(
      names_from = col,
      values_from = c_vctr
    )

  x_long <- x_wide %>%
    tidyr::pivot_longer(
      cols = c("A","B"),
      names_to = "col",
      values_to = "c_vctr"
    )


  expect_equal(
    x_wide,
    tibble::tibble(
      row = c("A","B","C"),
      A = color_vctr(c(1,3,5), text_color = c("blue","orange","pink")),
      B = color_vctr(c(2,4,6), text_color = c("yellow","green","brown"))
    )
  )

  expect_equal(
    x_long,
    tibble::tibble(
      row = rep(c("A","B","C"), each = 2),
      col = rep(c("A","B"), times = 3),
      c_vctr = color_vctr(1:6, text_color = c("blue","yellow","orange","green","pink","brown"))
    )
  )

})

test_that("integer to anything else",{
  base <- color_vctr(1L, text_color = "green")

  ## integer
  expect_equal(
    vec_c(base, 2L),
    color_vctr(c(1L,2), text_color = c("green",NA))
  )
  expect_equal(
    vec_c(base, color_vctr(2L, text_color = "blue")),
    color_vctr(c(1L,2), text_color = c("green","blue"))
  )

  ## double
  expect_equal(
    vec_c(base, 1),
    color_vctr(c(1,1), text_color = c("green",NA))
  )
  expect_equal(
    vec_c(base, color_vctr(2, text_color = "blue")),
    color_vctr(c(1,2), text_color = c("green","blue"))
  )

  ## logical
  expect_equal(
    vec_c(base, TRUE),
    color_vctr(c(1,1), text_color = c("green",NA))
  )
  expect_equal(
    vec_c(base, color_vctr(TRUE, text_color = "blue")),
    color_vctr(c(1,1), text_color = c("green","blue"))
  )
  ## complex
  expect_equal(
    vec_c(base, 0i ^ (1)),
    color_vctr(c(1,0i ^ (1)), text_color = c("green",NA))
  )
  expect_equal(
    vec_c(base, color_vctr(0i ^ (1), text_color = "blue")),
    color_vctr(c(1,0i ^ (1)), text_color = c("green","blue"))
  )

  ## char
  expect_error(
    vec_c(base, "A")
  )
  expect_error(
    vec_c(base, color_vctr("A", text_color = "blue"))
  )

  ## factor
  expect_error(
    vec_c(base, factor("A"))
  )
  expect_error(
    vec_c(base, color_vctr(factor("A"), text_color = "blue"))
  )

  ##ordered
  expect_error(
    vec_c(base, factor("A",ordered = TRUE))
  )
  expect_error(
    vec_c(base, color_vctr(factor("A",ordered = TRUE), text_color = "blue"))
  )

  ## raw
  expect_error(
    vec_c(base, raw(00))
  )
  expect_error(
    vec_c(base, color_vctr(raw(00), text_color = "blue"))
  )

  ## Date
  expect_error(
    vec_c(base, as.Date("1970-01-01"))
  )
  expect_error(
    vec_c(base, color_vctr(as.Date("1970-01-01"), text_color = "blue"))
  )

  ## difftime
  expect_error(
    vec_c(base, as.difftime(c("0:3:20", "11:23:15")))
  )
  expect_error(
    vec_c(base, color_vctr(as.difftime(c("0:3:20", "11:23:15")), text_color = "blue"))
  )

  ## POSIXct
  expect_error(
    vec_c(base, as.POSIXct("1970-01-01"))
  )
  expect_error(
    vec_c(base, color_vctr(as.POSIXct("1970-01-01"), text_color = "blue"))
  )

  ## POSIXlt
  expect_error(
    vec_c(base, as.POSIXlt("1970-01-01"))
  )
  expect_error(
    vec_c(base, color_vctr(as.POSIXlt("1970-01-01"), text_color = "blue"))
  )

  ## list
  expect_equal(
    vec_c(base, list(1)),
    list(base, 1)
  )
  expect_equal(
    vec_c(list(1), base),
    list(1, base)
  )
})

test_that("double to anything else",{
  base <- color_vctr(1, text_color = "green")

  ## integer
  expect_equal(
    vec_c(base, 2L),
    color_vctr(c(1,2), text_color = c("green",NA))
  )
  expect_equal(
    vec_c(2L,base),
    color_vctr(c(2,1), text_color = c(NA,"green"))
  )
  expect_equal(
    vec_c(base, color_vctr(2L, text_color = "blue")),
    color_vctr(c(1,2), text_color = c("green","blue"))
  )
  expect_equal(
    vec_c(color_vctr(2L, text_color = "blue"),base),
    color_vctr(c(2,1), text_color = c("blue","green"))
  )
  ## double
  expect_equal(
    vec_c(base, 1),
    color_vctr(c(1,1), text_color = c("green",NA))
  )
  expect_equal(
    vec_c(base, color_vctr(2, text_color = "blue")),
    color_vctr(c(1,2), text_color = c("green","blue"))
  )

  ## logical
  expect_equal(
    vec_c(base, TRUE),
    color_vctr(c(1,1), text_color = c("green",NA))
  )
  expect_equal(
    vec_c(base, color_vctr(TRUE, text_color = "blue")),
    color_vctr(c(1,1), text_color = c("green","blue"))
  )

  ## complex
  expect_equal(
    vec_c(base, 0i ^ (1)),
    color_vctr(c(1,0i ^ (1)), text_color = c("green",NA))
  )
  expect_equal(
    vec_c(base, color_vctr(0i ^ (1), text_color = "blue")),
    color_vctr(c(1,0i ^ (1)), text_color = c("green","blue"))
  )

  ## char
  expect_error(
    vec_c(base, "A")
  )
  expect_error(
    vec_c(base, color_vctr("A", text_color = "blue"))
  )

  ## factor
  expect_error(
    vec_c(base, factor("A"))
  )
  expect_error(
    vec_c(base, color_vctr(factor("A"), text_color = "blue"))
  )

  ##ordered
  expect_error(
    vec_c(base, factor("A",ordered = TRUE))
  )
  expect_error(
    vec_c(base, color_vctr(factor("A",ordered = TRUE), text_color = "blue"))
  )

  ## raw
  expect_error(
    vec_c(base, raw(00))
  )
  expect_error(
    vec_c(base, color_vctr(raw(00), text_color = "blue"))
  )

  ## Date
  expect_error(
    vec_c(base, as.Date("1970-01-01"))
  )
  expect_error(
    vec_c(base, color_vctr(as.Date("1970-01-01"), text_color = "blue"))
  )

  ## difftime
  expect_error(
    vec_c(base, as.difftime(c("0:3:20", "11:23:15")))
  )
  expect_error(
    vec_c(base, color_vctr(as.difftime(c("0:3:20", "11:23:15")), text_color = "blue"))
  )

  ## POSIXct
  expect_error(
    vec_c(base, as.POSIXct("1970-01-01"))
  )
  expect_error(
    vec_c(base, color_vctr(as.POSIXct("1970-01-01"), text_color = "blue"))
  )

  ## POSIXlt
  expect_error(
    vec_c(base, as.POSIXlt("1970-01-01"))
  )
  expect_error(
    vec_c(base, color_vctr(as.POSIXlt("1970-01-01"), text_color = "blue"))
  )

  ## list
  expect_equal(
    vec_c(base, list(1)),
    list(base, 1)
  )
  expect_equal(
    vec_c(list(1), base),
    list(1, base)
  )
})

test_that("logical to anything else",{
  base <- color_vctr(TRUE, text_color = "green")

  ## integer
  expect_equal(
    vec_c(base, 2L),
    color_vctr(c(1L,2), text_color = c("green",NA))
  )
  expect_equal(
    vec_c(base, color_vctr(2L, text_color = "blue")),
    color_vctr(c(1L,2), text_color = c("green","blue"))
  )

  ## double
  expect_equal(
    vec_c(base, 1),
    color_vctr(c(1,1), text_color = c("green",NA))
  )
  expect_equal(
    vec_c(base, color_vctr(2, text_color = "blue")),
    color_vctr(c(1,2), text_color = c("green","blue"))
  )

  ## logical
  expect_equal(
    vec_c(base, TRUE),
    color_vctr(c(TRUE,TRUE), text_color = c("green",NA))
  )
  expect_equal(
    vec_c(base, color_vctr(TRUE, text_color = "blue")),
    color_vctr(c(TRUE,TRUE), text_color = c("green","blue"))
  )

  ## complex
  expect_error(
    vec_c(base, 0i ^ (1))
  )
  expect_error(
    vec_c(base, color_vctr(0i ^ (1), text_color = "blue"))
  )

  ## char
  expect_error(
    vec_c(base, "A")
  )
  expect_error(
    vec_c(base, color_vctr("A", text_color = "blue"))
  )

  ## factor
  expect_error(
    vec_c(base, factor("A"))
  )
  expect_error(
    vec_c(base, color_vctr(factor("A"), text_color = "blue"))
  )

  ##ordered
  expect_error(
    vec_c(base, factor("A",ordered = TRUE))
  )
  expect_error(
    vec_c(base, color_vctr(factor("A",ordered = TRUE), text_color = "blue"))
  )

  ## raw
  expect_error(
    vec_c(base, raw(00))
  )
  expect_error(
    vec_c(base, color_vctr(raw(00), text_color = "blue"))
  )

  ## Date
  expect_error(
    vec_c(base, as.Date("1970-01-01"))
  )
  expect_error(
    vec_c(base, color_vctr(as.Date("1970-01-01"), text_color = "blue"))
  )

  ## difftime
  expect_error(
    vec_c(base, as.difftime(c("0:3:20", "11:23:15")))
  )
  expect_error(
    vec_c(base, color_vctr(as.difftime(c("0:3:20", "11:23:15")), text_color = "blue"))
  )

  ## POSIXct
  expect_error(
    vec_c(base, as.POSIXct("1970-01-01"))
  )
  expect_error(
    vec_c(base, color_vctr(as.POSIXct("1970-01-01"), text_color = "blue"))
  )

  ## POSIXlt
  expect_error(
    vec_c(base, as.POSIXlt("1970-01-01"))
  )
  expect_error(
    vec_c(base, color_vctr(as.POSIXlt("1970-01-01"), text_color = "blue"))
  )

  ## list
  expect_equal(
    vec_c(base, list(1)),
    list(base, 1)
  )
  expect_equal(
    vec_c(list(1), base),
    list(1, base)
  )
})

test_that("complex to anything else",{
  base <- color_vctr(0i ^ (1), text_color = "green")

  ## integer
  expect_equal(
    vec_c(base, 2L),
    color_vctr(c(0i ^ (1),2), text_color = c("green",NA))
  )
  expect_equal(
    vec_c(base, color_vctr(2L, text_color = "blue")),
    color_vctr(c(0i ^ (1),2), text_color = c("green","blue"))
  )

  ## double
  expect_equal(
    vec_c(base, 1),
    color_vctr(c(0i ^ (1),1), text_color = c("green",NA))
  )
  expect_equal(
    vec_c(base, color_vctr(2, text_color = "blue")),
    color_vctr(c(0i ^ (1),2), text_color = c("green","blue"))
  )

  ## logical
  expect_error(
    vec_c(base, TRUE)
  )
  expect_error(
    vec_c(base, color_vctr(TRUE, text_color = "blue"))
  )

  ## complex
  expect_equal(
    vec_c(base, 0i ^ (1)),
    color_vctr(c(0i ^ (1),0i ^ (1)), text_color = c("green",NA))
  )
  expect_equal(
    vec_c(base, color_vctr(0i ^ (1), text_color = "blue")),
    color_vctr(c(0i ^ (1),0i ^ (1)), text_color = c("green","blue"))
  )

  ## char
  expect_error(
    vec_c(base, "A")
  )
  expect_error(
    vec_c(base, color_vctr("A", text_color = "blue"))
  )

  ## factor
  expect_error(
    vec_c(base, factor("A"))
  )
  expect_error(
    vec_c(base, color_vctr(factor("A"), text_color = "blue"))
  )

  ##ordered
  expect_error(
    vec_c(base, factor("A",ordered = TRUE))
  )
  expect_error(
    vec_c(base, color_vctr(factor("A",ordered = TRUE), text_color = "blue"))
  )

  ## raw
  expect_error(
    vec_c(base, raw(00))
  )
  expect_error(
    vec_c(base, color_vctr(raw(00), text_color = "blue"))
  )

  ## Date
  expect_error(
    vec_c(base, as.Date("1970-01-01"))
  )
  expect_error(
    vec_c(base, color_vctr(as.Date("1970-01-01"), text_color = "blue"))
  )

  ## difftime
  expect_error(
    vec_c(base, as.difftime(c("0:3:20", "11:23:15")))
  )
  expect_error(
    vec_c(base, color_vctr(as.difftime(c("0:3:20", "11:23:15")), text_color = "blue"))
  )

  ## POSIXct
  expect_error(
    vec_c(base, as.POSIXct("1970-01-01"))
  )
  expect_error(
    vec_c(base, color_vctr(as.POSIXct("1970-01-01"), text_color = "blue"))
  )

  ## POSIXlt
  expect_error(
    vec_c(base, as.POSIXlt("1970-01-01"))
  )
  expect_error(
    vec_c(base, color_vctr(as.POSIXlt("1970-01-01"), text_color = "blue"))
  )

  ## list
  expect_equal(
    vec_c(base, list(1)),
    list(base, 1)
  )
  expect_equal(
    vec_c(list(1), base),
    list(1, base)
  )
})

test_that("character to anything else",{
  base <- color_vctr("A", text_color = "green")

  ## integer
  expect_error(
    vec_c(base, 2L)
  )
  expect_error(
    vec_c(base, color_vctr(2L, text_color = "blue"))
  )

  ## double
  expect_error(
    vec_c(base, TRUE),
  )
  expect_error(
    vec_c(base, color_vctr(2, text_color = "blue"))
  )

  ## logical
  expect_error(
    vec_c(base, TRUE)
  )
  expect_error(
    vec_c(base, color_vctr(TRUE, text_color = "blue"))
  )

  ## complex
  expect_error(
    vec_c(base, 0i ^ (1)),
  )
  expect_error(
    vec_c(base, color_vctr(0i ^ (1), text_color = "blue"))
  )

  ## char
  expect_equal(
    vec_c(base, "A"),
    color_vctr(c("A","A"), text_color = c("green",NA))
  )
  expect_equal(
    vec_c(base, color_vctr("A", text_color = "blue")),
    color_vctr(c("A","A"), text_color = c("green","blue"))
  )

  ## factor
  expect_equal(
    vec_c(base, factor("A")),
    color_vctr(c("A","A"), text_color = c("green",NA))

  )
  expect_equal(
    vec_c(base, color_vctr(factor("A"), text_color = "blue")),
    color_vctr(c("A","A"), text_color = c("green","blue"))
  )

  ##ordered
  expect_equal(
    vec_c(base, factor("A",ordered = TRUE)),
    color_vctr(c("A","A"), text_color = c("green",NA))
  )
  expect_equal(
    vec_c(base, color_vctr(factor("A",ordered = TRUE), text_color = "blue")),
    color_vctr(c("A","A"), text_color = c("green","blue"))
  )

  ## raw
  expect_error(
    vec_c(base, raw(00))
  )
  expect_error(
    vec_c(base, color_vctr(raw(00), text_color = "blue")),
  )

  ## Date
  expect_error(
    vec_c(base, as.Date("1970-01-01"))
  )
  expect_error(
    vec_c(base, color_vctr(as.Date("1970-01-01"), text_color = "blue"))
  )

  ## difftime
  expect_error(
    vec_c(base, as.difftime(c("0:3:20", "11:23:15")))
  )
  expect_error(
    vec_c(base, color_vctr(as.difftime(c("0:3:20", "11:23:15")), text_color = "blue"))
  )

  ## POSIXct
  expect_error(
    vec_c(base, as.POSIXct("1970-01-01"))
  )
  expect_error(
    vec_c(base, color_vctr(as.POSIXct("1970-01-01"), text_color = "blue"))
  )

  ## POSIXlt
  expect_error(
    vec_c(base, as.POSIXlt("1970-01-01"))
  )
  expect_error(
    vec_c(base, color_vctr(as.POSIXlt("1970-01-01"), text_color = "blue"))
  )
  ## list
  expect_equal(
    vec_c(base, list(1)),
    list(base, 1)
  )
  expect_equal(
    vec_c(list(1), base),
    list(1, base)
  )
})

test_that("factor to anything else",{
  base <- color_vctr(factor("A"), text_color = "green")

  ## integer
  expect_error(
    vec_c(base, 2L)
  )
  expect_error(
    vec_c(base, color_vctr(2L, text_color = "blue"))
  )

  ## double
  expect_error(
    vec_c(base, TRUE),
  )
  expect_error(
    vec_c(base, color_vctr(2, text_color = "blue"))
  )

  ## logical
  expect_error(
    vec_c(base, TRUE)
  )
  expect_error(
    vec_c(base, color_vctr(TRUE, text_color = "blue"))
  )

  ## complex
  expect_error(
    vec_c(base, 0i ^ (1)),
  )
  expect_error(
    vec_c(base, color_vctr(0i ^ (1), text_color = "blue"))
  )

  ## char
  expect_equal(
    vec_c(base, "A"),
    color_vctr(c("A","A"), text_color = c("green",NA))
  )
  expect_equal(
    vec_c(base, color_vctr("A", text_color = "blue")),
    color_vctr(c("A","A"), text_color = c("green","blue"))
  )

  ## factor
  expect_equal(
    vec_c(base, factor("A")),
    color_vctr(factor(c("A","A")), text_color = c("green",NA))
  )
  expect_equal(
    vec_c(base, color_vctr(factor("A"), text_color = "blue")),
    color_vctr(factor(c("A","A")), text_color = c("green","blue"))
  )

  ##ordered
  expect_error(
    vec_c(base, factor("A",ordered = TRUE))
  )
  expect_error(
    vec_c(base, color_vctr(factor("A",ordered = TRUE), text_color = "blue"))
  )

  ## raw
  expect_error(
    vec_c(base, raw(00))
  )
  expect_error(
    vec_c(base, color_vctr(raw(00), text_color = "blue")),
  )

  ## Date
  expect_error(
    vec_c(base, as.Date("1970-01-01"))
  )
  expect_error(
    vec_c(base, color_vctr(as.Date("1970-01-01"), text_color = "blue"))
  )

  ## difftime
  expect_error(
    vec_c(base, as.difftime(c("0:3:20", "11:23:15")))
  )
  expect_error(
    vec_c(base, color_vctr(as.difftime(c("0:3:20", "11:23:15")), text_color = "blue"))
  )

  ## POSIXct
  expect_error(
    vec_c(base, as.POSIXct("1970-01-01"))
  )
  expect_error(
    vec_c(base, color_vctr(as.POSIXct("1970-01-01"), text_color = "blue"))
  )

  ## POSIXlt
  expect_error(
    vec_c(base, as.POSIXlt("1970-01-01"))
  )
  expect_error(
    vec_c(base, color_vctr(as.POSIXlt("1970-01-01"), text_color = "blue"))
  )
  ## list
  expect_equal(
    vec_c(base, list(1)),
    list(base, 1)
  )
  expect_equal(
    vec_c(list(1), base),
    list(1, base)
  )
})

test_that("ordered factor to anything else",{
  base <- color_vctr(factor("A", ordered = TRUE), text_color = "green")

  ## integer
  expect_error(
    vec_c(base, 2L)
  )
  expect_error(
    vec_c(base, color_vctr(2L, text_color = "blue"))
  )

  ## double
  expect_error(
    vec_c(base, TRUE),
  )
  expect_error(
    vec_c(base, color_vctr(2, text_color = "blue"))
  )

  ## logical
  expect_error(
    vec_c(base, TRUE)
  )
  expect_error(
    vec_c(base, color_vctr(TRUE, text_color = "blue"))
  )

  ## complex
  expect_error(
    vec_c(base, 0i ^ (1)),
  )
  expect_error(
    vec_c(base, color_vctr(0i ^ (1), text_color = "blue"))
  )

  ## char
  expect_equal(
    vec_c(base, "A"),
    color_vctr(c("A","A"), text_color = c("green",NA))
  )
  expect_equal(
    vec_c(base, color_vctr("A", text_color = "blue")),
    color_vctr(c("A","A"), text_color = c("green","blue"))
  )

  ## factor
  expect_error(
    vec_c(base, factor("A"))
  )
  expect_error(
    vec_c(base, color_vctr(factor("A"), text_color = "blue"))
  )

  ##ordered
  expect_equal(
    vec_c(base, factor("A",ordered = TRUE)),
    color_vctr(factor(c("A","A"), ordered = TRUE), text_color = c("green",NA))
  )
  expect_equal(
    vec_c(base, color_vctr(factor("A",ordered = TRUE), text_color = "blue")),
    color_vctr(factor(c("A","A"), ordered = TRUE), text_color = c("green","blue"))
  )

  ## raw
  expect_error(
    vec_c(base, raw(00))
  )
  expect_error(
    vec_c(base, color_vctr(raw(00), text_color = "blue"))
  )

  ## Date
  expect_error(
    vec_c(base, as.Date("1970-01-01"))
  )
  expect_error(
    vec_c(base, color_vctr(as.Date("1970-01-01"), text_color = "blue"))
  )

  ## difftime
  expect_error(
    vec_c(base, as.difftime(c("0:3:20", "11:23:15")))
  )
  expect_error(
    vec_c(base, color_vctr(as.difftime(c("0:3:20", "11:23:15")), text_color = "blue"))
  )

  ## POSIXct
  expect_error(
    vec_c(base, as.POSIXct("1970-01-01"))
  )
  expect_error(
    vec_c(base, color_vctr(as.POSIXct("1970-01-01"), text_color = "blue"))
  )

  ## POSIXlt
  expect_error(
    vec_c(base, as.POSIXlt("1970-01-01"))
  )
  expect_error(
    vec_c(base, color_vctr(as.POSIXlt("1970-01-01"), text_color = "blue"))
  )
  ## list
  expect_equal(
    vec_c(base, list(1)),
    list(base, 1)
  )
  expect_equal(
    vec_c(list(1), base),
    list(1, base)
  )
})

test_that("raw to anything else",{
  base <- color_vctr(raw(01), text_color = "green")

  ## integer
  expect_error(
    vec_c(base, 2L)
  )
  expect_error(
    vec_c(base, color_vctr(2L, text_color = "blue"))
  )

  ## double
  expect_error(
    vec_c(base, 1)
  )
  expect_error(
    vec_c(base, color_vctr(2, text_color = "blue"))
  )

  ## logical
  expect_error(
    vec_c(base, TRUE)
  )
  expect_error(
    vec_c(base, color_vctr(TRUE, text_color = "blue"))
  )

  ## complex
  expect_error(
    vec_c(base, 0i ^ (1)),
  )
  expect_error(
    vec_c(base, color_vctr(0i ^ (1), text_color = "blue"))
  )

  ## char
  expect_error(
    vec_c(base, "A")
  )
  expect_error(
    vec_c(base, color_vctr("A", text_color = "blue"))
  )

  ## factor
  expect_error(
    vec_c(base, factor("A"))
  )
  expect_error(
    vec_c(base, color_vctr(factor("A"), text_color = "blue"))
  )

  ##ordered
  expect_error(
    vec_c(base, factor("A",ordered = TRUE))
  )
  expect_error(
    vec_c(base, color_vctr(factor("A",ordered = TRUE), text_color = "blue"))
  )

  ## raw
  expect_equal(
    vec_c(base, raw(01)),
    color_vctr(c(raw(01), raw(01)), text_color = c("green", NA))
  )
  expect_equal(
    vec_c(base, color_vctr(raw(01), text_color = "blue")),
    color_vctr(c(raw(01), raw(01)), text_color = c("green","blue"))
  )

  ## Date
  expect_error(
    vec_c(base, as.Date("1970-01-01"))
  )
  expect_error(
    vec_c(base, color_vctr(as.Date("1970-01-01"), text_color = "blue"))
  )

  ## difftime
  expect_error(
    vec_c(base, as.difftime(c("0:3:20", "11:23:15")))
  )
  expect_error(
    vec_c(base, color_vctr(as.difftime(c("0:3:20", "11:23:15")), text_color = "blue"))
  )

  ## POSIXct
  expect_error(
    vec_c(base, as.POSIXct("1970-01-01"))
  )
  expect_error(
    vec_c(base, color_vctr(as.POSIXct("1970-01-01"), text_color = "blue"))
  )

  ## POSIXlt
  expect_error(
    vec_c(base, as.POSIXlt("1970-01-01"))
  )
  expect_error(
    vec_c(base, color_vctr(as.POSIXlt("1970-01-01"), text_color = "blue"))
  )

  ## list
  expect_equal(
    vec_c(base, list(1)),
    list(base, 1)
  )
  expect_equal(
    vec_c(list(1), base),
    list(1, base)
  )
})

test_that("Date to anything else",{
  base <- color_vctr(as.Date("1970-01-01"), text_color = "green")

  ## integer
  expect_error(
    vec_c(base, 2L)
  )
  expect_error(
    vec_c(base, color_vctr(2L, text_color = "blue"))
  )

  ## double
  expect_error(
    vec_c(base, 1)
  )
  expect_error(
    vec_c(base, color_vctr(2, text_color = "blue"))
  )

  ## logical
  expect_error(
    vec_c(base, TRUE)
  )
  expect_error(
    vec_c(base, color_vctr(TRUE, text_color = "blue"))
  )

  ## complex
  expect_error(
    vec_c(base, 0i ^ (1)),
  )
  expect_error(
    vec_c(base, color_vctr(0i ^ (1), text_color = "blue"))
  )

  ## char
  expect_error(
    vec_c(base, "A")
  )
  expect_error(
    vec_c(base, color_vctr("A", text_color = "blue"))
  )

  ## factor
  expect_error(
    vec_c(base, factor("A"))
  )
  expect_error(
    vec_c(base, color_vctr(factor("A"), text_color = "blue"))
  )

  ##ordered
  expect_error(
    vec_c(base, factor("A",ordered = TRUE))
  )
  expect_error(
    vec_c(base, color_vctr(factor("A",ordered = TRUE), text_color = "blue"))
  )

  ## raw
  expect_error(
    vec_c(base, raw(00))
  )
  expect_error(
    vec_c(base, color_vctr(raw(00), text_color = "blue"))
  )

  ## Date
  expect_equal(
    vec_c(base, as.Date("1970-01-01")),
    color_vctr(c(as.Date("1970-01-01"), as.Date("1970-01-01")), text_color = c("green",NA))
  )
  expect_equal(
    vec_c(base, color_vctr(as.Date("1970-01-01"), text_color = "blue")),
    color_vctr(c(as.Date("1970-01-01"), as.Date("1970-01-01")), text_color = c("green","blue"))
  )

  ## difftime
  expect_error(
    vec_c(base, as.difftime(c("0:3:20", "11:23:15")))
  )
  expect_error(
    vec_c(base, color_vctr(as.difftime(c("0:3:20", "11:23:15")), text_color = "blue"))
  )

  ## POSIXct
  expect_equal(
    vec_c(base, as.POSIXct("1970-01-01")),
    color_vctr(c(as.POSIXct("1970-01-01"), as.POSIXct("1970-01-01")), text_color = c("green",NA))
  )
  expect_equal(
    vec_c(base, color_vctr(as.POSIXct("1970-01-01"), text_color = "blue")),
    color_vctr(c(as.POSIXct("1970-01-01"), as.POSIXct("1970-01-01")), text_color = c("green","blue"))
  )

  ## POSIXlt
  expect_equal(
    vec_c(base, as.POSIXlt("1970-01-01")),
    color_vctr(c(as.POSIXlt("1970-01-01"), as.POSIXlt("1970-01-01")), text_color = c("green",NA))
  )
  expect_equal(
    vec_c(base, color_vctr(as.POSIXlt("1970-01-01"), text_color = "blue")),
    color_vctr(c(as.POSIXlt("1970-01-01"), as.POSIXlt("1970-01-01")), text_color = c("green","blue"))
  )

  ## list
  expect_equal(
    vec_c(base, list(1)),
    list(base, 1)
  )
  expect_equal(
    vec_c(list(1), base),
    list(1, base)
  )
})

test_that("difftime to anything else",{
  base <- color_vctr(as.difftime(c("0:3:20")), text_color = "green")

  ## integer
  expect_error(
    vec_c(base, 2L)
  )
  expect_error(
    vec_c(base, color_vctr(2L, text_color = "blue"))
  )

  ## double
  expect_error(
    vec_c(base, 1)
  )
  expect_error(
    vec_c(base, color_vctr(2, text_color = "blue"))
  )

  ## logical
  expect_error(
    vec_c(base, TRUE)
  )
  expect_error(
    vec_c(base, color_vctr(TRUE, text_color = "blue"))
  )

  ## complex
  expect_error(
    vec_c(base, 0i ^ (1)),
  )
  expect_error(
    vec_c(base, color_vctr(0i ^ (1), text_color = "blue"))
  )

  ## char
  expect_error(
    vec_c(base, "A")
  )
  expect_error(
    vec_c(base, color_vctr("A", text_color = "blue"))
  )

  ## factor
  expect_error(
    vec_c(base, factor("A"))
  )
  expect_error(
    vec_c(base, color_vctr(factor("A"), text_color = "blue"))
  )

  ##ordered
  expect_error(
    vec_c(base, factor("A",ordered = TRUE))
  )
  expect_error(
    vec_c(base, color_vctr(factor("A",ordered = TRUE), text_color = "blue"))
  )

  ## raw
  expect_error(
    vec_c(base, raw(00))
  )
  expect_error(
    vec_c(base, color_vctr(raw(00), text_color = "blue"))
  )

  ## Date
  expect_error(
    vec_c(base, as.Date("1970-01-01"))
  )
  expect_error(
    vec_c(base, color_vctr(as.Date("1970-01-01"), text_color = "blue"))
  )

  ## difftime
  expect_equal(
    vec_c(base, as.difftime(c("0:5:20"))),
    color_vctr(as.difftime(c("0:3:20", "0:5:20")), text_color = c("green",NA))
  )
  expect_equal(
    vec_c(base, color_vctr(as.difftime(c("0:5:20")), text_color = "blue")),
    color_vctr(as.difftime(c("0:3:20", "0:5:20")), text_color = c("green","blue"))

  )

  ## POSIXct
  expect_error(
    vec_c(base, as.POSIXct("1970-01-01"))
  )
  expect_error(
    vec_c(base, color_vctr(as.POSIXct("1970-01-01"), text_color = "blue"))
  )

  ## POSIXlt
  expect_error(
    vec_c(base, as.POSIXlt("1970-01-01"))
  )
  expect_error(
    vec_c(base, color_vctr(as.POSIXlt("1970-01-01"), text_color = "blue"))
  )

  ## list
  expect_equal(
    vec_c(base, list(1)),
    list(base, 1)
  )
  expect_equal(
    vec_c(list(1), base),
    list(1, base)
  )
})

test_that("POSIXct to anything else",{
  base <- color_vctr(as.POSIXct("1970-01-01"), text_color = "green")

  ## integer
  expect_error(
    vec_c(base, 2L)
  )
  expect_error(
    vec_c(base, color_vctr(2L, text_color = "blue"))
  )

  ## double
  expect_error(
    vec_c(base, 1)
  )
  expect_error(
    vec_c(base, color_vctr(2, text_color = "blue"))
  )

  ## logical
  expect_error(
    vec_c(base, TRUE)
  )
  expect_error(
    vec_c(base, color_vctr(TRUE, text_color = "blue"))
  )

  ## complex
  expect_error(
    vec_c(base, 0i ^ (1)),
  )
  expect_error(
    vec_c(base, color_vctr(0i ^ (1), text_color = "blue"))
  )

  ## char
  expect_error(
    vec_c(base, "A")
  )
  expect_error(
    vec_c(base, color_vctr("A", text_color = "blue"))
  )

  ## factor
  expect_error(
    vec_c(base, factor("A"))
  )
  expect_error(
    vec_c(base, color_vctr(factor("A"), text_color = "blue"))
  )

  ##ordered
  expect_error(
    vec_c(base, factor("A",ordered = TRUE))
  )
  expect_error(
    vec_c(base, color_vctr(factor("A",ordered = TRUE), text_color = "blue"))
  )

  ## raw
  expect_error(
    vec_c(base, raw(00))
  )
  expect_error(
    vec_c(base, color_vctr(raw(00), text_color = "blue"))
  )

  ## Date
  expect_equal(
    vec_c(base, as.Date("1970-01-01")),
    color_vctr(c(as.POSIXct("1970-01-01"), as.POSIXct("1970-01-01")), text_color = c("green",NA))

  )
  expect_equal(
    vec_c(base, color_vctr(as.Date("1970-01-01"), text_color = "blue")),
    color_vctr(c(as.POSIXct("1970-01-01"), as.POSIXct("1970-01-01")), text_color = c("green","blue"))
  )

  ## difftime
  expect_error(
    vec_c(base, as.difftime(c("0:5:20")))
  )
  expect_error(
    vec_c(base, color_vctr(as.difftime(c("0:5:20")), text_color = "blue"))
  )

  ## POSIXct
  expect_equal(
    vec_c(base, as.POSIXct("1970-01-01")),
    color_vctr(c(as.POSIXct("1970-01-01"), as.POSIXct("1970-01-01")), text_color = c("green",NA))
  )
  expect_equal(
    vec_c(base, color_vctr(as.POSIXct("1970-01-01"), text_color = "blue")),
    color_vctr(c(as.POSIXct("1970-01-01"), as.POSIXct("1970-01-01")), text_color = c("green","blue"))
  )

  ## POSIXlt
  expect_equal(
    vec_c(base, as.POSIXlt("1970-01-01")),
    color_vctr(c(as.POSIXlt("1970-01-01"), as.POSIXlt("1970-01-01")), text_color = c("green",NA))
  )
  expect_equal(
    vec_c(base, color_vctr(as.POSIXlt("1970-01-01"), text_color = "blue")),
    color_vctr(c(as.POSIXlt("1970-01-01"), as.POSIXlt("1970-01-01")), text_color = c("green","blue"))
  )

  ## list
  expect_equal(
    vec_c(base, list(1)),
    list(base, 1)
  )
  expect_equal(
    vec_c(list(1), base),
    list(1, base)
  )
})

test_that("POSIXlt to anything else",{
  base <- color_vctr(as.POSIXlt("1970-01-01"), text_color = "green")

  ## integer
  expect_error(
    vec_c(base, 2L)
  )
  expect_error(
    vec_c(base, color_vctr(2L, text_color = "blue"))
  )

  ## double
  expect_error(
    vec_c(base, 1)
  )
  expect_error(
    vec_c(base, color_vctr(2, text_color = "blue"))
  )

  ## logical
  expect_error(
    vec_c(base, TRUE)
  )
  expect_error(
    vec_c(base, color_vctr(TRUE, text_color = "blue"))
  )

  ## complex
  expect_error(
    vec_c(base, 0i ^ (1)),
  )
  expect_error(
    vec_c(base, color_vctr(0i ^ (1), text_color = "blue"))
  )

  ## char
  expect_error(
    vec_c(base, "A")
  )
  expect_error(
    vec_c(base, color_vctr("A", text_color = "blue"))
  )

  ## factor
  expect_error(
    vec_c(base, factor("A"))
  )
  expect_error(
    vec_c(base, color_vctr(factor("A"), text_color = "blue"))
  )

  ##ordered
  expect_error(
    vec_c(base, factor("A",ordered = TRUE))
  )
  expect_error(
    vec_c(base, color_vctr(factor("A",ordered = TRUE), text_color = "blue"))
  )

  ## raw
  expect_error(
    vec_c(base, raw(00))
  )
  expect_error(
    vec_c(base, color_vctr(raw(00), text_color = "blue"))
  )

  ## Date
  expect_equal(
    vec_c(base, as.Date("1970-01-01")),
    color_vctr(c(as.POSIXct("1970-01-01"), as.POSIXct("1970-01-01")), text_color = c("green",NA))

  )
  expect_equal(
    vec_c(base, color_vctr(as.Date("1970-01-01"), text_color = "blue")),
    color_vctr(c(as.POSIXct("1970-01-01"), as.POSIXct("1970-01-01")), text_color = c("green","blue"))
  )

  ## difftime
  expect_error(
    vec_c(base, as.difftime(c("0:5:20")))
  )
  expect_error(
    vec_c(base, color_vctr(as.difftime(c("0:5:20")), text_color = "blue"))
  )

  ## POSIXct
  expect_equal(
    vec_c(base, as.POSIXct("1970-01-01")),
    color_vctr(c(as.POSIXct("1970-01-01"), as.POSIXct("1970-01-01")), text_color = c("green",NA))
  )
  expect_equal(
    vec_c(base, color_vctr(as.POSIXct("1970-01-01"), text_color = "blue")),
    color_vctr(c(as.POSIXct("1970-01-01"), as.POSIXct("1970-01-01")), text_color = c("green","blue"))
  )

  ## POSIXlt
  expect_equal(
    vec_c(base, as.POSIXlt("1970-01-01")),
    color_vctr(c(as.POSIXlt("1970-01-01"), as.POSIXlt("1970-01-01")), text_color = c("green",NA))
  )
  expect_equal(
    vec_c(base, color_vctr(as.POSIXlt("1970-01-01"), text_color = "blue")),
    color_vctr(c(as.POSIXlt("1970-01-01"), as.POSIXlt("1970-01-01")), text_color = c("green","blue"))
  )


  ## list
  expect_equal(
    vec_c(base, list(1)),
    list(base, 1)
  )
  expect_equal(
    vec_c(list(1), base),
    list(1, base)
  )
})
