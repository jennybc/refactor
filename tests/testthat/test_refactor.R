context("Factor creation")

test_that("factor levels can be set logically", {
  x <- c('b', 'a', 'c', 'c')
  xfact <- refactor(x)
  expect_identical(c('c', 'a', 'b'), levels(xfact))

  xfact <- refactor(x, logic = "appear")
  expect_identical(c('b', 'a', 'c'), levels(xfact))
})

test_that("factor levels can be set explicitly", {
  x <- c('b', 'a', 'c', 'c')
  xfact <- refactor(x, levels = c('c', 'b', 'a'))
  expect_identical(c('c', 'b', 'a'), levels(xfact))
})

test_that("other arguments of factor() can be set if levels are specified", {
  x <- c('b', 'a', 'c', 'c')
  xfact <- refactor(x, levels = c('c', 'b', 'a'), ordered = TRUE)
  expect_true(is.ordered(xfact))
})
