library(testthat)
library(shinysim)

test_that("shiny::reactive() and reactive() both work", {
  no_namespace <- "test <- shiny::reactive(abcd"
  with_namespace <- "test <- reactive(abcd"
  
  expect_equal(
    convert_assignments(no_namespace),
    convert_assignments(with_namespace)
  )        
})


test_that("strings_to_find() most recent list", {
  expect_equal(
    strings_to_find(),
    "^(library|[\\w\\.\\$0:9]+ (<-|=[^=]))"
  )
  
})


test_that("assignments can be = or <-", {
  x <- c("a", "a = 1", "b == 2", "c <- 3")
  expect_equal(
    find_all_assignments_r(x),
    x[c(2,4)]
  )
})