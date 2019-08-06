library(testthat)
library(shinyloadr)

test_that("shiny::reactive() and reactive() both work", {
  no_namespace <- "test <- shiny::reactive(abcd"
  with_namespace <- "test <- reactive(abcd"
  
  expect_equal(
    shinyloadr:::convert_assignments(no_namespace),
    shinyloadr:::convert_assignments(with_namespace)
  )        
})