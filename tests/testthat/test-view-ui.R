library(shiny)

test_that("error if not shiny.tag", {
  x <- ""
  
  expect_error(view_ui(x))
})


test_that("shiny.tag.list is accepted", {
  x <- 
    tagList(
      h4("A header"),
      selectInput("select", "Select here", choices = 1:10)
    )

  expect_message(
    view_ui(x, close_after = 0.1),
    regexp = "Listening"
  )
})
