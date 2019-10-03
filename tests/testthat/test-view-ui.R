library(shiny)
library(mockery)

test_that("error if not shiny.tag", {
  x <- ""
  
  expect_error(view_ui(x))
})


# test_that("shiny.tag.list is accepted", {
#   x <- 
#     tagList(
#       h4("A header"),
#       selectInput("select", "Select here", choices = 1:10)
#     )
# 
#   expect_message(
#     view_ui(x, close_after = 0.1),
#     regexp = "Listening"
#   )
# })

test_that("parameters were appropriately passed", {
  run_app_mock <- mock()
  
  stub(
    where = view_ui,
    what = "shinyApp",
    how = "test app"
  )
  
  stub(
    where = view_ui,
    what = "runApp",
    how = run_app_mock
  )
  
  x <- 
    tagList(
      h4("A header"),
      selectInput("select", "Select here", choices = 1:10)
    )
  
  view_ui(x)
  
  expect_called(run_app_mock, n = 1)
  
  expect_args(
    mock_object = run_app_mock,
    n = 1,
    appDir = "test app", 
    launch.browser = rstudioapi::viewer
  )
})
