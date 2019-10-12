library(mockery)

test_that("file name used", {
  expected <- "test.R"
  actual <- which_file(expected)
  expect_equal(actual, expected)
})


test_that("current source editor used", {
  expected <- "test.R"
  stub(
    which_file,
    "rstudioapi::getSourceEditorContext",
    list(path = expected)
  )
  stub(which_file, "is.null", FALSE)
  stub(which_file, "menu", 1)
  
  actual <- which_file()
  
  expect_equal(actual, expected)
})


test_that("file.choose used", {
  expected <- "test.R"
  stub(
    which_file,
    "rstudioapi::getSourceEditorContext",
    NULL
  )
  stub(which_file, "is.null", TRUE)
  stub(which_file, "menu", 2)
  stub(which_file, "file.choose", expected)
  
  actual <- which_file()
  
  expect_equal(actual, expected)
})
