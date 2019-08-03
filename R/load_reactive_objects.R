#' Add objects and inputs from a Rmd to your global environment
#' @description The function will encourage the creation of an input list that provides dummy values that will allow your code to run. Reactive objects are converted to functions so they can still be called as df() etc.
#' 
#' @param file 
#'
#' @return Will load all assignments from an Rmd into the global environment
#' @export
#' @importFrom readr read_lines
#'
#' @examples
#' \dontrun{
#' system.file(package = "shinyloadr", "Rmd/test_dashboard.Rmd") %>% 
#'  load_reactive_objects()
#' 
#' system.file(package = "shinyloadr", "Rmd/test_dashboard_no_inputs.Rmd") %>% 
#'  load_reactive_objects()
#' 
#' system.file(package = "shinyloadr", "Rmd/test_dashboard_missing_inputs.Rmd") %>% 
#'  load_reactive_objects()
#' }
#' 
load_reactive_objects <- function(file){
  #create temp folder
  temp_folder <- tempdir(check = TRUE)
  temp_R <- tempfile(tmpdir = temp_folder, fileext = ".R")
  
  # select file if not provided
  file_to_parse <- ifelse(missing(file), file.choose(), file) 

  # code as tibble
  final_code <- 
    code_to_df(file_to_parse, temp_R)
  
  # make sure demo inputs exist (if required)
  validate_inputs(file_to_parse, temp_R)
  
  # * load inputs ----
  eval(parse(text = find_input_code(file_to_parse, temp_R)), envir = .GlobalEnv)
  
  
  # load libraries and functions ----
  suppressMessages(
    eval(parse(text = final_code$code), envir = .GlobalEnv)
  )
}