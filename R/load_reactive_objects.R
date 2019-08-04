#' Add objects and inputs from a Rmd to your global environment
#' 
#' @description This function will run all assignments of your Rmd. In the process, this function will encourage the creation of a dummy \code{input} list that will mimic user input and allow your code to run. Lastly, reactive objects are converted to functions so they can still be called as \code{df()} etc.
#' @section Warning:
#' This function has the ability to overwrite your objects in your global environment. Make sure you understand how this function works before moving forward.
#' 
#' @param file Rmd to be evaluated and loaded into global environment
#' @param clear_environment When \code{TRUE}, will remove objects not named in \code{...}
#' @param restart When \code{TRUE}, will restart the current R session. If you have R default to restore RData by default, you will need to use the \code{clear_environment} argument as well 
#' @param ... other arguments passed on to  \code{\link{clear_environment}} as regular expressions: 
#' \itemize{
#'   \item \code{keep} 
#'   \item \code{remove}
#
#' }
#'
#' @export
#' @importFrom readr read_lines
#' @importFrom rstudioapi restartSession
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
load_reactive_objects <- function(file, 
                                  clear_environment = FALSE, 
                                  restart = FALSE, ...){
  
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
  
  if (restart) {
    rstudioapi::restartSession()
  }
  
  if (clear_environment) {
    clear_environment(...)
  }
  
  # * load inputs ----
  eval(parse(text = find_input_code(file_to_parse, temp_R)), envir = .GlobalEnv)
  
  # load libraries and functions ----
  suppressMessages(
    eval(parse(text = final_code$code), envir = .GlobalEnv)
  )
}
