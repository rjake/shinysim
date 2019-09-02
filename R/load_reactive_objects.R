#' Add objects and inputs from a Rmd to your global environment
#' 
#' @description This function will run all assignments of your Rmd. In the process, this function will encourage the creation of a dummy \code{input} list that will mimic user input and allow your code to run. Lastly, reactive objects are converted to functions so they can still be called as \code{df()} etc.
#' @section Warning:
#' This function has the ability to overwrite your objects in your global environment. Make sure you understand how this function works before moving forward.
#' 
#' @param file Rmd to be evaluated and loaded into global environment
#' @param clear_environment When \code{TRUE}, will remove objects not named in \code{...}
#' @param restart When \code{TRUE}, will restart the current R session. If you have R default to restore RData by default, you will need to use the \code{clear_environment} argument as well 
#' @param keep a regular expression of objects to keep when \code{clear_environment = TRUE}
#'
#' @export
#' @importFrom readr read_lines
#' @importFrom rstudioapi restartSession
#'
#' @examples
#' \dontrun{
#' system.file(package = "shinysim", "Rmd/test_dashboard.Rmd") %>% 
#'  load_reactive_objects()
#' 
#' system.file(package = "shinysim", "Rmd/test_dashboard_no_inputs.Rmd") %>% 
#'  load_reactive_objects()
#' 
#' system.file(package = "shinysim", "Rmd/test_dashboard_missing_inputs.Rmd") %>% 
#'  load_reactive_objects()
#' }
#' 
load_reactive_objects <- function(file,
                                  clear_environment = FALSE,
                                  restart = FALSE,
                                  keep = NULL) {

  # create temp folder
  #temp_folder <- tempdir(check = TRUE)
  #temp_R <- tempfile(tmpdir = temp_folder, fileext = ".R")

  # select file if not provided
  file_to_parse <- which_file(file)

  # check if Rmd or R
  is_rmd <- str_detect(file_to_parse, "[rR]md$")

  # make sure demo inputs exist (if required)
  validate_inputs(file_to_parse)
  
  # confirm to continue
  confirm <-   menu(
    choices = c("Yes", "No"),
    title = "WARNING: This next step will load all object assignments into your global environment.\nDo you want to continue?"
  )
  

  if (confirm == 1) {
    if (restart) {
      rstudioapi::restartSession()
    }

    if (clear_environment) {
      # remove_object will return "cleared" if successful
      result <- remove_objects(keep)
      if (result != "cleared") {
        stop(result, call. = FALSE)
      }
    } else {
      result <- "proceed"
    }

    if (result %in% c("cleared", "proceed")) {
      # * load inputs ----
      eval(parse(text = find_input_code(file_to_parse)), envir = .GlobalEnv)

      # find all libraries and functions ----
      
        if (is_rmd) { 
          # code as tibble (orig + converted functions)
          final_code <- code_to_df(file_to_parse)
          # parsed code
          parsed_code <- parse(text = final_code$code)
        } else {
          # create ouput list so assignments don't break
          assign("output", list(), .GlobalEnv)
          # parsed code
          parsed_code <- parse_server_file(file_to_parse)
        }
      
      # final evaluation
      suppressMessages(
        eval(parsed_code, envir = .GlobalEnv)
      )
    }
  }
}
