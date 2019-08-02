library(tidyverse)
library(glue)
library(knitr)

load_reactive_objects <- function(file){
  # source helper functions
  source(paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/helper_functions.R"))
  
  #create temp folder
  temp_folder <- tempdir(check = TRUE)
  temp_R <- tempfile(tmpdir = temp_folder, fileext = ".R")
  
  # select file if not provided
  file_to_parse <- ifelse(missing(file), file.choose(), file)
  
  raw_code <- read_file(file)

  # code as tibble
  final_code <- 
    code_to_df(file_to_parse, temp_R)

  
  # INPUTS ----
  # make sure demo inputs exist (if required)
  validate_inputs(file_to_parse, temp_R)
  
  # * load inputs ----
  eval(parse(text = find_input_code(file_to_parse, temp_R)), envir = .GlobalEnv)
  
  #run_all_chunks()
  
  # REACTIVE FUNCTIONS ----
  # * load functions ----
  eval(parse(text = final_code$code), envir = .GlobalEnv) #[20]
  
  rm(
    code_to_df,
    convert_assignments,
    #file_to_parse,#final_code,
    find_all_assignments,
    find_input_code,
    #temp_folder,temp_R,
    validate_inputs,
    envir = .GlobalEnv
  )
  #unlink(temp_folder, recursive = TRUE)
}

load_reactive_objects("GitHub/shinybreakdown/test_dashboard.Rmd")
