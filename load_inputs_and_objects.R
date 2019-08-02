# SETUP ----
# read rmd
library(tidyverse)
library(glue)

source(paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/helper_functions.R"))

file_to_parse <- file.choose()

final_code <- code_to_df()

# INPUTS ----
# make sure demo inputs exist (if required)
validate_inputs()

# * load inputs ----
eval(parse(text = find_input_code()))

#run_all_chunks()

# REACTIVE FUNCTIONS ----
# * load functions ----
eval(parse(text = final_code$code)) #[20]

rm(
  code_to_df,
  convert_assignments,
  #file_to_parse,
  final_code,
  find_all_assignments,
  find_all_chunks,
  find_input_code,
  temp_folder,
  temp_R,
  validate_inputs
)
unlink(temp_folder, recursive = TRUE)
