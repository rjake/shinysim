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


# REACTIVE FUNCTIONS ----
# * load functions ----
eval(parse(text = find_reactive_functions()))

rm(
  code_to_df,
  file_to_parse,
  final_code,
  find_input_code,
  find_reactive_functions,
  validate_inputs
)
