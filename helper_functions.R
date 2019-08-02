#temp_folder <- tempdir()
#temp_R <- tempfile(tmpdir = temp_folder, fileext = ".R")

# Find all assignments and libraries ----
find_all_assignments <- function(file, output) {
  x <- knitr::purl(file, output = output, quiet = TRUE)
  r_code <- as.character(parse(x)) %>% trimws()
  assignments <- r_code[str_detect(r_code, "^(\\w+ <-|library)")]
  
  return(assignments)
}

convert_assignments <- function(x){
  if (str_detect(x, "\\w+ <- .*reactive\\(")) {
    x <- x %>%
      str_replace_all("reactive\\((\\{)?", "function\\(\\)\\{") %>%
      str_replace_all("\\}( )?\\)", "\\}") %>%
      str_replace_all("\\)$", "\\}") %>%
      str_replace_all('\"', "'")
  }
  
  return(x)
}

# Get code ready for evaluation ----
code_to_df <- function(file, output) {
  tibble(raw = as.character(find_all_assignments(file, output))) %>% 
    rowwise() %>% 
    mutate(code = convert_assignments(raw)) %>% 
    ungroup()
}


# Find input demo ----
find_input_code <- function(file, output){
  replace_evals <- 
    read_file(file) %>% 
    str_replace_all("eval = F(ALSE)?", "eval = TRUE")
  
  # create R doc from Rmd
  knitr::purl(text = replace_evals, output = output, quiet = TRUE)
  
  parsed <- parse(output)
  
  input_code <- parsed[grepl("input <-", parsed)]
  
  as.character(input_code)
}


# Write message based on inputs available ----
validate_inputs <- function(file, output) {
  input_demo_values <-
    find_input_code(file, output) %>%
    str_extract_all("(\\w+)(?=\\s\\=)") %>%
    unlist()

  input_ref <-
    read_lines(file = file) %>%
    tibble(text = trimws(.)) %>%
    mutate(
      line = row_number(),
      text = str_remove(text, "#.*") # remove comments
    ) %>%
    filter(str_detect(text, "input\\$\\w+")) %>%
    mutate(input_name = str_extract_all(text, "input\\$\\w+")) %>%
    unnest(input_name) %>%
    distinct(input_name, line) %>%
    group_by(input_name = str_remove(input_name, "input\\$")) %>%
    summarise(
      times_used = n(),
      lines = glue_collapse(line, ",")
    ) %>%
    ungroup() %>%
    mutate(missing = (!input_name %in% input_demo_values | length(input_demo_values) == 0))

  if (nrow(input_ref) == 0) { # no inputs
    print("No inputs")
    
  } else if (sum(input_ref$missing) == 0) { # no missing references
    message("\n\nAll inputs accounted for :)")
    
  } else { # missing references
    message("Here are the inputs you have listed:\n")
    print(as.data.frame(input_ref))

    input_df <-
      input_ref %>%
      filter(missing == TRUE)

    input_add <-
      glue('{input_df$input_name} = ""') %>%
      glue_collapse(sep = ", \n")

    if (length(find_input_code()) == 0) { # no input demo, create new list
      update_input_code <- glue("input <- list({input_add})")

      message("\n# Add this code chunk to your Rmd:\n")
      message("```{r input_demo, eval = FALSE}")
      print(styler::style_text(update_input_code))
      message("```")
      
    } else { # append list
      message("Update code:")
      update_input_code <- glue("input <- list(\n..., \n{input_add}\n)")
      # str_replace(trimws(input_demo), "\\)$", glue("\n, {input_add})"))
      styler::style_text(update_input_code)
      
    }
    
    readline(prompt = "Do you want to continue? Press enter to continue or Esc to cancel")
  }
}
