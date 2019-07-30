# Get code ready for evaluation ----
code_to_df <- function() {
  clean_code <-
    read_file(file_to_parse) %>%
    str_replace_all(" +", " ") %>%
    str_replace_all("\\n(\\s?)(#.*)\r", "") %>% # single line comments
    str_replace_all("(\\%\\>\\%)( #[^\r]*)\r", "\\1 ") %>% # inline comments after pipe
    str_replace_all("([,\\{\\(])( #[^\r]*)\r", "\\1 ") %>% # inline comments after brackets
    str_replace_all("(\\<\\-)[ \r\n]+", "\\1 ") %>% # assignments
    str_replace_all("(,)[ \r\n]+", "\\1 ") %>% # commas
    str_replace_all("(\\%\\>\\%)[ \r\n]+", "\\1 ") %>% # dplyr pipe
    str_replace_all("(\\+)[ \r\n]+", "\\1 ") %>% # ggplot pipe
    str_replace_all("([\\{\\(]+)[ \r\n]+", "\\1 ") %>% # open bracket/parens
    str_replace_all("[ \r\n]+([\\}\\)]+)", "\\1 ") # move close bracket/parens up
  
  # find closing parenteses for all open parenteses
  n_open <- 0
  final_edit <- ""
  
  # add ; to lines
  for (i in 1:nchar(clean_code)) {
    x <- str_sub(clean_code, i, i)
    if (x == "{") {
      n_open <- n_open + 1
    }
    if (x == "}") {
      n_open <- n_open - 1
    }
    if (n_open > 0 && str_detect(x, "[\r\n]")) {
      x <- ";"
    }
    final_edit <- paste0(final_edit, x)
  }
  
  # turn code into df
  tibble(text = read_lines(str_replace_all(final_edit, ";+", ";")))
  
}


# Find input demo ----
find_input_code <- function() {
  # final_code <- code_to_df()
  final_code %>%
  filter(str_detect(text, "input <-")) %>%
  pull(text)
}


# Write message based on inputs available ----
validate_inputs <- function() {
  
  input_demo_values <-
    find_input_code() %>%
    str_extract_all("(\\w+)(?=\\s\\=)") %>%
    unlist()
  
  input_ref <-
    read_lines(file = file_to_parse) %>%
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
  
  
  input_ref
  
  
  if (nrow(input_ref) == 0) { # no inputs
    print("No inputs")
  } else if (sum(input_ref$missing) > 0) { # missing references
    input_df <-
      input_ref %>%
      filter(missing == TRUE)
    
    input_add <-
      glue('{input_df$input_name} = ""') %>%
      glue_collapse(sep = ", \n")
    
    rm(input_df)
    
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
    # print input statement
  } else {
    print("All inputs accounted for :)")
  }
}

# Run all chunks
run_all_chunks <- function(envir = globalenv()){
  ## Function to run ALL chunks in a Rmd file
  ## Based on
  ## http://stackoverflow.com/questions/24753969/knitr-run-all-chunks-in-an-rmarkdown-document
  temp_R <- tempfile(tmpdir = ".", fileext = ".R")
  knitr::purl(file_to_parse, output = temp_R)
  sys.source(temp_R, envir = envir)
  unlink(temp_R)
}


# Find reactive functions ----
find_reactive_functions <- function() {
  final_code %>%
  filter(str_detect(text, "<- reactive")) %>%
  #slice(21) %>%
  mutate(
    text =
      trimws(text) %>%
      str_replace("reactive\\((\\{)?", "function\\(\\)\\{") %>%
      str_replace("\\}( )?\\)", "\\}") %>%
      str_replace("\\)$", "\\}") %>%
      str_replace_all('\"', "'")
  ) %>%
  pull(text)
}

# x <-
#   tibble(text = c("a <- function(){ df <- get_data}", "b <- function(){ df <- get_data}")) %>%
#   pull(text)
#
# eval(parse(text = x))
