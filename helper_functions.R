temp_folder <- tempdir()
temp_R <- tempfile(tmpdir = temp_folder, fileext = ".R")

# Get code ready for evaluation ----
code_to_df <- function() {
  tibble(raw = as.character(find_all_assignments())) %>% 
    rowwise() %>% 
    mutate(code = convert_assignments(raw)) %>% 
    ungroup()
}


# Find input demo ----
find_input_code <- function(){
  replace_evals <- 
    read_file(file_to_parse) %>% 
    str_replace_all("eval = F(ALSE)?", "eval = TRUE")
  
  # create R doc from 
  knitr::purl(text = replace_evals, output = temp_R, quiet = TRUE)
  
  parsed <- parse(temp_R)
  
  input_code <- suppressWarnings(parsed[str_detect(parsed, "input <-")])
  
  as.character(input_code)
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
find_all_chunks <- function(envir = globalenv()){
  ## Function to run ALL chunks in a Rmd file
  ## Based on
  ## http://stackoverflow.com/questions/24753969/knitr-run-all-chunks-in-an-rmarkdown-document
  knitr::purl(file_to_parse, output = temp_R, quiet = TRUE)
  # sys.source(temp_R, envir = envir)
  # unlink(temp_R)
}


# Find reactive functions ----
find_all_assignments <- function() {
  x <- knitr::purl(file_to_parse, output = temp_R, quiet = TRUE)
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

# x <-
#   tibble(text = c("a <- function(){ df <- get_data}", "b <- function(){ df <- get_data}")) %>%
#   pull(text)
#
# eval(parse(text = x))
