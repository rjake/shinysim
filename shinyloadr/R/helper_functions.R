# Find all assignments and libraries ----

#' Title
#'
#' @param file 
#' @param output 
#'
#' @return A data frame of all assignments and libraries
#' @export
#' @importFrom knitr purl
#'
find_all_assignments <- function(file, output) {
  x <- purl(file, output = output, quiet = TRUE)
  r_code <- as.character(parse(x)) %>% trimws()
  assignments <- r_code[grepl("^(\\w+ <-|library)", r_code)]
  
  return(assignments)
}


#' Title
#'
#' @param x 
#'
#' @return
#' @export
#' @importFrom stringr str_detect str_replace_all
#'
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

#' Title
#'
#' @param file 
#' @param output 
#'
#' @return
#' @export
#' @importFrom tibble tibble
#' @importFrom dplyr rowwise mutate ungroup
#'
code_to_df <- function(file, output) {
  tibble(raw = as.character(find_all_assignments(file, output))) %>% 
    rowwise() %>% 
    mutate(code = convert_assignments(raw)) %>% 
    ungroup()
}


#' Title
#'
#' @param file 
#' @param output 
#'
#' @return
#' @export
#' @importFrom readr read_file
#' @importFrom stringr str_replace_all
#' @importFrom knitr purl
#'
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


#' Title
#'
#' @param file 
#' @param output 
#'
#' @return Prints a statement about the inputs that are either listed or missing
#' @export
#' @importFrom stringr str_extract_all str_remove
#' @importFrom readr read_lines
#' @importFrom tibble tibble
#' @importFrom dplyr mutate row_number filter distinct group_by summarise n
#' @importFrom tidyr unnest
#' @importFrom glue glue glue_collapse
#' @importFrom styler style_text
#' 
#'
validate_inputs <- function(file, output) {
  input_code <- find_input_code(file, output)
  
  input_demo_values <-
    input_code %>%
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
      lines = glue_collapse(line, sep = ",")
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

    if (length(input_code) == 0) { # no input demo, create new list
      update_input_code <- glue("input <- list({input_add})")

      message("\n# Add this code chunk to your Rmd:\n")
      message("```{r input_demo, eval = FALSE}")
      print(styler::style_text(update_input_code))
      message("```")
      
    } else { # append list
      message("Update code:")
      update_input_code <- glue("input <- list(\n..., \n{input_add}\n)")
      # str_replace(trimws(input_demo), "\\)$", glue("\n, {input_add})"))
      print(styler::style_text(update_input_code))
      
    }
    
    readline(
      prompt = 
        "Without all of the input list items accounted for, some of your functions may not work.\nDo you want to continue? Press [Enter] to continue or [Esc] to cancel."
    )
  }
}
