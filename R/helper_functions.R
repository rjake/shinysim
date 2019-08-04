#' Find all libraries and assignments
#'
#' @param file
#' @param output
#'
#' @description A data frame of all assignments and libraries
#' @importFrom knitr purl
#'
find_all_assignments <- function(file, output) {
  x <- purl(file, output = output, quiet = TRUE)
  r_code <- as.character(parse(x)) %>% trimws()
  assignments <- r_code[grepl("^(\\w+ <-|library)", r_code)]

  return(assignments)
}


#' Convert reactive dataframes to functions
#'
#' @param x
#' @description Code will break if additional arguments are present
#' @noRd
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


#' Convert R code to a data frame
#'
#' @param file
#' @param output
#'
#' @importFrom tibble tibble
#' @importFrom dplyr rowwise mutate ungroup
#'
code_to_df <- function(file, output) {
  tibble(raw = as.character(find_all_assignments(file, output))) %>%
    rowwise() %>%
    mutate(code = convert_assignments(raw)) %>%
    ungroup()
}


#' Look for input <- demo
#'
#' @param file
#' @param output
#'
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


#' Validate demo input statement
#'
#' @param file
#' @param output
#'
#' @description Prints a statement about the inputs that are either listed or missing
#' @importFrom stringr str_extract_all str_remove
#' @importFrom readr read_lines
#' @importFrom tibble tibble
#' @importFrom dplyr mutate row_number filter distinct group_by summarise n select arrange
#' @importFrom tidyr unnest
#' @importFrom pander pandoc.table
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
      lines = glue_collapse(line, sep = ", ")
    ) %>%
    ungroup() %>%
    mutate(
      missing = (!input_name %in% input_demo_values | length(input_demo_values) == 0),
      status = ifelse(missing, "missing", "have")
    )


  if (nrow(input_ref) == 0) { # no inputs
    print("No inputs")

  } else if (sum(input_ref$missing) == 0) { # no missing references
    message("\n\nAll inputs accounted for :)")

  } else { # missing references
    message("Here are the inputs you have listed:\n")
    input_ref %>%
      select(status, input = input_name, lines) %>%
      arrange(status) %>%
      pander::pandoc.table(justify = "left", split.cells = 50)

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

#' Clear all objects in environment
#'
#' @param keep A regular expression of objects in environment to keep
#' @param remove A regular expression of objects in environment to remove 
#' @return 
#' @export
#'
#' @examples
clear_environment <- function(keep = NULL, remove = NULL) {
  
  all_objects <- ls(envir = .GlobalEnv)
  base_regex <- "temp_|final_code"
  
  if (!missing(remove)) {
    base_regex <- 
      gsub(paste0("\\|?", remove, "\\|?"), "|", base_regex) %>% 
      gsub("(\\|\\|)+", "", .) %>% # multiple pipes: ||
      gsub("\\^(\\||$)", "", .) %>% # "^|" or ends with ^
      gsub("\\|\\$", "", .) # "|$"
  } 
  
  final_regex <- 
    ifelse(missing(keep), base_regex, paste(c(base_regex, keep), collapse = "|")) %>% 
    gsub("(\\|\\|)+", "", .)
  
  identify_objects <- !grepl(final_regex, all_objects)
  remove_objects <- all_objects[identify_objects]
  
  # list items to be kept
  if (length(remove_objects) != length(all_objects)) {
    message(
      paste(
        "these items will be kept:\n -", 
        paste(all_objects[!identify_objects], collapse = "\n - ")
      )
    )
  }
  
  # list items to be removed and then remove them
  if (length(remove_objects) > 0) {
    message(
      paste(
        "these items will be removed:\n -", 
        paste(remove_objects, collapse = "\n - ")
      )
    )  
    # confirm selections
    confirm <-
      readline(
        prompt =
          "Do you want to continue? Press [Enter] to continue or [Esc] to cancel."
      ) == ""
    
    # clear environment if enter is used
    if (confirm){
      rm(list = remove_objects, envir = .GlobalEnv)
    }
  }
  
}
