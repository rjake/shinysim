#' Select file to use
#'
#' @param file path to file. 
#'
#' @description If the file is not specified, a menu will appear asking the user if they want to use the active source file loaded in RStudio or if they want to select the file (opens a new window).
#' @importFrom utils menu
#'
which_file <- function(file) {
  if (!missing(file)) {
    file_to_parse <- file
  } else {
    current_source <- rstudioapi::getSourceEditorContext()$path
    if (is.null(current_source)) {
      file_to_parse <- file.choose()
    } else {
      current_text <- basename(current_source)
      find_file <-
        menu(c(
          paste("Use current file:", current_text),
          "Choose file in browser"
        ))
      if (find_file == 1) {
        file_to_parse <- current_source
      } else {
        file_to_parse <- file.choose()
      }
    }
  }
}


#' Valid strings for assignments/column names
#'
#' @export
valid_assignments <- function() {
  "[\\w\\.\\$0:9]+"
}


#' Find all libraries and assignments
#'
#' @param file
#' @param output
#'
#' @description A data frame of all assignments and libraries
#' @importFrom knitr purl
#' @importFrom stringr str_detect
#'
find_all_assignments <- function(file) {
  strings <- paste0("^(library|", valid_assginments(), " <-)")
  
  x <- purl(file, output = tempfile(), quiet = TRUE)
  r_code <- as.character(parse(x)) %>% trimws()
  
  assignments <- r_code[str_detect(r_code, strings)]
  
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
      str_replace_all("(shiny::)?reactive\\(", "function() (")
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



#' Look for input usage
#'
#' @param file
#' @param output
#'
#' @description Prints a statement about the inputs that are either listed or missing
#' @importFrom stringr str_extract_all str_remove
#' @importFrom readr read_lines
#' @importFrom tibble tibble
#' @importFrom dplyr mutate row_number filter distinct group_by summarise n
#' @importFrom tidyr unnest
#' @importFrom glue glue glue_collapse
#'
#'
input_usage <- function(file) {
  tibble(text = trimws(read_lines(file = file))) %>%
    mutate(
      line = row_number(),
      text = str_remove(.data$text, "#.*") # remove comments
    ) %>%
    filter(str_detect(.data$text, "input\\$[\\w\\._]+")) %>%
    mutate(input_name = str_extract_all(.data$text, "input\\$[\\w\\._]+")) %>%
    unnest(.data$input_name) %>%
    distinct(.data$input_name, .data$line) %>%
    group_by(input_name = str_remove(.data$input_name, "input\\$")) %>%
    summarise(
      times_used = n(),
      lines = glue_collapse(.data$line, sep = ", ")
    ) %>%
    ungroup()
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
    input_usage(file) %>%
    mutate(
      missing = (!.data$input_name %in% input_demo_values | length(input_demo_values) == 0),
      status = ifelse(missing, "missing", "have")
    )


  if (nrow(input_ref) == 0) { # no inputs
    print("No inputs")

  } else if (sum(input_ref$missing) == 0) { # no missing references
    message("\nall inputs accounted for :)\n")

  } else { # missing references
    message("Here are the inputs you have listed:\n")
    input_ref %>%
      select(.data$status, input = .data$input_name, .data$lines) %>%
      arrange(.data$status) %>%
      pander::pandoc.table(justify = "left", split.cells = 25)

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
  }
  
  menu(
    choices = c("Yes", "No"),
    title = "WARNING: This next step will load all object assignments into your global environment.\nDo you want to continue?"
  )
}



#' Clear all objects in environment
#'
#' @param keep A regular expression of objects in environment to keep
#' 
#' @importFrom glue glue
#' @importFrom utils menu
#' @export
#'
#' @examples
#' \dontrun{
#' df <- iris
#' df2 <- iris
#' my_df <- iris
#' remove_objects(keep = "^df")
#' }
remove_objects <- function(keep = NULL) {
  
  all_objects <- ls(envir = .GlobalEnv)
  base_regex <- "temp_|final_code"
  
  final_regex <- 
    ifelse(missing(keep), base_regex, paste(c(base_regex, keep), collapse = "|")) %>% 
    gsub("(\\|\\|)+", "", .) %>% 
    gsub("\\|$", "", .) # ends with |, if keep = ""
  
  identify_objects <- !grepl(final_regex, all_objects)
  remove_objects <- all_objects[identify_objects]
  
  # list items to be removed and then remove them
  if (length(remove_objects) == 0) {
    final_result <- "No items to remove"
    message(final_result)
    
  } else {
    # list items to be removed
    message(
      paste(
        "these items will be removed or replaced when data is loaded:\n -", 
        paste(remove_objects, collapse = "\n - ")
      )
    )  
    
    # list items to be kept
    if (length(remove_objects) != length(all_objects)) {
      message(
        paste(
          "\nthese items will be kept:\n -", 
          paste(all_objects[!identify_objects], collapse = "\n - ")
        )
      )
      
      regex_phrase <- glue('editing this regex pattern: "{keep}"')
    } else {
      regex_phrase <- 'using the (keep = "") argument'
    }
    
    # confirm selections
    confirm <-
      menu(
        choices = c("Looks good to me", "I need to edit this list"),
        title = "Do you want to continue? (Press 0 to exit)"
      )
    
    # clear environment if enter is used
    if (confirm == 0) {
      final_result <- glue("Function ended because 0 was selected")
      final_result
      
    } else if (confirm == 1){
      rm(list = remove_objects, envir = .GlobalEnv)
      final_result <- "cleared"
      
    } else if (confirm == 2) {
      final_result <- glue("Please update the function by {regex_phrase}")
      
    }
    
    final_result
  }
}
