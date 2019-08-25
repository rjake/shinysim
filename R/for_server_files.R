#' Find text between brackets or parentheses
#'
#' @param text the text to scan
#' @param pattern look between "c" curly braces or "p" parenthese 
#'
#' @export
#'
#' @examples
#' "here is text (between two parentheses), as an example" %>% 
#'   char_between("p")
#'   
#' "here is text {between two curly braces}, as an example" %>% 
#'   char_between("c")
#'   
char_between <- function(text, pattern = c("c", "p")) {
  pattern <- match.arg(pattern)
  regex_pattern <-
    case_when(
      pattern == "c" ~ "[\\{\\}]",
      pattern == "p" ~ "[\\(\\)]",
      TRUE ~ NA_character_ 
    )
  
  if (is.na(regex_pattern)) {
    stop('"c" curly braces {} or "p" parentheses () not specified') 
  }
  
  pattern_match <-  
    str_locate_all(string = as.character(text), pattern = regex_pattern)[[1]] %>% 
    range()
  
  substr(text, pattern_match[1] + 1, pattern_match[2] - 1) %>% 
    trimws()
  
}



#' Parse server file for assignments & inputst
#'
#' @param file file to parse
#' 
#' @export
#' @examples 
#' \dontrun{
#' parse_server_file(file = "inst/shiny/server.R") %>% 
#'   eval(envir = .GlobalEnv)
#' }
parse_server_file <- function(file) {
  output <- list()
  
  raw_code <- parse(file)
  
  server_line <- grep("^(shiny)?server\\b", raw_code, ignore.case = TRUE)
  
  if (length(server_line) == 0) {stop("server not found")}
  if (length(server_line) > 1) {warning("more than one server assignment found")}
  
  server_code <- raw_code[server_line]
  
  new_code <-
    char_between(server_code) %>% 
    convert_assignments() %>% 
    parse(text = .)
  
  to_eval <- expression()
  
  if (server_line == 1) {
    to_eval <- c(to_eval, new_code)
  } else {
    to_eval <- c(raw_code[1:(server_line-1)], new_code)
  }
  
  if (server_line != length(raw_code)) {
    to_eval <- c(to_eval, raw_code[(server_line + 1):length(raw_code)])
  }
  
  to_eval
}

input_usage(file = "inst/shiny/server.R")
require(shiny)
parse_server_file(file = "inst/shiny/server.R") %>% eval()