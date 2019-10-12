#' Find text between brackets or parentheses
#'
#' @param text the text to scan
#' @param pattern look between "c" curly braces or "p" parenthese 
#'
#' @export
#' @importFrom dplyr case_when
#' @importFrom stringr str_locate_all
#'
#' @examples
#' "here is text (between two parentheses), as an example" %>% 
#'   char_between("p")
#'   
#' "here is text {between two curly braces}, as an example" %>% 
#'   char_between("c")
#'   
char_between <- function(text, pattern = c("c", "p")) {
  if (missing(pattern)) {
    warning(
      'pattern not specified, curly braces {} used for parsing', 
      call. = FALSE
    ) 
  }
  
  pattern <- match.arg(pattern)
  regex_pattern <-
    ifelse(
      pattern == "c", 
      "[\\{\\}]",
      "[\\(\\)]" 
    )
  
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
#' breakout_server_code(file = "inst/shiny/server.R") %>% 
#'   eval(envir = .GlobalEnv)
#' }
breakout_server_code <- function(file) {
  
  raw_code <- as.character(parse(file))
  
  server_line <- grep("^(shiny)?server\\b", raw_code, ignore.case = TRUE)
  
  server_missing <- length(server_line) == 0
  
  if (length(server_line) > 1) {
    stop("more than one server assignment found", call. = FALSE)
  }
  
  to_eval <- vector("character", 0)
  
  if (server_missing) { # treat all code as "server" code
    warning("server not found, using whole file", call. = FALSE)
    new_code <- unlist(lapply(raw_code, convert_assignments))
    to_eval <- new_code
  } else {
    server_code <- raw_code[server_line]
    
    new_code <-
      unlist(
        lapply(char_between(server_code, pattern = "c"), convert_assignments)
      )
    
    if (server_line == 1) { # use just the server code
      to_eval <- new_code
    } else {# replace the server <- with the assignments contained within
      to_eval <- c(raw_code[1:(server_line - 1)], new_code)
    }
    
    # if there is extra code after "server <-" (server assignment isn't the last assignment)
    if(server_line != length(raw_code)) {
      to_eval <- c(to_eval, raw_code[(server_line + 1):length(raw_code)])
    }
    
  }
  
  
  to_eval
}
