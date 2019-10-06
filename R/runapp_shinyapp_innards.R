#' Returns what was called inside of a function stored as text
#' 
#' @param x the code stored as text
#' @param fn the function to sub out
#' 
#' @export
#'
#' @examples
#' x <- "mean(list(1:10, 10:30))"
#' guts(x, "mean")
#' guts(x, "mean") %>% guts("list")
#' 
guts <- function(x, fn) {
  return_args <- function(...) {
    (as.list(match.call(expand.dots = T)))
  }
  
  assign(fn, return_args)
  
  eval(parse(text = x))
}


#' Returns asignments only from expressions
#' 
#' @param x expression from code stored as text
#' 
deparse_server <- function(x) {
  x %>% 
    deparse() %>% 
    trimws() %>% 
    find_all_assignments_r()
}


#' Pulls the calls out of runApp and shinyApp
#' 
#' @param x expression containing runApp(...) 
#'
inside_runapp <- function(x){
  sub("runApp\\(shinyApp", "runApp(list", x) %>% 
    guts("runApp") %>% 
    guts("list")
}


#' @rdname inside_runapp
inside_shinyapp <- function(x){
  guts(x, "shinyApp")
}


#' Returns server code from shinyApp or runApp
#' 
#' @param text code stored as text
#' 
extract_from_app_fn <- function(text) {
  
  code <- gsub("shiny::", "", text)
  
  if (!grepl("server", code)) {
    warning("server not listed", call. = FALSE)
    inside_code <- list(server = "")
  } else if (grepl("^runApp.*server =", code)) {
    inside_code <- inside_runapp(code)
  } else if (grepl("shinyApp.*server =", code)) {
    inside_code <- inside_shinyapp(code)
  } 
  
  deparse_server(inside_code$server)
}
