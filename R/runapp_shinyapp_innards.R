guts <- function(x, fn) {
  return_args <- function(...) {
    (as.list(match.call(expand.dots = T)))
  }
  
  assign(fn, return_args)
  
  eval(parse(text = x))
}


deparse_server <- function(x) {
  x %>% 
    deparse() %>% 
    trimws() %>% 
    find_all_assignments_r()
}


inside_runapp <- function(x){
  sub("runApp\\(shinyApp", "runApp(list", x) %>% 
    guts("runApp") %>% 
    guts("list")
}


inside_shinyapp <- function(x){
  guts(x, "shinyApp")
}


extract_from_app_fn <- function(x) {
  
  code <- parse(text = gsub("shiny::", "", x))
  
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


app_assigned <- {
  paste0(
    "^",
    valid_assignments(), 
    "\\s?(<-|=)\\s?shinyApp.*"
  )
}
