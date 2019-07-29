# SETUP ----
# read rmd
library(tidyverse)
library(glue)

selected_file <- file.choose()

# READ CODE ----
# get code ready for evaluation
make_edits <-
  read_file(selected_file) %>%
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
for (i in 1:nchar(make_edits)) {
  x <- str_sub(make_edits, i, i)
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
final_code <-
  tibble(text = read_lines(str_replace_all(final_edit, ";+", ";")))
  

# INPUTS ----
# make sure demo inputs exist (if required)
input_demo <-
  final_code %>%
  filter(str_detect(text, "input <-")) %>%
  pull(text)


input_demo_values <-
  input_demo %>%
  str_extract_all("(\\w+)(?=\\s\\=)") %>%
  unlist()


validate_inputs <- function() {
  
  input_ref <-
    read_lines(file = selected_file) %>%
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

    if (length(input_demo) == 0) { # no input demo, create new list
      update_input_code <- glue("input <- list({input_add})")
      message(
        glue(
          "# Add this code chunk to your Rmd:
        \n```{{r input_demo, eval = FALSE}}\n\n",
          styler::style_text(update_input_code),
          "\n\n```"
        )
      )
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

validate_inputs()

# * load inputs ----
eval(parse(text = input_demo))


# REACTIVE FUNCTIONS ----
reactive_functions <-
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


# * load functions ----
eval(parse(text = reactive_functions))


# x <-
#   tibble(text = c("a <- function(){ df <- get_data}", "b <- function(){ df <- get_data}")) %>%
#   pull(text)
#
# eval(parse(text = x))

rm(
  final_code,
  #input,
  input_ref,
  final_edit,
  i,
  input_add,
  input_demo,
  input_demo_values,
  make_edits,
  n_open,
  reactive_functions,
  selected_file,
  update_input_code,
  x
)
