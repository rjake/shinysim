
<!-- README.md is generated from README.Rmd. Please edit that file -->
shinyloadr
==========

<!-- badges: start -->
<!-- badges: end -->
Troubleshooting reactive data in a flexdashboard can be difficult. The goal of this package is to convert reactive data frames into functions that will read from your raw data and become available in your global environment. The main function will also recommended that you put a dummy `input` list in your Rmd to simulate the reactive inputs. You can learn more about a dummy input list in [this vignette](../docs/articles/tips-and-tricks.html).

Installation
------------

<!--  
  You can install the released version of `shinyloadr` from [CRAN](https://CRAN.R-project.org) with:
  
  ``` r
  install.packages("shinyloadr")
  ```

-->
You can install the development version from [GitHub](https://github.com/rjake/shinyloadr) with:

``` r
# install.packages("devtools")
devtools::install_github("rjake/shinyloadr")
```

Example
-------

There is a dummy flexdashboard available when the package is installed for you to see how this works.

``` r
library(shinyloadr)

system.file(package = "shinyloadr", "Rmd/test_dashboard_no_inputs.Rmd") %>% 
 load_reactive_objects()
```

This will result in the following output

    Here are the inputs you have listed:

      input_name  times_used  lines  missing
    1      displ           1     45     TRUE
    2       year           2  48,49     TRUE

    Add this code chunk to your Rmd:
    ```{r input_demo, eval = FALSE}
    input <- list(
      displ = "",
      year = ""
    )
    ```

    Without all of the input list items accounted for, some of your functions may not work.
    Do you want to continue? Press [Enter] to continue or [Esc] to cancel.

Hitting `[Enter]` will then load these objects into your global environment.

There are additional arguments you can use to resart R or to clear the environment. The `keep` and `remove` arguments take regular expressions as a pattern match. To get an exact match, use the `^` and `$` to signify the beginning and end (ex. `^your_text$`). To list objects, separate them with a `|`. as shown below.

``` r
load_reactive_objects(
  ...,
  clear_environment = TRUE, 
  keep = "test_|^df$|raw_data", # objects to keep
  remove = "input"              # objects to remove 
)
```
