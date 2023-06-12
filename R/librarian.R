#' Load Common Packages
#'
#' Load common packages for basic use "basic", statistics "stats", and FMT reports "reports".
#' @param x use case: basic, stats, reports
#' @export

librarian <- function(x="basic"){
  if (x=="basic"){
    library(tidyverse)
    library(janitor)
    print("Loaded Basic Libraries")
  } else if (x=="stats"){
    library(tidyverse)
    library(janitor)
    library(MASS)
    library(car)
    library(esquisse)
    library(moderndive)
    library(rstatix)
    print("Loaded Stats Libraries")
  } else if (x=="reports"){
    library(tidyverse)
    library(janitor)
    library(magrittr)
    library(kableExtra)
    library(formattable)
    library(knitr)
    library(xlsx)
    print("Loaded Quarterly Report Libraries")
  } else {print("Libraries not Selected")}
}
