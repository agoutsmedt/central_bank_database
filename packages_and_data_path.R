# Loading packages----

if(! "pacman" %in% installed.packages()){
  install.packages("pacman", dependencies = TRUE)
}

pacman::p_load(tidyverse,
               rvest,
               RSelenium,
               polite,
               glue,
               here,
               arrow,
               pdftools)

# Data Path----

data_path <- here(path.expand("~"),
                  "data",
                  "central_bank_database")
bis_data_path <- here(data_path, "BIS")

# Loading functions---

source("helper_functions.R")