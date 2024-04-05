# Loading packages----

if(! "pacman" %in% installed.packages()){
  install.packages("pacman", dependencies = TRUE)
}

pacman::p_load(tidyverse,
               data.table,
               janitor,
               zip,
               rvest,
               RSelenium,
               polite,
               glue,
               here,
               arrow,
               pdftools,
               tesseract,
               tidyfast,
               tidytext)

# Data Path----

data_path <- here(path.expand("~"),
                  "data",
                  "central_bank_database")
bis_data_path <- here(data_path, "BIS")
ecb_data_path <- here(data_path, "ECB")
boe_data_path <- here(data_path, "BoE")

# Loading functions---

source("helper_scripts/helper_functions.R")
user_agent <- "polite R package, for https://github.com/agoutsmedt/central_bank_database (aurelien.goutsmedt@uclouvain.be)"
