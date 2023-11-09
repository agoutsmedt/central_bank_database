# Loading packages and paths

source("packages_and_data_path.R")

##################### Cleaning data #######################################
file <- list.files(bis_data_path) %>% 
  as_tibble() %>% 
  filter(str_detect(value, "speeches_metadata_\\d+\\.rds")) %>% 
  mutate(date = str_extract(value, "\\d{8}") %>% ymd()) %>% 
  arrange(desc(date)) %>% 
  slice(1) %>% 
  pull(value)

data <- readRDS(here(bis_data_path,
                     file))
