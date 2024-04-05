# Loading packages and paths----

source("packages_and_data_path.R")

# Scraping the ECB website----

#' The aim of this script is to scrap different contents on the ECB website:
#' 
#' - the speeches
#' - the economic bulletins
#' - the research papers:
#'   - the working papers
#'   - the discussion papers
#'   - the occasional papers

## Paths and data----           
ecb_pub_path <- "https://www.ecb.europa.eu/pub/"

#' We load the old data
#' 
file <- list.files(ecb_data_path) %>% 
  as_tibble() %>% 
  filter(str_detect(value, "ecb_metadata_\\d+\\.rds")) %>% 
  mutate(date = str_extract(value, "\\d{8}") %>% ymd()) %>% 
  arrange(desc(date)) %>% 
  slice(1) %>% 
  pull(value)

old_ecb_doc <- readRDS(here(ecb_data_path,
                            file))

#' We create a file with the different paths. And we need also to set a number of iteration for scrolling
#' the page (that will be useful later, to adapt to the number of documents)
ecb_paths <- tribble(
  ~ document, ~ url, ~scroll_iteration,
  "Occasional papers", str_c(ecb_pub_path, "research/", "occasional-papers", "/html/index.en.html"), 80, # first type of working papers
  "Working papers", str_c(ecb_pub_path, "research/", "working-papers", "/html/index.en.html"), 600, # second type of working papers
  "Discussion papers", str_c(ecb_pub_path, "research/", "discussion-papers", "/html/index.en.html"), 10, # third type of working papers
  "Economic bulletin", str_c(ecb_pub_path, "economic-bulletin/html/all_releases.en.html"), 10,
  "Speech", "https://www.ecb.europa.eu/press/key/html/downloads.en.html", NA
)

## Launching the web browser----

session <- bow("https://www.ecb.europa.eu/")
remDr <- rsDriver(browser = "firefox",
                  port = 4444L,
                  chromever = NULL)
browser <- remDr[["client"]]

## Dowloading speeches table----

browser$navigate(filter(ecb_paths, document == "Speech")$url)
browser$findElement("css", "a.cross.linkButton.linkButtonLarge.floatRight.highlight-extra-light")$clickElement() # Refuse cookies

# We click on the link towards the .csv table that will be stored in our download directory
browser$findElement("css", ".csv")$clickElement()

# We move the downloaded csv files towards the ECB data
download_files <- list.files("C:/Users/goutsmedt/Downloads")[str_which(list.files("C:/Users/goutsmedt/Downloads"), ".csv")]
speeches_in_csv <- tibble(files = download_files) %>% 
  bind_cols(file.info(paste0("C:/Users/goutsmedt/Downloads/", download_files))) %>% 
  arrange(desc(mtime)) %>% 
  slice(1) %>% 
  pull(files)
file.copy(paste0("C:/Users/goutsmedt/Downloads/", speeches_in_csv),
          here(ecb_data_path, "speeches_ecb.csv"),
          overwrite = TRUE)

ecb_speeches <- read_delim(here(ecb_data_path, "speeches_ecb.csv"), delim = "|") %>% 
  arrange(desc(date), speakers) %>% 
  mutate(document = "speech",
         id = paste0("s", str_remove_all(date, "-"), "_", 1:n()),
         .by = date) %>% 
  rename(description = subtitle)

speech_metadata <- ecb_speeches %>% 
  select(-contents)

speech_text <- ecb_speeches %>% 
  select(id, contents)

## Scraping metadata of working papers----

extract_official_id <- "(ocp|op|eb|wp|w|dp)(\\.)?\\d+(?=(~|(\\.|_|)en|\\.pdf))" # this will be used to extract the official id from URL
all_data <- list()
for(doc in ecb_paths$document[1:3]){
  browser$navigate(filter(ecb_paths, document == doc) %>% pull(url))
  Sys.sleep(session$delay) 
  browser$findElement("css", "a.cross.linkButton.linkButtonLarge.floatRight.highlight-extra-light")$clickElement() # Refusing cookies
  Sys.sleep(1) 
  
  scroll_iteration <- ecb_paths %>% 
    filter(document == doc) %>% 
    pull(scroll_iteration)
  scroll_height <- 1600
  for(i in 1:scroll_iteration) {   
    print(i)
    browser$executeScript(glue("window.scrollBy(0,{scroll_height});"))
    Sys.sleep(0.4)
  }

  # We need a complex css selector to open the Details menu, but not "Annexes" or others.
  buttons <- browser$findElements("css selector", ".ecb-langSelector+ .accordion .header:nth-child(1) .title")
  for(i in seq_along(buttons)){
    buttons[[i]]$clickElement()
    Sys.sleep(0.4)
  }

  # Extracting the date with the number, to control for missing data
  dates <- browser$findElements("css selector", ".loaded > dt") %>% 
    map_chr(., ~.$getElementText()[[1]]) %>% 
    tibble(date = .) %>% 
    mutate(id = paste0(date, "_", 1:n()), .by = date)
  
  # Extracting the number of the papers. Very few (just one?) may be missing (see below)
  numbers <- browser$findElements("css selector", ".category , .loaded > dt") %>% 
    map_chr(., ~.$getElementText()[[1]]) %>% 
    tibble(category = .) %>% 
    mutate(number = cumsum(str_detect(category, "^\\d+ ")),
           date = if_else(str_detect(category, "^\\d+ "), category, NA)) %>% 
    mutate(date = paste0(date, "_", 1:n()), .by = date) %>% 
    mutate(id = first(date), .by = number) %>% 
    filter(! str_detect(category, "^\\d+ ")) %>% 
    select(-number, -date)
  
  # Extracting titles
  #' ".category+ .title" allowed to extract only the titles and not things like the 
  #' title of the web page, or the "Details" or "Annex" menus. .category represents the
  #' number of the paper. Problem is that in very rare occasions (only once), this information
  #' is missing, creating some problems. So we prefer taking all the titles and cleaning 
  #' ourselves.
  titles <- browser$findElements("css selector", ".loaded > dt , .title") %>% 
    map_chr(., ~.$getElementText()[[1]]) %>% 
    tibble(title = .) %>% 
    mutate(number = cumsum(str_detect(title, "^\\d+ ")),
           date = if_else(str_detect(title, "^\\d+ "), title, NA)) %>% 
    mutate(date = paste0(date, "_", 1:n()), .by = date) %>% 
    mutate(id = first(date), .by = number) %>% 
    filter(! str_detect(title, "^\\d+ ")) %>% 
    distinct(id, .keep_all = TRUE) %>% 
    select(-number, -date) %>% 
    slice(-1) # remove the first title that is the title of the web page
  
  # Extracting the author
  authors <- browser$findElements("css selector", ".loaded > dt , #lazyload-container ul") %>% 
    map_chr(., ~.$getElementText()[[1]]) %>% 
    tibble(author = .) %>%
    mutate(number = cumsum(str_detect(author, "^\\d+ ")),
           date = if_else(str_detect(author, "^\\d+ "), author, NA)) %>%
    mutate(date = paste0(date, "_", 1:n()), .by = date) %>% 
    mutate(id = first(date), .by = number) %>% 
    filter(str_detect(date, "^NA")) %>% 
    select(-number, - date) %>% 
    distinct(id, .keep_all = TRUE) # This is linked to the fact that we have several documents for one date in 1999 and it seems to be doublons
  
  # Extracting the supplementary information
  supplementary_information <- browser$findElements("css selector", ".loaded > dt , .content-box dt , .content-box dd") %>% 
    map_chr(., ~.$getElementText()[[1]]) %>% 
    tibble(text = .) %>% 
    mutate(number = cumsum(str_detect(text, "^\\d+ ")),
           date = if_else(str_detect(text, "^\\d+ "), text, NA)) %>% 
    mutate(date = paste0(date, "_", 1:n()), .by = date) %>% 
    mutate(id = first(date), .by = number) %>% 
    mutate(info_type = lag(str_extract(text, "^Abstract$|^JEL Code$|^Network$"), 1)) %>%  # We identify Abstracts and other information
    filter(! is.na(info_type)) %>% 
    select(-number, -date) %>% 
    pivot_wider(names_from = info_type, values_from = text) 
 
  # Merging all the metadata 
  metadata <- dates %>% 
    left_join(numbers) %>% 
    left_join(titles) %>% 
    left_join(authors) %>% 
    left_join(supplementary_information) %>% 
    mutate(category = if_else(str_detect(title, "^Estimating the Fed’s Unconventional Policy Shocks$"), "No. 2585", category)) # correcting missing WP

  # Extracting the URLs
  url <-  browser$findElements("css selector", ".category+ .title a") %>% 
    map_chr(., ~.$getElementAttribute("href")[[1]]) %>% 
    tibble(url = .) %>% 
    mutate(official_id = str_extract(url, extract_official_id)) %>% 
    unique()

  url_pdf <-  browser$findElements("css selector", ".pdf") %>% 
    map_chr(., ~.$getElementAttribute("href")[[1]]) %>% 
     .[!str_detect(., "ECB_(C|c)irculation_(M|m)odel")] %>%  
    tibble(url_pdf = .) %>% 
    mutate(official_id = str_extract(url_pdf, extract_official_id)) %>%
    filter(! is.na(official_id)) %>% 
    unique()
  
  urls <- url %>% 
    full_join(url_pdf) %>% 
    mutate(number = str_extract(official_id, "\\d+$") %>% as.integer())
  
  # Merging URLs with metadata
  metadata <- metadata %>% 
    mutate(number = str_extract(category, "\\d+$") %>% as.integer()) %>% 
    left_join(urls)

  #' You cannot differentiate the authors in the information above as there is no delimiter. The easiest
  #' thinkable way to differentiate them is to extract the author individually (they are of course more authors than documents)
  #' and then to use the name to extract the authors. The author column will thus become a "list" column.  
  list_all_author <- browser$findElements("css selector", ".authors a") %>% 
    map_chr(., ~.$getElementText()[[1]]) %>% 
    unique() %>% 
    .[! str_count(.) == 0]
  
  metadata <- metadata %>% 
    mutate(author = str_extract_all(author, paste0(list_all_author, collapse = "|")),
           document = doc)
  
  all_data[[paste0(doc)]] <- metadata
}

## Scraping metadata of Economic Bulletin----

browser$navigate(filter(ecb_paths, document == "Economic bulletin") %>% pull(url))
Sys.sleep(session$delay) 
browser$findElement("css", "a.cross.linkButton.linkButtonLarge.floatRight.highlight-extra-light")$clickElement() # Refusing cookies
Sys.sleep(1) 

scroll_iteration <- ecb_paths %>% 
  filter(document == "Economic bulletin") %>% 
  pull(scroll_iteration)
scroll_height <- 1600
for(i in 1:scroll_iteration) {   
  print(i)
  browser$executeScript(glue("window.scrollBy(0,{scroll_height});"))
  Sys.sleep(0.4)
}

economic_bulletin_metadata <- tibble(
  date = browser$findElements("css selector", "dt") %>% 
    map_chr(., ~.$getElementText()[[1]]),
  title = browser$findElements("css selector", ".-filter .title") %>% 
    map_chr(., ~.$getElementText()[[1]]),
  url = browser$findElements("css selector", ".title a") %>% 
    map_chr(., ~.$getElementAttribute("href")[[1]]),
  url_pdf = browser$findElements("css selector", ".pdf") %>% 
    map_chr(., ~.$getElementAttribute("href")[[1]])
) %>% 
  mutate(official_id = str_extract(url_pdf, extract_official_id),
         document = "Economic bulletin")

##' Merging all the data together
##' 

ecb_doc <- bind_rows(all_data) %>% 
  bind_rows(economic_bulletin_metadata) %>% 
  mutate(date = dmy(date),
         id = case_match(document,
                         "Occasional papers" ~ paste0("op", number),
                         "Working papers" ~ paste0("wp", number),
                         "Discussion papers" ~ paste0("dp", number),
                         "Economic bulletin" ~ official_id)) %>%
  rename(description = Abstract) %>% 
  select(-category, -number, -official_id) %>% 
  bind_rows(ecb_speeches)

# This is the last step to merge the new data downloaded with the data we already had
if(old_ecb_doc %in% ls()){
  new_ecb_doc <- ecb_doc %>% 
    bind_rows(old_ecb_doc) %>% 
    unique()
} else {
  new_ecb_doc <- ecb_doc
}

saveRDS(new_ecb_doc, here(ecb_data_path, 
                      glue("ecb_metadata_{str_remove_all(Sys.Date(), \"-\")}.rds")))

# Downloading documents----
if(! "pdf" %in% list.files(ecb_data_path)) dir.create(here(ecb_data_path, 
                                                           "pdf"))
# We first need to check which pdf we already have in our directory
list_pdf <- list.files(here(ecb_data_path, 
                            "pdf")) %>% 
  str_remove("\\.pdf")

domain <- "https://www.ecb.europa.eu/"
pdf_to_download <- new_ecb_doc %>%  # This is the list of the remaining pdf to get
  filter(document != "speech", 
         ! id %in% list_pdf) %>% 
  select(id, url_pdf) 

path <- here(ecb_data_path, "pdf") 

rstudioapi::jobRunScript(here("helper_scripts",  "background_polite_download.R"),
             importEnv = TRUE,
             workingDir = here())

# Extracting texts----

# We import the data frame if it exists

text_file <- list.files(ecb_data_path) %>% 
  as_tibble() %>% 
  filter(str_detect(value, "ecb_text_\\d+\\.parquet")) %>% 
  mutate(date = str_extract(value, "\\d{8}") %>% ymd()) %>% 
  arrange(desc(date)) %>% 
  slice(1) %>% 
  pull(value)

ecb_text <- read_parquet(here(ecb_data_path,
                              text_file))

if(ecb_text %in% ls()){
  text_to_extract <- new_ecb_doc %>% 
    filter(! id %in% ecb_text$id,
           document != "speech") %>% 
    mutate(id = str_c(id, ".pdf")) %>% 
    pull(id)
} else { # If the data.frame does not exist, we can create a list of the pdfs
  text_to_extract <- new_ecb_doc %>% 
    mutate(id = str_c(id, ".pdf")) %>% 
    filter(document != "speech")%>% 
    pull(id) 
}

# We use a function to extract the text from the pdfs stocked in a certain repository
new_text <- map(text_to_extract, ~extracting_text(., here(ecb_data_path, "pdf"))) %>% 
  bind_rows()

# We merge with the data that we have already collected, if it exists
if(is.data.frame(ecb_text)){
  ecb_text <- ecb_text %>% 
    bind_rows(new_text)
} else {
  ecb_text <- new_text
} 

# Character recognition on problematic pdfs----

# We identify the pdf for which we are unable to recognize text. That's the pdf with zero
# characters all along the file

pdf_without_ocr <- ecb_text %>% 
  mutate(test_ocr = str_count(text, "[a-z]+")) %>% 
  mutate(non_ocr_document = all(test_ocr < 160), .by = file) %>% 
  filter(non_ocr_document == TRUE) %>% 
  distinct(file) %>% 
  pull()

# We can then use tesseract to run OCR on these problematic pdfs, and integrate the new text
# to the data

new_text_ocr <- vector(mode = "list", length = length(pdf_without_ocr)) # The file to fill in the background job
background_data_path <- ecb_data_path
rstudioapi::jobRunScript(here("helper_scripts",  "background_run_ocr.R"),
                         importEnv = TRUE,
                         workingDir = here(),
                         exportEnv = "R_GlobalEnv")

ecb_text_cleaned <- ecb_text %>% 
  filter(! file %in% pdf_without_ocr) %>% 
  bind_rows(bind_rows(new_text_ocr)) %>% 
  arrange(file, page)

write_parquet(ecb_text_cleaned, 
              here(ecb_data_path,
                   glue("ecb_text_{str_remove_all(Sys.Date(), \"-\") }.parquet")),
              compression = "brotli")

# New Method

# url <- "https://www.ecb.europa.eu/pub/research/working-papers/html/papers-2023.include.en.html"
# 
# test <- read_html(url) %>% 
#   html_elements(xpath = "/html/body/dd") %>% 
#   html_text2() %>% 
#   tibble(text = ., date = read_html(url) %>% 
#            html_elements(".date") %>% 
#            html_text2()) %>% 
#   separate_longer_delim(text, "\n")
