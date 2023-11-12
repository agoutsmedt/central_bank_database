# Loading packages and paths----

source("packages_and_data_path.R")

# Scraping on BIS website----

## BIS website path----
bis_website_path <- "https://www.bis.org/"
cbspeeches_path <- paste0(bis_website_path, 
                          "cbspeeches/index.htm")

# Using Polite package to introduce to the host and have information about scraping possibilities
session <- bow(cbspeeches_path)

## Scrapping metadata----

# We load the data we have already saved (if any); loading the most recent version
file <- list.files(bis_data_path) %>% 
  as_tibble() %>% 
  filter(str_detect(value, "speeches_metadata_\\d+\\.rds")) %>% 
  mutate(date = str_extract(value, "\\d{8}") %>% ymd()) %>% 
  arrange(desc(date)) %>% 
  slice(1) %>% 
  pull(value)

data <- readRDS(here(bis_data_path,
                     file))

# If there is no data (meaning we have an error at readRDS()), we can start from the beginning. If we have some 
# data, we can start from the date of the most recent speech we have in our data.
if(is.data.frame(data) == FALSE){
  # This is the url to use if you start from scratch. It is registered as an expression that you will have to 
  # evaluate below by specifying the page number i in a loop (see below)
  scraping_path <- expr(glue("{cbspeeches_path}?cbspeeches_page={i}&cbspeeches_page_length=25"))
} else{
# If the data already exists, we search for the date of the last speeches we have downloaded
last_date <- data %>% 
  pull(date) %>% 
  max()

# We extract the month of the last date in a two digits format (like "05" rather than "5"), to put it later in the BIS url
month <- if(str_count(month(last_date)) == 1){
  paste0("0", month(last_date))
} else{
  month(last_date)
}

# Same for the day
day <- if(str_count(day(last_date)) == 1){
  paste0("0", day(last_date))
} else{
  day(last_date)
}

scraping_path <- expr(glue("https://www.bis.org/cbspeeches/index.htm?fromDate={day}%2F{month}%2F{year(last_date)}&cbspeeches_page={i}&cbspeeches_page_length=25"))
}

# Launch Selenium to go on the website of bis
driver <- rsDriver(browser = "firefox", 
                   chromever = NULL,
                   port = 4444L) # can also be "chrome"
remote_driver <- driver[["client"]]
remote_driver$navigate(eval(scraping_path, envir = list(i = 1)))
Sys.sleep(2) # Just to have time to load the page

# We search the number of pages for the speeches we need to download
#' I use tryCatch() to deal with a specific case: if we have only one page, the number of pages is 
#' not displayed, so it will throw an error. In case of error, nb_pages must equal 1.
tryCatch(
  {
    nb_pages <- remote_driver$findElement("css selector", ".pageof")$getElementText() %>% # We just set `page` for searching the total number of pages
      pluck(1) %>% 
      str_remove_all("Page 1 of |,") %>%
      as.integer()   },
  error = function(e) {
    print("There was an error, meaning that there is likely only one page to scrape")
    nb_pages <<- 1 # specific assignment of a value (see https://stackoverflow.com/questions/2628621/how-do-you-use-scoping-assignment-in-r)
  }
)

# Loop which depends on the pages we want to scrap on "https://www.bis.org/doclist/cbspeeches.htm"
new_data <- tibble(date = c(),
                   title = c(),
                   description = c(),
                   speaker = c(),
                   url = c())
for(i in 1:nb_pages){
  remote_driver$navigate(eval(scraping_path))
  nod <- nod(session, eval(scraping_path)) # introducing to the new page
  Sys.sleep(session$delay) # using the delay time set by polite
  
  dates <- remote_driver$findElements("css selector", ".item_date")
  titles <- remote_driver$findElements("css selector", ".dark")
  descriptions <- remote_driver$findElements("css selector", "p p")
  speakers <- remote_driver$findElements("css selector", "span+ p")
  
  new_data <- new_data %>% 
    bind_rows(tibble(date = map_chr(dates, ~.$getElementText()[[1]]),
                     title = map_chr(titles, ~.$getElementText()[[1]]),
                     description = map_chr(descriptions, ~.$getElementText()[[1]]),
                     speaker = map_chr(speakers, ~.$getElementText()[[1]]),
                     url = map_chr(titles, ~.$getElementAttribute("href")[[1]])))
}

# We just check that we have the correct number of speeches
nb_items <- remote_driver$findElement("css selector", ".listitems")$getElementText() %>% # We just set i for searching the total number of pages
  pluck(1) %>%
  str_remove_all("[\\sitems]") %>% 
  as.integer()
if(nb_items == nrow(new_data)){
  message("You have extracted the metadata of all speeches")
} else{
  "There is a problem. You have probably missed some speeches."
}

# We just add some small information and transform the date column in date format
new_data <- new_data %>% 
  mutate(date = dmy(date),
         document = "speech",
         pdf_link = str_replace(url, "htm$", "pdf"),
         file_name = str_extract(pdf_link, "(?<=\\/)r\\d+.+(?=\\.pdf)"))

# We can now save our extraction (and add the new data if data already existed)
if(is.data.frame(data) == FALSE){
  data <- new_data
} else {
  data <- data %>% 
    bind_rows(new_data) %>% 
    arrange(desc(date)) %>% 
    unique()
}

# We can now save the result of the last extraction
saveRDS(data, here(bis_data_path,
                   glue("speeches_metadata_{str_remove_all(Sys.Date(), \"-\")}.rds")))

# Cleaning data----
# 
# # See "helper_functions.R"
# # We separate the speaker from the title, and extract useful information about the speeches like 
# # the Central Bank to which the speaker belong
# data_cleaned <- cleaning_bis_speeches_metadata(data)
# 
# missing_cb <- data_cleaned %>% 
#   filter(is.na(central_bank))
# 
# saveRDS(data_cleaned, here(bis_data_path,
#                            glue("speeches_metadata_cleaned_{str_remove_all(Sys.Date(), \"-\")}.rds")))

# Downloading PDFs----

# We first check the pdf we already have in our data
list_pdf <- list.files(here(bis_data_path,
                            "pdf")) %>% 
  str_remove("\\.pdf")

pdf_to_download <- data %>% 
  filter(! file_name %in% list_pdf) %>% 
  pull(file_name)

# We download the pdf we don't have, but we let 1 second before each download
# Sometimes, pdf are missing and we don't want the "walk" to stop for this reason 
# so we use a function that catch errors.
walk(sample(pdf_to_download), ~polite_download("https://www.bis.org/",
                                            glue("review/{.}.pdf"),
                                            path = here(bis_data_path, "pdf")))

# Extracting texts----

# We import the data frame if it exists

text_file <- list.files(bis_data_path) %>% 
  as_tibble() %>% 
  filter(str_detect(value, "speeches_text_\\d+\\.parquet")) %>% 
  mutate(date = str_extract(value, "\\d{8}") %>% ymd()) %>% 
  arrange(desc(date)) %>% 
  slice(1) %>% 
  pull(value)

bis_text <- read_parquet(here(bis_data_path,
                         text_file))

if(is.data.frame(bis_text)){
text_to_extract <- data %>% 
  mutate(file = paste0(file_name, ".pdf")) %>% 
  filter(! file %in% bis_text$file) %>% 
  pull(file)
} else { # If the data.frame does not exist, we can create a list of the pdfs
  text_to_extract <- data %>% 
    mutate(file = paste0(file_name, ".pdf")) %>% 
    pull(file) 
}

# We use a function to extract the text from the pdfs stocked in a certain repository
new_text <- map(text_to_extract, ~extracting_text(., here(bis_data_path, "pdf"))) %>% 
  bind_rows()

# We merge with the data that we have already collected, if it exists
if(is.data.frame(bis_text)){
  bis_text <- bis_text %>% 
    bind_rows(new_text)
} else {
  bis_text <- new_text
}  

# Character recognition on problematic pdfs----

# We identify the pdf for which we are unable to recognize text. That's the pdf with zero
# characters all along the file

pdf_without_ocr <- bis_text %>% 
  mutate(test_ocr = str_count(text, "[a-z]+")) %>% 
  mutate(non_ocr_document = all(test_ocr < 160), .by = file) %>% 
  filter(non_ocr_document == TRUE) %>% 
  distinct(file) %>% 
  pull()

# We can then use tesseract to run OCR on these problematic pdfs, and integrate the new text
# to the data

new_text_ocr <- vector(mode = "list", length = length(pdf_without_ocr)) # The file to fill in the background job
background_data_path <- bis_data_path
rstudioapi::jobRunScript(here("helper_scripts",  "background_run_ocr.R"),
                         importEnv = TRUE,
                         workingDir = here(),
                         exportEnv = "R_GlobalEnv")

bis_text_cleaned <- bis_text %>% 
  filter(! file %in% pdf_without_ocr) %>% 
  bind_rows(bind_rows(new_text_ocr)) %>% 
  arrange(file, page)

write_parquet(bis_text_cleaned, 
              here(bis_data_path,
                       glue("speeches_text_{str_remove_all(Sys.Date(), \"-\") }.parquet")),
              compression = "brotli")

# Scraping missing pdf----

missing_text <- bis_text_cleaned %>% 
  filter(is.na(text)) %>% 
  mutate(file_link = paste0("https://www.bis.org/review/", file),
         file_link = str_replace(file_link, "\\.pdf", ".htm")) %>% 
  distinct(file, file_link)

# If your remote driver has been closed, you can reopen the window:
remote_driver$open()

# We just extract the text from the web page of the speech
missing_text_scraped <- vector(mode = "list", length = nrow(missing_text))
missing_text_scraped <- missing_text %>% 
  mutate(text = "",
         page = 1,
         .before = everything()) 

for(i in 1:nrow(missing_text)){
  remote_driver$navigate(missing_text$file_link[i])
  Sys.sleep(session$delay)
  missing_text_scraped$text[i] <- remote_driver$findElement("css selector", "div#cmsContent")$getElementText()[[1]]
}

# We merge the new text with the whole data set and we save again.
bis_text_completed <- bis_text_cleaned %>% 
  filter(! file %in% missing_text$file) %>% 
  bind_rows(select(missing_text_scraped, -file_link))

write_parquet(bis_text_completed, 
              here(bis_data_path,
                   glue("speeches_text_{str_remove_all(Sys.Date(), \"-\")}.parquet")),
              compression = "brotli")

