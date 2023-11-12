# Loading packages and paths----

source("packages_and_data_path.R")

# Scraping on BIS website----

## BIS website path----
bis_website_path <- "https://www.bis.org/"

# Using Polite package to introduce to the host and have information about scraping possibilities
session <- bow(bis_website_path)

## Scrapping metadata----

# We load the data we have already saved (if any); loading the most recent version
file <- list.files(bis_data_path) %>% 
  as_tibble() %>% 
  filter(str_detect(value, "wp_metadata_\\d+\\.rds")) %>% 
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
  scraping_path <- expr(glue("{bis_website_path}reshub/index.htm?reshub_page={page}&reshub_page_length=25"))
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
  
  scraping_path <- expr(glue("{bis_website_path}reshub/index.htm?fromDate={day}%2F{month}%2F{year(last_date)}&cbspeeches_page={page}&cbspeeches_page_length=25"))
}

# Launch Selenium to go on the website of bis
driver <- rsDriver(browser = "firefox", 
                   chromever = NULL,
                   port = 4444L) # can also be "chrome"
remote_driver <- driver[["client"]]
remote_driver$navigate(eval(scraping_path, envir = list(page = 1)))
Sys.sleep(2) # Just to have time to load the page

# We search the number of pages for the speeches we need to download
nb_pages <- remote_driver$findElement("css selector", ".pageof")$getElementText() %>% # We just set i for searching the total number of pages
  pluck(1) %>% 
  str_remove_all("Page 1 of |,") %>%
  as.integer()

# Loop which depends on the pages we want to scrap on "https://www.bis.org/doclist/cbspeeches.htm"
metadata <- vector(mode = "list", length = nb_pages)
for(page in 3:5){
  tryCatch(
    {
      remote_driver$navigate(eval(scraping_path))
      nod <- nod(session, eval(scraping_path)) # introducing to the new page
      Sys.sleep(session$delay) # using the delay time set by polite
      
      article_count <- 25 * page - 24
      
      url <- remote_driver$findElements("css selector", ".dark") %>% 
        map_chr(., ~.$getElementAttribute("href")[[1]])
      metadata[[page]] <- remote_driver$findElements("css selector", "td") %>% 
        map_chr(., ~.$getElementText()[[1]]) %>% 
        tibble(text = .) %>% 
        mutate(url = url,
               initial_id = article_count:(article_count+length(url)-1)) %>% 
        separate_longer_delim(text, "\n")
    },
    error = function(e) {
      print(glue("Error for page"))
      metadata[[page]] <- "Problem with this page"
    }
  )
}

new_data <- bind_rows(metadata) %>% 
  mutate(rank = 1:n(), .by = initial_id) %>% 
  mutate(type = if_else(rank == 1, "title", NA),
         type = if_else(str_detect(text, "^by "), "author", type),
         type = if_else(str_detect(text, "\\(added "), "wp_serie", type),
         type = if_else(str_detect(text, "^Papers:"), "number", type),
         type = if_else(str_detect(text, "^JEL:"), "jel_code", type))

# simple test to check consistency: one info per paper (but info may be missing of course)
test_info <- FALSE
if(test_info){
  test_number_info <- new_data %>% 
    count(type) %>% 
    mutate(total_wp = nrow(bind_rows(metadata)),
           filled_info = n/total_wp)
  cat(glue("We have {filter(test_number_info, type == 'title')$filled_info *100}% of titles,
         \n{filter(test_number_info, type == 'author')$filled_info *100}% of authors,
         \n{filter(test_number_info, type == 'number')$filled_info *100}% of numbers,
         \n{filter(test_number_info, type == 'wp_serie')$filled_info *100}% of wp_series and 
         \n{filter(test_number_info, type == 'jel_code')$filled_info *100}% of jel codes.\n"))
  cat(glue("We have {data_clean %>% filter(is.na(type)) %>% nrow()} NA values"))
}

new_data <- new_data %>% 
  pivot_wider(names_from = type, values_from = text) %>% 
  group_by(initial_id) %>% 
  fill(title, author, wp_serie, number, jel_code, .direction = "downup") %>% 
  ungroup() %>% 
  select(-rank, -initial_id) %>% 
  unique() %>% 
  mutate(document = "Working papers",
         date = str_extract(wp_serie, "(?<=\\(added ).+(?=\\))"),
         date = dmy(date)) %>% 
  mutate(id_date = paste0(str_remove_all(date, "-"), "_", 1:n()), .by = date) %>% 
  mutate(id_author = str_extract(author, "(?<=by )[A-z]+"),
         id = paste0(id_author, "_", id_date)) %>% 
  select(-id_date, -id_author)

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

# We can now save our extraction (and add the new data if data already existed)
if(is.data.frame(data) == FALSE){
  data <- new_data
} else {
  data <- data %>% 
    filter(date < last_date) %>% # To avoid weird doublons, as we have scraped the last date in the new_data
    bind_rows(new_data) %>% 
    arrange(desc(date)) %>% 
    unique()
}

# We can now save the result of the last extraction
saveRDS(data, here(bis_data_path,
                   glue("wp_metadata_{str_remove_all(Sys.Date(), \"-\")}.rds")))

# Downloading pdf----

 
