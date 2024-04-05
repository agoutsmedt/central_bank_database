# Loading packages and paths----

source("packages_and_data_path.R")

# Scraping the ECB website----

#' The aim of this script is to scrap different contents on the ECB website:
#' 
#' - the speeches
#' - the economic bulletins
#' - various press documents:
#'    - interviews
#'    - monetary policy statements
#'    - monetary policy accounts
#' - the research papers:
#'   - the working papers
#'   - the discussion papers
#'   - the occasional papers

## Paths and data----         
ecb_domain <- "https://www.ecb.europa.eu/"
ecb_pub_path <- str_c(ecb_domain, "pub/")

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

#' We create a file with the different paths. 
ecb_paths <- tribble(
  ~ document, ~ url, ~ first_year,
  "Occasional papers", str_c(ecb_pub_path, "research/", "occasional-papers", "/html/papers-"), 2000, # first type of working papers
  "Working papers", str_c(ecb_pub_path, "research/", "working-papers", "/html/papers-"), 1999, # second type of working papers
  "Discussion papers", str_c(ecb_pub_path, "research/", "discussion-papers", "/html/papers-"), 2016, # third type of working papers
  "Economic bulletin articles", str_c(ecb_domain, "press/economic-bulletin/articles/"), 1999,
  "Economic bulletin focus", str_c(ecb_domain, "press/economic-bulletin/focus/"), 1999,
  "Economic bulletin", str_c(ecb_domain, "press/economic-bulletin/html/all_releases.en.html"), NA,
  "Monthly bulletin", str_c(ecb_domain, "press/economic-bulletin/mb/html/index.en.html"), NA,
  "Press conference", str_c(ecb_domain, "press/press_conference/monetary-policy-statement/"), 1998,
  "Monetary policy accounts", str_c(ecb_domain, "press/accounts/"), 2015,
  "Interviews", str_c(ecb_domain, "press/inter/date/"), 2004,
  "Speeches", str_c(ecb_domain, "press/key/date/"), 1997,
)
extract_official_id <- "(ocp|op|eb|wp|w|dp|is|sp|mg|in|mb)(\\.)?[0-9_]+(?=(~|(\\.|_|)en|\\.fr|\\.de|\\.es|\\.it|\\.nl|\\.pdf))" # this will be used to extract the official id from URL

## Launching the web browser----

session <- bow(ecb_domain)
remDr <- rsDriver(browser = "firefox",
                  port = 4444L,
                  chromever = NULL)
browser <- remDr[["client"]]

## Scraping metadata of Economic Bulletin----

browser$navigate(filter(ecb_paths, document == "Economic bulletin") %>% pull(url))
Sys.sleep(session$delay) 
browser$findElement("css", "a.cross.linkButton.linkButtonLarge.floatRight.highlight-extra-light")$clickElement() # Refusing cookies
Sys.sleep(1) 

economic_bulletin_metadata <- data.table(
  date = browser$findElements("css selector", "dt") %>% 
    map_chr(., ~.$getElementText()[[1]]),
  title = browser$findElements("css selector", ".-filter .title") %>% 
    map_chr(., ~.$getElementText()[[1]]),
  url = browser$findElements("css selector", ".title a") %>% 
    map_chr(., ~.$getElementAttribute("href")[[1]]),
  url_pdf = browser$findElements("css selector", ".pdf") %>% 
    map_chr(., ~.$getElementAttribute("href")[[1]])
)

economic_bulletin_metadata[, `:=` (id = str_extract(url_pdf, extract_official_id),
                                   document = "Economic bulletin",
                                   date = dmy(date),
                                   information = title)]
economic_bulletin_metadata[, year := year(date)]

# We close the browser for now
remDr$server$stop()

### Scraping monthly bulletins-----

#' This type of documents has disapeared in 2014, so no need for actualisation
if(! "old_ecb_doc" %in% ls){
page <- read_html(filter(ecb_paths, document == "Monthly bulletin")$url)
content <- page %>%
  html_elements(xpath = "/html/body/div[3]/main/div[4]/div[1]/dl")

mb_data <- data.table(year = content %>% 
                        html_elements("dt") %>% 
                        html_text2() %>% 
                        .[-1],
                      information = content %>% 
                        html_elements("dd") %>% 
                        .[-1])

mb_data[, url_pdf := map(information, ~html_elements(., "a") %>% 
                            html_attr("href"))]
mb_data <- dt_unnest(mb_data[, -"information"], url_pdf)[, .(year, V1)]
setnames(mb_data, "V1", "url_pdf")
mb_data[, `:=` (id = str_extract(url_pdf, "mb\\d+"),
                year = as.integer(year),
                document = "Monthly bulletin")]
mb_data[, date := paste("01", str_extract(id, "\\d{2}$"), str_extract(id, "\\d{4}")) %>% dmy()]
}

### Scraping articles and focus related to Economic Bulletin-----

econ_bulletin_doc <- c("Economic bulletin articles", "Economic bulletin focus")
scraped_data_type <- vector(mode = "list", length(econ_bulletin_doc))
names(scraped_data_type) <- econ_bulletin_doc
errors <- list() # to register errors

for(document_type in econ_bulletin_doc){
  
  #' We will only search for the years we don't have (we take the last one because it is likely that we did 
  #' not scrape all the content for this year)
  if("old_ecb_doc_" %in% ls()){
    existing_years <- old_ecb_doc %>%
      filter(document == document_type) %>% 
      pull(date) %>% 
      year() %>%
      unique()
    
    years <- setdiff(filter(ecb_paths, document == document_type)$first_year:format(Sys.Date(), "%Y"),
                     existing_years) %>% 
      c(., max(existing_years))
  } else {
    years <- filter(ecb_paths, document == document_type)$first_year:format(Sys.Date(), "%Y")
  }
  
  scraped_data <- vector(mode = "list", length = length(years))
  names(scraped_data) <- years
  
  for(year in years){
    tryCatch({
      url <- filter(ecb_paths, document == document_type)$url %>% 
        str_c(., year, "/html/index_include.en.html")
      html <- read_html(url)
      
      if(length(html) > 1){ # we take care that the page is not empty
        data <- data.table(date = html %>% 
                             html_elements("body > dt .date") %>%
                             html_text2(),
                           title = html %>%
                             html_elements(xpath = "/html/body/dd") %>%
                             html_element(".title a") %>% 
                             html_text2(),
                           authors = html %>%
                             html_elements(xpath = "/html/body/dd") %>%
                             html_element("ul") %>% 
                             html_text2(),
                           url = html %>%
                             html_elements(xpath = "/html/body/dd") %>%
                             html_element(".title a") %>% 
                             html_attr("href"), 
                           information = html %>%
                             html_elements(xpath = "/html/body/dd") %>%
                             html_element(".main-publication") %>% 
                             html_text2(),
                           additional_information = html %>%
                             html_elements(xpath = "/html/body/dd") %>%
                             html_element(".content-box"))
        
        if(!all(is.na(data$authors))){ #if authors is not NA
        additional_data <- data.table(url_authors = html %>%
                                        html_elements(xpath = "/html/body/dd") %>%
                                        html_element("ul"))
        additional_data[, id_authors := map(url_authors, ~ html_elements(., "li a") %>% 
                                              html_attr("href") %>% 
                                              str_extract("(?<=profiles/).+(?=\\.en)"))]
        additional_data[, url_authors := map(url_authors, ~ html_elements(., "li a") %>% 
                                               html_attr("href"))]
        
        data <- cbind(data, additional_data)
        }
        
        scraped_data[[paste(year)]] <- data
      }
      # log to follow progress    
      cat(paste("year", year, 
                "in", document_type,"done."),
          file = here(ecb_data_path, "how_far_in_metadata.txt"))
    }, error = function(e){
      cat(paste("year", year, 
                "in", document_type,"NOT DONE.\n"))
      errors[[paste(year)]] <- c(document_type, year)
    })
  }
  scraped_data <- compact(scraped_data) # remove empty elements
  scraped_data_type[[paste(document_type)]] <- rbindlist(scraped_data, idcol = "year",
                                                         fill = TRUE)
}

new_eb_data <- rbindlist(scraped_data_type, idcol = "document", fill = TRUE)

### Cleaning the economic bulletin scraped data---------
extract_eb_id <- "(eb(box)?|ebart|mb)(\\.)?[0-9_-]+(focus\\d+|article\\d+|pp\\d+-\\d+|box\\d+)?(?=(~|(\\.|_|)en|\\.pdf))" # this will be used to extract the official id from URL
new_eb_data[, `:=` (date = dmy(date),
                    year = as.integer(year),
                    id = str_extract(url, extract_eb_id),
                    url_pdf = ifelse(str_detect(url, "\\.pdf"), url, NA_character_))]
new_eb_data[, url := ifelse(!is.na(url_pdf), NA_character_, url)]

#' We clean the additional information stored in the `information` column, like the abstract, JEL codes, etc...
additional_information <- new_eb_data[, .(id, additional_information)]
additional_information[, existing_information := map_int(additional_information, length)]
additional_information <- additional_information[existing_information > 0,]

additional_information[, `:=` (info_type = map(additional_information, ~html_elements(., "dt") %>% 
                                                 html_text2()),
                               information = map(additional_information, ~html_elements(., "dd") %>% 
                                                   html_text2()))]
info_types <- dt_unnest(additional_information[, -c("additional_information", "existing_information")], info_type)[, .(id, V1)]
setnames(info_types, "V1", "info_type")
informations <- dt_unnest(additional_information[, -c("additional_information", "existing_information")], information)[, .(id, V1)]
setnames(informations, "V1", "information")
additional_information <- cbind(info_types, informations[, -"id"]) %>% 
  .[! is.na(info_type) & info_type != "1 May 1999",] # easier to clean weird value than to rescrape

# There can be two abstracts or JEL codes per paper, but they are exactly the same (don't know where the error comes from)
additional_information <- additional_information[!duplicated(additional_information, by = c("id", "info_type"))]

additional_information <- dcast(additional_information,
                                id ~ info_type,
                                value.var = "information") %>% 
  clean_names() # use of janitor to have more appropriate column names

#' We merge with the the original data
new_eb_data <- merge(new_eb_data[, -"additional_information"], additional_information, by = "id", all.x = TRUE)
new_eb_data <- rbindlist(list(economic_bulletin_metadata,
                              new_eb_data,
                              mb_data), # rewrite code with new data as mb_data needed only once
                         fill = TRUE)

## Scraping metadata of working papers----

scraped_data_type <- vector(mode = "list", length(ecb_paths$document[str_detect(ecb_paths$document, "papers")]))
names(scraped_data_type) <- ecb_paths$document[str_detect(ecb_paths$document, "papers")]
errors <- list() # to register errors

for(document_type in ecb_paths$document[str_detect(ecb_paths$document, "papers")]){

#' We will only search for the years we don't have (we take the last one because it is likely that we did 
#' not scrape all the content for this year)
if("old_ecb_doc_" %in% ls()){
  existing_years <- old_ecb_doc %>%
    filter(document == document_type) %>% 
    pull(date) %>% 
    year() %>%
    unique()
  
  years <- setdiff(filter(ecb_paths, document == document_type)$first_year:format(Sys.Date(), "%Y"),
          existing_years) %>% 
    c(., max(existing_years))
} else {
  years <- filter(ecb_paths, document == document_type)$first_year:format(Sys.Date(), "%Y")
}
  
scraped_data <- vector(mode = "list", length = length(years))
names(scraped_data) <- years

for(year in years){
  tryCatch({
  url <- filter(ecb_paths, document == document_type)$url %>% 
    str_c(., year, ".include.en.html")
  html <- read_html(url)
  Sys.sleep(3)
  
  if(length(html) > 1){ # we take care that the page is not empty
  data <- data.table(original_id = html %>%
                       html_elements(xpath = "/html/body/dd") %>%
                       html_element(".category") %>% 
                       html_text2(),
                     date = html %>% 
                       html_elements("body > dt .date") %>%
                       html_text2(),
                     title = html %>%
                       html_elements(xpath = "/html/body/dd") %>%
                       html_element(".category+ .title") %>% 
                       html_text2(),
                     authors = html %>%
                       html_elements(xpath = "/html/body/dd") %>%
                       html_element("ul") %>% 
                       html_text2(),
                     url_pdf = html %>%
                       html_elements(xpath = "/html/body/dd") %>%
                       html_element(".title a") %>% 
                       html_attr("href"), 
                     information = html %>%
                       html_elements(xpath = "/html/body/dd") %>%
                       html_element(".content-box"))

  additional_data <- data.table(url_authors = html %>%
                                  html_elements(xpath = "/html/body/dd") %>%
                                  html_element("ul"))
  additional_data[, id_authors := map(url_authors, ~ html_elements(., "li a") %>% 
                                  html_attr("href") %>% 
                                  str_extract("(?<=profiles/).+(?=\\.en)"))]
  additional_data[, url_authors := map(url_authors, ~ html_elements(., "li a") %>% 
                                        html_attr("href"))]
  
  data <- cbind(data, additional_data)

  scraped_data[[paste(year)]] <- data
  }
  # log to follow progress    
  cat(paste("year", year, 
            "in", document_type,"done."),
      file = here(ecb_data_path, "how_far_in_metadata.txt"))
  }, error = function(e){
    cat(paste("year", year, 
              "in", document_type,"NOT DONE.\n"))
    errors[[paste(year)]] <- c(document_type, year)
  })
}
scraped_data <- compact(scraped_data) # remove empty elements
scraped_data_type[[paste(document_type)]] <- rbindlist(scraped_data, idcol = "year")
}

new_wp_data <- rbindlist(scraped_data_type, idcol = "document", fill = TRUE)

### Cleaning the new scraped data---------
new_wp_data[, `:=` (date = dmy(date),
                 year = as.integer(year),
                 id = str_extract(url_pdf, extract_official_id),
                 url_pdf = str_c(ecb_domain, url_pdf))]

#' We clean the additional information stored in the `information` column, like the abstract, JEL codes, etc...
additional_information <- new_wp_data[, .(id, information)]
additional_information[, `:=` (info_type = map(information, ~html_elements(., "dt") %>% 
                                                 html_text2()),
                               information = map(information, ~html_elements(., "dd") %>% 
                                                   html_text2()))]
info_types <- dt_unnest(additional_information, info_type)[, .(id, V1)]
setnames(info_types, "V1", "info_type")
informations <- dt_unnest(additional_information, information)[, .(id, V1)]
setnames(informations, "V1", "information")
additional_information <- cbind(info_types, informations[, -"id"]) %>% 
  .[! is.na(info_type) & info_type != "1 May 1999",] # easier to clean weird value than to rescrape

# There can be two abstracts or JEL codes per paper, but they are exactly the same (don't know where the error comes from)
additional_information <- additional_information[!duplicated(additional_information, by = c("id", "info_type"))]

additional_information <- dcast(additional_information,
                                id ~ info_type,
                                value.var = "information") %>% 
  clean_names() # use of janitor to have more appropriate column names

#' We merge with the the original data
new_wp_data <- merge(new_wp_data[, -"information"], additional_information, by = "id", all.x = TRUE)

## Scraping metadata of press conferences, interviews and speeches-----------

press_documents <- ecb_paths %>% 
  filter(document %in% c("Press conference", "Monetary policy accounts", "Interviews", "Speeches")) %>% 
  pull(document)
scraped_data_type <- vector(mode = "list", length(press_documents))
names(scraped_data_type) <- press_documents


for(document_type in press_documents){
  #' We will only search for the years we don't have (we take the last one because it is likely that we did 
  #' not scrape all the content for this year)
  if("old_ecb_doc_" %in% ls()){
    existing_years <- old_ecb_doc %>%
      filter(document == document_type) %>% 
      pull(date) %>% 
      year() %>%
      unique()
    
    years <- setdiff(filter(ecb_paths, document == document_type)$first_year:format(Sys.Date(), "%Y"),
                     existing_years) %>% 
      c(., max(existing_years))
  } else {
    years <- filter(ecb_paths, document == document_type)$first_year:format(Sys.Date(), "%Y")
  }
  scraped_data <- vector(mode = "list", length = length(years))
  names(scraped_data) <- years
  
  for(year in 2008:2024){
    tryCatch({
    url <- filter(ecb_paths, document == document_type)$url %>% 
      str_c(., year, "/html/index_include.en.html")
    html <- read_html(url)
    Sys.sleep(5)
    
    if(length(html) > 1){ # we take care that the page is not empty
      data <- data.table(date = html %>% 
                           html_elements("body > dt .date") %>%
                           html_text2(),
                         title = html %>%
                           html_elements(xpath = "/html/body/dd") %>%
                           html_element(".title a") %>% 
                           html_text2(),
                         information = html %>%
                           html_elements(xpath = "/html/body/dd") %>%
                           html_element(".subtitle") %>% 
                           html_text2(),
                         url = html %>%
                           html_elements(xpath = "/html/body/dd") %>%
                           html_element(".title a") %>% 
                           html_attr("href") %>% 
                           str_c(ecb_domain, .))

      scraped_data[[paste(year)]] <- data
    }
      # log to follow progress    
      cat(paste("year", year, 
                "in", document_type,"done."),
          file = here(ecb_data_path, "how_far_in_metadata.txt"))
    }, error = function(e){
      cat(paste("year", year, 
                "in", document_type,"NOT DONE.\n"))
      errors[[paste(year)]] <- c(document_type, year)
    })
    
  }
  scraped_data <- compact(scraped_data) # remove empty elements
  scraped_data_type[[paste(document_type)]] <- rbindlist(scraped_data, idcol = "year")
}
file.remove(here(ecb_data_path, "how_far_in_metadata.txt"))
new_press_data <- rbindlist(scraped_data_type, idcol = "document")

### Cleaning the new scraped data---------
new_press_data[, `:=` (date = dmy(date),
                    year = as.integer(year),
                    id = str_extract(url, extract_official_id))]
new_press_data[document %in% c("Speeches", "Interviews"), authors := str_extract(title, "^[^:]*")] #match everything until the first occurence of semi colon

## Merging with original data------

new_data <- rbindlist(list(new_wp_data,
                      new_press_data,
                      economic_bulletin_metadata),
                      fill = TRUE)
# To finish
# if("old_ecb_doc" %in% ls()){
#   new_wp_data[! id %in% old_ecb_doc$id]
# }

new_ecb_doc <- new_data
saveRDS(new_ecb_doc, here(ecb_data_path, 
                          glue("ecb_metadata_{str_remove_all(Sys.Date(), \"-\")}.rds")))
