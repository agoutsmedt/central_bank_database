# Loading packages and paths----

source("packages_and_data_path.R")

# Extracting the ECB texts----

#' The aim of this script is to extract the texts of different ECB documents based on the metadata
#' scraped in `scraping_ecb_metadata`

## Paths and data----         
domain <- "https://www.ecb.europa.eu/"

#' We load the old data
#' 
file <- list.files(ecb_data_path) %>% 
  as_tibble() %>% 
  filter(str_detect(value, "ecb_metadata_\\d+\\.rds")) %>% 
  mutate(date = str_extract(value, "\\d{8}") %>% ymd()) %>% 
  arrange(desc(date)) %>% 
  slice(1) %>% 
  pull(value)

ecb_metadata <- readRDS(here(ecb_data_path,
                            file))

text_file <- list.files(ecb_data_path) %>% 
  as_tibble() %>% 
  filter(str_detect(value, "ecb_text_\\d+\\.parquet")) %>% 
  mutate(date = str_extract(value, "\\d{8}") %>% ymd()) %>% 
  arrange(desc(date)) %>% 
  slice(1) %>% 
  pull(value)

ecb_text <- read_parquet(here(ecb_data_path,
                              text_file))
setDT(ecb_text, key = "id")

# Extracting Text from URLs-----

text_to_scrape <- ecb_metadata[str_detect(url, "\\.html$"), .(id, url, document)]
all_text <- vector(mode = "list", length = length(text_to_scrape$id))
names(all_text) <- text_to_scrape$id
errors <- vector(mode = "list")

# To start again with the errors value (due to problem of connexion)
if(length(errors) > 0){
  ids <- errors$id
} else {
  ids <- text_to_scrape[! id %in% names(compact(all_text))]$id
}

for(i in ids){
  tryCatch({
    # getting the html
    page <- text_to_scrape[id == i, url] %>% 
      read_html()
    
    # extracting relevant information
    data_text <- data.table(text = page %>% 
                              html_nodes(".titlepage .title, .section h1, .section h2, .section h3, .section p") %>% 
                              html_text2() %>% 
                              str_split(pattern = "\n\n") %>% 
                              unlist() %>% 
                              .[. != ""], # empty strings happen sometimes
                            section = NA_character_)
    
    #' For interviews, we want to know when the text corresponds to the questions asked by journalists
    #' (they are in bold)
    if(text_to_scrape[id == i, document] %in% c("Interviews", "Press conference")){
      # To remove introductory information we already have
      data_text <- data_text[! str_detect(text, "^Interview with")]
      
      questions <- page %>% 
        html_elements("strong") %>% 
        html_text2() %>% 
        .[. != ""] %>% 
        .[!str_detect(., "^Figure|^Table|^Disclaimer")] # To remove some weird interviews on youtube with no question
      
      if(length(questions) > 0){
        data_text[which(text %in% questions), section := "question"]
        if(any(str_detect(questions, "Question:|SPIEGEL:"))) data_text[str_detect(text, "^Question:|SPIEGEL:"), section := "question"]
        if(!all(is.na(data_text$section))) data_text[is.na(section), section := "answer"] # take care of when questions are not fitting
        
        # We don't want what comes before the first question (like in press conference) to be called "answer"
        if(!all(is.na(data_text$section))) first_question <- which(data_text$section == "question") %>% min() else first_question <- 0
        if(first_question > 1) data_text[1:(first_question-1), section := NA_character_] # for interviews, which start by a question
        if(!all(is.na(data_text$section))) data_text[is.na(section), section := "press conference"]
        }
      } 
  
    useless_info <- page %>% 
      html_elements("h1+ p strong , .impressum, .ecb-authors, #toc1~ .ecb-authors+ p,.ecb-pressContentSubtitle, .ecb-publicationDate") %>% 
      html_text2() %>% 
      str_split(pattern = "\n\n") %>% 
      unlist()
    
    if(length(useless_info) > 0){
      data_text[which(text %in% useless_info), section := "useless_info"]
      if(text_to_scrape[id == i, document] == "Economic bulletin"){
        data_text <- dt_fill(data_text, section, .direction = "down") # does not work for addition EB documents
      } 
      # In case sections exist but no way to match it in the text
      if(nrow(data_text[which(text %in% useless_info),]) > 0) data_text <- data_text[is.na(section) | section != "useless_info",]
      }
    #' We want to extract the titles/sections of the article to remove them from the main text, while 
    #' keeping the information. Also useful to identify bibliography sections.
    sections <- page %>% 
      html_elements(".titlepage .title, .section h1, .section h2, .section h3") %>% 
      html_text2() %>% 
      .[. != ""] %>% # empty sections happen sometimes
      unique() # just a precaution
    
    if(length(sections) > 0){
      data_text[which(text %in% sections), section := text]
      data_text <- dt_fill(data_text, section, .direction = "down")
    }
    
    # Remove irrelevant part in accounts
    if(text_to_scrape[id == i, document] == "Monetary policy accounts"){
      irrelevant_sections <- c("Related press and web releases",
                               "Members",
                               "Other attendees",
                               "Accompanying persons",
                               "Other ECB staff")
      data_text <- data_text[! section %in% irrelevant_sections]
    }
    
    # To remove boxes and articles from economic bulletin
    if(text_to_scrape[id == i, document] == "Economic bulletin"){
      start_irrelevant <- which(data_text$text %in% c("Boxes", "Articles", "Article")) %>% 
        min()
      data_text <- data_text[1:(start_irrelevant-1),]
    }
    
    # To remove the name of sections
    # The if is in case sections exist but no way to match it in the text
    if(nrow(data_text[which(text %in% sections),]) > 0) data_text <- data_text[-which(text %in% sections),]
    
    #' We get the footnotes separately. It can create noise if mixed to the main text. But it is also interesting
    #' to have them separately to extract references.
    footnotes <- page %>% 
      html_elements(".footnotes p, .footnotes li") %>% 
      html_text2()
    
    if(length(footnotes) > 0){
      add_footnotes <- data.table(text = footnotes,
                                  section = "footnotes")
      data_text <- rbind(data_text,
                         add_footnotes)
    }
      
      all_text[[paste(i)]] <- data_text
      # log to follow progress    
      cat(paste("item", i, 
                "at", text_to_scrape[id == i, url],"done for", text_to_scrape[id == i, document]),
          file = here(ecb_data_path, "how_far_in_metadata.txt"))
      
    }, error = function(e){
      cat(paste("item", i, 
                "at", text_to_scrape[id == i, url],"NOT DONE.\n"))
      errors[[paste(i)]] <- data.table(id = i, 
                                       url = text_to_scrape[id == i, url], 
                                       document = text_to_scrape[id == i, document])
    })
  } 

new_text <- rbindlist(all_text, idcol = "id",
          fill = TRUE) %>% 
  .[! is.na(text)] #happens with some weird interviews (like interviews on twitter = with no text)
file.remove(here(boe_data_path, "how_far_in_metadata.txt"))

#' Merging with existing texts
#' 

ecb_text <- rbindlist(list(ecb_text[! id %in% new_text$id],
                           new_text),
                      fill = TRUE)

# precautionary save
write_parquet(ecb_text, 
              here(ecb_data_path,
                   glue("ecb_text_{str_remove_all(Sys.Date(), \"-\") }.parquet")),
              compression = "brotli")

# Downloading pdf documents----
if(! "pdf" %in% list.files(ecb_data_path)) dir.create(here(ecb_data_path, 
                                                           "pdf"))
# We first need to check which pdf we already have in our directory
list_pdf <- list.files(here(ecb_data_path, 
                            "pdf")) %>% 
  str_remove("\\.pdf")

pdf_to_download <- ecb_metadata %>%  # This is the list of the remaining pdf to get
  filter(is.na(url),
         ! is.na(url_pdf), 
         ! id %in% list_pdf) %>% 
  select(id, url_pdf) 

path <- here(ecb_data_path, "pdf")
rstudioapi::jobRunScript(here("helper_scripts",  "background_polite_download.R"),
                         importEnv = TRUE,
                         workingDir = here())

# Extracting texts----

if(ecb_text %in% ls()){
  text_to_extract <- ecb_metadata %>% 
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
  bind_rows() %>% 
  filter(!is.na(text)) %>% 
  mutate(id = str_remove(file, "\\.pdf"), .before = everything())
setDT(new_text, key = "id")

## Character recognition on problematic pdfs in new texts----

# We identify the pdf for which we are unable to recognize text. That's the pdf with zero
# characters all along the file

pdf_without_ocr <- new_text %>% 
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
new_text_ocr <- bind_rows(new_text_ocr) %>% 
  mutate(id = str_remove(file, "\\.pdf"), .before = everything())
new_text <- rbind(new_text[! file %in% pdf_without_ocr],
                  new_text_ocr) %>% 
  arrange(file, page)

## Merging the data-----

# We merge with the data that we have already collected, if it exists
if(is.data.frame(ecb_text)){
  ecb_text <- rbindlist(list(ecb_text,
                 new_text),
            fill = TRUE)
} else {
  ecb_text <- new_text
} 

write_parquet(ecb_text, 
              here(ecb_data_path,
                   glue("ecb_text_{str_remove_all(Sys.Date(), \"-\") }.parquet")),
              compression = "brotli")
