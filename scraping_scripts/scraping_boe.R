# Scraping the Bank of England
# by Francois Claveau (and others)
# First created on Feb 1st 2020 (but it inherits from older scripts)
# Original script: "https://gitlab.com/bin-cirst/central_banks/-/blob/master/corpus/BankofEngland/01-web_scraping.R?ref_type=heads"

# This script scrapes the website of the Bank of England. It is constructed such that it can be ran again
# to update the database.


# Loading packages and paths----

source("packages_and_data_path.R")

## Loading small files and defining a few variables----

categories_to_scrape <- c("financial-stability-report",
                          "annual-report",
                          "quarterly-bulletin",
                          "financial-stability-paper",
                          "paper",
                          "article",
                          "external-mpc-discussion-paper",
                          "letter",
                          "record",
                          "statement",
                          "working-paper",
                          "speech",
                          "report")

# paths <- read.csv("../paths_for_centralbank_project.csv",sep = ";", skip=3,colClasses = c("character","character")) # the file storing relevant paths.
# dir.create(paths[2,"path"],recursive = TRUE,showWarnings = FALSE) # making sure the temporary directory exists.
# 
# path_bank <- "BankofEngland/"
# pdf_folder <- "pdf/"
# info_on_scrape_folder <- "scrape_info/"
# stable_dir_pdfs  <- paste0(paths[1,"path"],path_bank,pdf_folder)
# temp_dir_pdfs  <- paste0(paths[2,"path"],path_bank,pdf_folder)
# source("functions/functions_for_corpus.R")


### Loading old index if it exists ####

f_old_metadata <- here(boe_data_path, "dt_BoE_metadata.rds")
if(file.exists(f_old_metadata)){
  old_dt_BoE <- readRDS(f_old_metadata)
}

### Fetching all the relevant urls on the current sitemap ####

root_url_BoE <- "https://www.bankofengland.co.uk"
session <- bow(root_url_BoE)

# The static link
root_url_engCB <- session$robotstxt$sitemap$value

#get the xml sitemap
xml_sitemap <- rvest::read_html(root_url_engCB)
# x <- xml2::xml_contents(xml_sitemap)
data_frame_sitemap <- data.table(hyperlink= xml_sitemap %>% 
                                   rvest::html_elements(xpath = ".//loc") %>% 
                                   rvest::html_text())
data_frame_sitemap <- data_frame_sitemap[!hyperlink == root_url_BoE] # removing root link

# splitting the urls into high-level categories
data_frame_sitemap[, category := str_remove(hyperlink, "https://www.bankofengland.co.uk/") %>% 
                     str_remove("/.*")]
data_frame_sitemap[, url_length := nchar(hyperlink)] # column to sort on shorter url

# saving the categories ordered by size and which ones are scraped.
level1_cat_by_size <- data_frame_sitemap[, list(number_subpages = .N, shortest_url_for_level = .SD[which.min(url_length), hyperlink]),
                                         by=category][order(-number_subpages)]
level1_cat_by_size[, scraped := ifelse(category %in% categories_to_scrape, TRUE, FALSE)]
setcolorder(level1_cat_by_size, c("number_subpages","scraped"))
dir.create(here(boe_data_path, "scrape_info"), mode = "0755")
write.csv(level1_cat_by_size,
          file = here(boe_data_path, "scrape_info",glue("{Sys.Date()}_BoE_sitemap_by_level1_categories.csv")),
          row.names = FALSE)

# Narrowing down to relevant categories
dt_BoE <-  data_frame_sitemap[category %in% categories_to_scrape]

#### Looping through categories to get metadata ####
setkey(dt_BoE, category)

if("old_dt_BoE" %in% ls()){
  # Merging to old data to avoid replicating scrape
  
  setnames(dt_BoE,c("category", "url_length"),c("category_temp", "url_length_temp"))
  new_dt_BoE <-  old_dt_BoE %>% 
    full_join(dt_BoE)

  # Preparing the news IDS to attribute
  i_max <- c(new_dt_BoE[!is.na(doc_ID), doc_ID], dir(here(boe_data_path, "pdf"))) %>% 
    str_extract("[0-9]+") %>%
    unique() %>% 
    as.integer() %>% 
    max()
  
  ids_to_give <- fct_new_doc_ID((i_max+1):(i_max+nrow(new_dt_BoE[is.na(doc_ID)])),
                                "BoE")
    
    (i_max+1):(i_max+nrow(new_dt_BoE[is.na(doc_ID)])) %>% 
    formatC(width = 6,flag=0) %>% 
    str_c("BoE", .)
  
  # Working on columns of new rows
  new_dt_BoE[, new_scrape := ifelse(is.na(doc_ID), TRUE, FALSE)]
  new_dt_BoE[is.na(doc_ID),`:=`(doc_ID = ids_to_give,
                                category = category_temp,
                                url_length = url_length_temp)]
  new_dt_BoE$category_temp <- NULL ; dt_BoE$url_length_temp <- NULL
} else{ 
  # Or creating the relevant columns
  dt_BoE$old_ID <- 1:nrow(dt_BoE) # old way of creating a temporary id
  dt_BoE[,doc_ID := fct_new_doc_ID(old_ID,"BoE")]
  setkey(dt_BoE,category,doc_ID)
  
  dt_BoE[,`:=`(title = character(), 
               abstract = character(),
               additional_info = character(),
               new_scrape =TRUE
  )]
}

new_dt_BoE$temp_date <-  character()

for(categ in categories_to_scrape){
  for(i in new_dt_BoE[new_scrape == TRUE & category == categ,doc_ID]){ # each page in a given category
    # getting the html
    page <- new_dt_BoE[doc_ID == i, hyperlink] %>% 
      read_html
    
    # extracting relevant information
    extracted_title <-  page %>% 
      html_nodes("h1") %>% 
      html_text()
    extracted_url_doc <-  page %>% 
      html_nodes(".btn-pubs") %>% 
      html_attr("href") %>% 
      sub("\\?la.*", "", .) %>% 
      paste0(root_url_BoE, .)
    extracted_date <-   page %>%  
      html_nodes(".published-date") %>% 
      html_text(trim = TRUE) %>% 
      ifelse(length(.) > 0, ., NA)
    some_text <- page %>% 
      html_nodes(".page-section .content-block h2 , .page-section .content-block h3") %>% 
      html_text(trim = TRUE)
    if(categ %in% c("quarterly-bulletin", "speech", "paper", "statement","article", "letter", "report")){
      # In some categories of document, the content of the hero class is irrelevant while it is relevant in other categories. 
      # So we extract this content only for the relevant categories.
      some_text <-  page %>% 
        html_nodes(".hero") %>% 
        html_text(trim= TRUE) %>% 
        c(., some_text)
    }
    extracted_additional_info <-  ifelse(length(some_text) > 0,
                                         paste0(some_text, collapse="\n"), 
                                         NA)
    extracted_abstract <- page %>% 
      html_nodes('.page-section .content-block .page-content p')  %>% 
      html_text()
    
    irrelevant_parts <- page %>% 
      html_nodes(".page-section .content-block .page-content a")  %>% 
      html_text()
    extracted_abstract <- extracted_abstract[! extracted_abstract %in% irrelevant_parts]
    extracted_abstract <-  ifelse(length(extracted_abstract) > 0,
                                  paste0(extracted_abstract, collapse="\n"), 
                                  NA)
    
    # Printing a warning if some extractions are bigger than  unexpected.
    if(length(extracted_title)>1 | length(extracted_date)>1|
       length(extracted_additional_info)>1 | length(extracted_abstract)>1){
      print(paste0("There's something wrong with doc_ID ", i, " in category ", categ,", which has url: ", 
                   new_dt_BoE[doc_ID == i, hyperlink]))
      
      # Solution (after checking) if title:
      extracted_title <- extracted_title[1]
    } 
    
    # storing the info for this page
    new_dt_BoE[doc_ID == i,`:=`(title = extracted_title, 
                            temp_date = extracted_date, 
                            abstract = extracted_abstract,
                            additional_info = extracted_additional_info,
                            url_doc =  list(extracted_url_doc)
    )]
    
    # log to follow progress    
    cat(paste("item", which(new_dt_BoE[new_scrape == TRUE & category == categ,doc_ID] %in% i), 
              "in", nrow(new_dt_BoE[new_scrape == TRUE & category == categ]),"done for category", categ),
        file = here(boe_data_path, "how_far_in_metadata.txt"))
  }
}

file.remove(here(boe_data_path, "how_far_in_metadata.txt"))

## Corrections to url----

new_dt_BoE[new_scrape==TRUE, nb_other_urls:= url_doc %>% unlist() %>% length() -1 ,by=doc_ID] # temporary column 
new_dt_BoE[new_scrape==TRUE, main_doc_url:= url_doc %>% unlist() %>% .[1], by=doc_ID] # url for main doc 
new_dt_BoE[new_scrape==TRUE & nb_other_urls>0, other_doc_url:=  list(make_new_list(url_doc,skip_first = TRUE)), by=doc_ID] # url for other docs (as list)

# small corrections to url:
new_dt_BoE[grepl(paste0(root_url_BoE,root_url_BoE),main_doc_url), 
           main_doc_url:= sub(paste0(root_url_BoE,root_url_BoE),
                              root_url_BoE, main_doc_url)]
new_dt_BoE[grepl("\\n",main_doc_url),main_doc_url:=sub("\\n","",main_doc_url)]

new_dt_BoE[, temp_nb_main_url:= length(main_doc_url),by=doc_ID]

# Saving a temporary version of the metadata
saveRDS(new_dt_BoE, file = here(boe_data_path, glue("{Sys.Date()}_dt_BoE_not_clean.rds")))

# Downloading pdf----

# doc_ids in the pdf folders (stable and new)
docs_we_have <- dir(here(boe_data_path, "pdf")) %>% 
  str_remove(".pdf")

# compared to lines in metadata 
docs_to_download <- new_dt_BoE[!doc_ID %in% docs_we_have, doc_ID]
pdf_to_download <- new_dt_BoE[doc_ID %in%docs_to_download, list(doc_ID, main_doc_url)] %>%
  .[str_detect(main_doc_url, ".pdf")]
setnames(pdf_to_download, c("id", "url_pdf"))

path <- here(boe_data_path, "pdf") 
domain <- "https://www.bankofengland.co.uk/"

rstudioapi::jobRunScript(here("helper_scripts",  "background_polite_download.R"),
                         importEnv = TRUE,
                         workingDir = here())

### Downloading each doc  only if it is a pdf

not_pdf <- vector("character")
prob_download <- vector("character")

for(id in docs_to_download){
  u=new_dt_BoE[doc_ID == id, main_doc_url]
  if(grepl(".pdf",u)){
    f=paste0(temp_dir_pdfs, id, ".pdf")
    #tryCatch({
    download.file(u, f, mode="wb")
    #  },
    #warning = function(e){
    #  return(id)
    #    }
    #)
  } else{
    not_pdf <- c(not_pdf,id)
  }
}

# Storing which lines still have no downloaded files after the process
docs_we_have <- dir(here(boe_data_path, "pdf")) %>% 
  str_remove(".pdf")
write.csv(dt_BoE[!doc_ID %in%docs_we_have, list(doc_ID,main_doc_url,title)],
          file = here(boe_data_path,
                      "scrape_info",
                      glue("{Sys.Date()}_BoE_entry_without_file.csv")))

