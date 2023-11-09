# function to download a file with a time interval
slow_download <- function(url, destfile, sleep_time){
  download.file(url = url,
                destfile = destfile,
                method = "wininet",
                mode = "wb")
  Sys.sleep(sleep_time)
}

# Another variant with the polite package that allows ethical scraping
polite_download <- function(domain, url, ...){
  bow(domain) %>% 
    nod(glue(url)) %>% 
    rip(...)  
}

# Function to clean bis metadata 

cleaning_bis_metadata <- function(data){
  data_cleaned <- data %>% 
    mutate(pdf_link = str_replace(url, "htm$", "pdf"),
           file_name = str_extract(pdf_link, "(?<=\\/)r\\d+.+(?=\\.pdf)"),
           speaker = str_remove(speaker, "by "),
           speaker_from_title = str_extract(title, ".+?(?=:)"),
           speaker_cleaned = ifelse(is.na(speaker), speaker_from_title, speaker),
           speech_type = str_extract(description, "(.+?)(?=, )") %>% 
             str_remove(" by .+"),
           title = str_remove(title, paste0(speaker_from_title, ": ")),
           cb_info = str_extract(description, "(?<=, ).+") %>% 
             str_extract("(.+?)(?=, )"),
           central_bank = str_extract(cb_info, "((C|c)entral |Reserve )?(B|b)ank of (the )?[A-Z][a-z]+( (of )?[A-Z][a-z]+( [A-Z][a-z]+)?)?")) 
  
  central_banks <- c("Bundesbank",
                     "Europ(\\.|ean) Central Bank",
                     "ECB", # second name
                     "Swiss National Bank",
                     "Banque nationale suisse",
                     "Sveriges Ri(s)?k(s)?bank",
                     "Norges Bank",
                     "Danmarks National Bank",
                     "Netherlands Bank",
                     "Nederlandsche Bank", # second name
                     "Banco de Portugal", # second name
                     "Banco de España",
                     "Banque (de|of) France", # second name
                     "Czech National Bank",
                     "Austrian National( B|b)ank", # second name
                     "Oesterreichische Nationalbank", # third name
                     "Central Bank (\\&|and) Financial Services Authority of Ireland", # second name
                     "Bulgarian National Bank",
                     "Croati(a|o)n National Bank", # second name
                     "Centrale Bank van Curaçao en Sint Maarte",
                     "National Bank of the Republic of (North )?Macedonia",
                     "Central Bank of the United Arab Emirates",
                     "Bahrain Monetary Agency",
                     "Saudi Arabian Monetary Agency",
                     "South African Reserve Bank",
                     "Reserve Bank of india", # second name
                     "Hong Kong Monetary Authority",
                     "Hong Kong Monetary", # second name
                     "Monetary Authority of Singapore",
                     "Bangko Sentral ng Pilipinas",
                     "Bank Indonesia",
                     "Bank Negara Malaysia",
                     "Maldives Monetary Authority",
                     "Cayman Islands Monetary Authority",
                     "Monetary Authority of Macao",
                     "Mon\\. Authority of Macao", # second name
                     "Federal Reserve",
                     "Fed\\. Res(erve|\\.) System", # second name
                     "Eastern Caribbean Central Bank",
                     "Australian Reserve Bank",
                     "Bank for International Settlements") # not a central bank
  
  data_cleaned <- data_cleaned %>% 
    mutate(central_bank = ifelse(is.na(central_bank), str_extract(description, paste0(central_banks, collapse = "|")), central_bank),
           central_bank = ifelse(is.na(central_bank), str_extract(description, "((C|c)entral |Reserve )?(B|b)ank of (the )?[A-Z][a-z]+( (of )?[A-Z][a-z]+( [A-Z][a-z]+)?)?"), central_bank),
           central_bank = ifelse(is.na(central_bank), str_extract(title, "((C|c)entral |Reserve )?(B|b)ank of (the )?[A-Z][a-z]+( (of )?[A-Z][a-z]+( [A-Z][a-z]+)?)?"), central_bank)) # for the very first years
  
  
  # cleaning CBs to uniformise names
  
  correct_name <- tribble(
    ~error, ~correction,
    "Austria", "Austrian National Bank",
    "Oesterreichische", "Austrian National Bank",
    "Banco de España", "Bank of Spain",
    "Banco de Portugal", "Bank of Portugal",
    "Bank of China", "Bank of China",
    "Bank of England", "Bank of England",
    "France", "Bank of France",
    "Bank of Japan", "Bank of Japan",
    "Bank of Lativa", "Bank of Latvia",
    "Malaysia", "Central Bank of Malaysia",
    "Sveriges", "Sveriges Riksbank",
    "Sweden", "Sveriges Riksbank",
    "Uganda", "Bank of Uganda",
    "Suisse", "Swiss National Bank",
    "Ireland", "Central Bank of Ireland",
    "Morocco", "Bank of Morocco",
    "Turkey", "Central Bank of the Republic of Turkey",
    "Russia", "Bank of Russia",
    "Nederlandsche Bank", "Netherlands Bank",
    "ECB", "European Central Bank",
    "Danmarks", "Bank of Denmark",
    "Hong Kong", "Hong Kong Monetary Authority",
    "Fed\\. Res(erve|\\.) System", "Federal Reserve",
    "Macao", "Monetary Authority of Macao",
    "Norges Bank", "Central Bank of Norway",
    "South Africa", "South African Reserve Bank",
    "New Zealand", "Reserve Bank of New Zealand",
    "India", "Reserve Bank of India",
    "Australia", "Reserve Bank of Australia"
  )
  
  for(i in 1:nrow(correct_name)){
    data_cleaned <- data_cleaned %>% 
      mutate(central_bank = ifelse(str_detect(central_bank, correct_name$error[i]), correct_name$correction[i], central_bank))
  }
  
  data_cleaned <- data_cleaned %>% 
    mutate(central_bank = tolower(central_bank),
           last_extraction = Sys.Date()) # we keep the memory of the last time we have extracted the data.
}

# Function to extract the text of a pdf in a data.frame
extracting_text <- function(file, path) {
  tryCatch(
    {  
  message(glue("Extracting text from {file}"))
  text_data <- pdf_text(here(path, file)) %>%
    as_tibble() %>% 
    mutate(page = 1:n(),
           file = file) %>% 
    rename(text = value)
    },
  error = function(e) {
    print(glue("No pdf exist"))
    text_data <- tibble(text = NA,
                        page = 1,
                        file = file)
  }
  )
}
