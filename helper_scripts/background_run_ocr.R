# Script for running OCR with tesseract

library(tidyverse)
library(tesseract)
library(pdftools)
library(here)
library(glue)

for(i in 1:length(pdf_without_ocr)){
  tryCatch(
    {
  message(glue("Iteration {i}. {length(pdf_without_ocr) - i + 1} iterations remaining."))
  
  # In case of stopped, not to start again from zero
  if(! str_remove(pdf_without_ocr[i], ".pdf") %in% str_remove(list.files(), "_\\d+\\.tiff")){
  img_file <- pdf_convert(here(background_data_path, "pdf", pdf_without_ocr[i]), 
                          format = "tiff", 
                          dpi = 300) %>% 
    tibble(img = ., page = str_extract(., "(?<=_)\\d+"))
  } else {
    img_file <- list.files()[str_detect(list.files(), str_remove(pdf_without_ocr[i], ".pdf"))] %>% 
      tibble(img = ., page = str_extract(., "(?<=_)\\d+"))
  }
  text <- ocr(img_file$img, engine = tesseract("eng"))
  text <- tibble(text) %>%
    mutate(text = text,
           page = as.integer(img_file$page),
           file = str_remove(pdf_without_ocr[i], "\\.pdf")) %>% 
    arrange(page)
  new_text_ocr[[i]] <- text
  
  # We remove all the .png we have created
  file.remove(img_file$img)
  },
    error = function(e) {
      print(glue("Problem OCR pdf"))
      new_text_ocr[[i]] <- NA 
  }
  )
}

