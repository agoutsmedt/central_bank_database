# Script for downloading pdf via a job session

library(polite)
library(glue)

for(i in 1:nrow(pdf_to_download)){
  tryCatch(
    {
      bow(domain) %>% 
        nod(pdf_to_download$url_pdf[i]) %>% 
        rip(destfile = paste0(pdf_to_download$id[i], ".pdf"),
            path = path)
    },
    error = function(e) {
      print(glue("No pdf exist for {i}"))
    }
  )
}


