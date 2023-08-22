library(readr)
library(pdftools)
library(tm)
library(tidytext)
library(dplyr)
library(stringr)
library(tesseract)
library(tidytext)

# This script takes each pdf location created in "Hospital_PDF_locations.R" and 
# downloads the pdfs locally. With each download, I convert the text to usable data and 
# store it in the "hospital_pdf_locations" dataset. 

# Run the entire "paths.R" script
source("paths.R")

# Read in "hospital_pdf_locations" dataset
hospital_pdf_locations <- readRDS(paste0(created_data_path, "/hospital_pdf_locations.rds")))

# Do this if it's the first time running this code, otherwise comment out
#hospital_text_list <- vector("list", nrow(hospital_pdf_locations))

# uncomment this if you already have some elements of hospital_text_list saved
hospital_text_list <- readRDS(paste0(created_data_path, "/hospital_text_list.rds"))

# create list of unique eins in hospital_pdf_locations
unique_eins <- unique(hospital_pdf_locations$ein)

start_time <- Sys.time()
for (i in 401:700) {
  tryCatch({
  url <- hospital_pdf_locations$pdf_url[i]
  ein <- hospital_pdf_locations$ein[i]
  year <- hospital_pdf_locations$tax_prd_yr[i]
  # download the pdfs locally
  download.file(url, destfile = paste0(windows_path, "/PDFs/", ein, '_', year, ".pdf"),
                method="auto", quiet=FALSE, mode="wb")
  
  text <- pdf_ocr_text(paste0(windows_path, "/PDFs/", ein, '_', year, ".pdf"),
                        dpi = 600,
                        opw = "",
                        upw = "",
                        language = "eng")
  text <- str_split(text, "\n")
  # combine elements to one list
  text <- str_c(text, collapse = " ")
  # change to tibble
  text <- tibble(line = 1:length(text), text = text)
  # one row for each phrase
  text <- text %>% 
        tidytext::unnest_tokens(phrase, text, token = "regex", pattern = "\"") %>%
        filter(phrase != ", " & phrase != ", \n") %>%
        select(phrase)
  # replace ith element with list of ein, year, and text
  hospital_text_list[[i]] <- list(ein = ein, year = year, text = text)
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}
end_time <- Sys.time()
print(end_time - start_time)

saveRDS(hospital_text_list, paste0(windows_path, "created_data/hospital_text_list.rds"))
    
# rows 1-100 done in 6 hours
# rows 101-300 done in 10 hours
# rows 301-400 done in 5 hours
# rows 401-700 done in 15 hours
