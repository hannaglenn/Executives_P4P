library(readr)
library(pdftools)
library(tidytext)
library(dplyr)
library(stringr)
library(tesseract)

# In this script, I read in the pdfs that were manually downloaded after finding errors from the 
# first pull of pdfs. 

source("paths.R")

# Read in and clean text from all pdfs in the "AHA Manual PDFs" folder
pdfs <- list.files(paste0(created_data_path, "AHA_pdfs_manual_matched"),
                   full.names=FALSE)
# create empty list to store information in 
AHA_ein_list_manualmatched <- vector("list", length(pdfs))


# write for loop to extract text from all files
for (i in seq_along(pdfs)){
   tryCatch({
  # get the file name
  file_name <- str_split(pdfs[[i]], "_") 
  ein <- file_name[[1]][[1]]
  year <- str_remove(file_name[[1]][[3]], ".pdf")
  
  # get total number of pages
  total_pages <- pdf_info(paste0(created_data_path, "/AHA_pdfs_manual_matched/",pdfs[[i]]))$pages
  # create empty list to store data from each page
  individual_page_list <- vector("list", total_pages)

  # loop through each of the pages
  for (page in 1:total_pages){
    tryCatch({
    text <- pdf_ocr_text(paste0(created_data_path, "/AHA_pdfs_manual_matched/",pdfs[[i]]),
                         pages = page,
                         dpi = 600,
                         opw = "",
                         upw = "",
                         language = "eng")
    #get text into workable format
    text <- str_split(text, "\\n")
    #combine elements to one list
    text <- str_c(text, collapse = " ") 
    #change to tibble
    text <- tibble(line = 1:length(text), text=text)
    # one row for each phrase
    text <- text %>%
      tidytext::unnest_tokens(phrase, text, token = "regex", pattern = "\"") %>%
      filter(phrase != ", " & phrase != ", \n") %>%
      select(phrase)
    
    # detect if there is relevant text on this page
    text <- text %>%
      mutate(section_keyword = ifelse(str_detect(phrase, "officers, directors, trustees, kep employees, and highest compensated employees|officers, directors, trustees, key employees, highest compensated employees|current officers, directors, trustees, and key employees|highest compensated employees|compensation of officers, directors,trustees, key employees"), 1, 0))
    contains_keywords <- ifelse(sum(text$section_keyword)>0,1,0)
    
    if (contains_keywords==1){
      individual_page_list[[page]] <- text
    }
    if (contains_keywords==0){
      individual_page_list[[page]] <- NULL
    }
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
  text_data <- do.call(rbind, individual_page_list)
  AHA_ein_list_manualmatched[[i]] <- c(ein, year, text_data)
  saveRDS(AHA_ein_list_manualmatched, paste0(created_data_path, "AHA_ein_list_manualmatched.rds"))
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}




