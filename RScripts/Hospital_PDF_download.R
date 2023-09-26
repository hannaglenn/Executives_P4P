library(readr)
library(pdftools)
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
hospital_pdf_locations <- readRDS(paste0(created_data_path, "/hospital_pdf_locations.rds"))

# first, extract years 2016-2020
#hospital_pdf_locations_20162020 <- hospital_pdf_locations %>% 
    #filter(tax_prd_yr >= 2016 & tax_prd_yr <= 2020)

# extract years 2010-2015
hospital_pdf_locations_20102015 <- hospital_pdf_locations %>% 
    filter(tax_prd_yr >= 2010 & tax_prd_yr <= 2015)

# create list of unique eins in hospital_pdf_locations
#unique_eins <- unique(hospital_pdf_locations_20162020$ein)
unique_eins <- unique(hospital_pdf_locations_20102015$ein)

# sort unique_eins in ascending order
unique_eins <- sort(unique_eins, decreasing = FALSE)

# make a list where all the data for each ein will be stored
#all_ein_list_20102015 <- vector("list", length(unique_eins))
#all_ein_list_20162020 <- readRDS(paste0(created_data_path, "/all_ein_list_2016_2020.rds"))
#all_ein_list_20102015 <- readRDS(paste0(created_data_path, "/all_ein_list_2010_2015.rds"))

# write a for loop for each ein number
for (i in 1:length(unique_eins)){
    tryCatch({
    # filter to only this ein
    ein_locations <- hospital_pdf_locations_20102015 %>% 
        filter(ein == unique_eins[i])
    
    # create a list the length of the number of rows
    individual_ein_list <- vector("list", length(ein_locations$ein))

    # loop through each row of data
    for (row in seq_along(ein_locations$ein)) {
        url <- ein_locations$pdf_url[row]
        ein <- ein_locations$ein[row]
        year <- ein_locations$tax_prd_yr[row]
        num <- ein_locations$num[row]

        # download the pdfs locally
        download.file(url, destfile = paste0(created_data_path, "/pdfs/", ein, '_', year, '_', num, ".pdf"),
                method="auto", quiet=FALSE, mode="wb")
        total_pages <- pdf_info(paste0(created_data_path, "/pdfs/", ein, '_', year,  '_', num,".pdf"))$pages

        if (total_pages > 8){
            # use OCR to extract text fom pages 7-9 (typical location of names)
            text <- pdf_ocr_text(paste0(created_data_path, "/pdfs/", ein, '_', year, '_', num, ".pdf"),
                                pages = c(7:9),
                                dpi = 600,
                                opw = "",
                                upw = "",
                                language = "eng")
        
            # get text into workable format
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
        
            # detect if the right pages were collected
            text <- text %>%
                    mutate(officer_keyword=ifelse(str_detect(phrase,"president|ceo|cfo|chief|chair|secretary|treasurer|physician"), 1, 0))
            contains_keywords <- ifelse(sum(text$officer_keyword)>0,1,0)

            # if the right pages were collected, then save the objects to individual_ein_list
            if (contains_keywords == 1) {
            individual_ein_list[[row]] <- list(ein = ein,
                                               year = year,
                                               text = text)
            }

            # if the right pages were not collected, loop through the pages until the right pages are collected
            if (contains_keywords == 0) {
                num_pages <- pdf_info(paste0(created_data_path, "/pdfs/", ein, '_', year, '_', num, ".pdf"))$pages

                found_page <- 0

                # extract text starting at the last page and working backwards
                while (found_page == 0) {
                    text <- pdf_ocr_text(paste0(created_data_path, "/pdfs/", ein, '_', year, '_', num, ".pdf"),
                                    pages = num_pages,
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
                    text <- text %>%
                        mutate(officer_keyword=ifelse(str_detect(phrase,"president|ceo|cfo|chief|chair|secretary|treasurer"), 1, 0),
                               section_keyword=ifelse(str_detect(phrase,"officers, directors, trustees, key employees, and highest compensated employees|
                                                                officers, directors, trustees, key employees, highest compensated employees"), 1, 0))
                    contains_both_keywords <- ifelse(sum(text$officer_keyword)>0 & sum(text$section_keyword)>0,1,0)

                    if (contains_both_keywords == 1) {
                        # get the text from the pages around the page where it found the key words
                        text <- pdf_ocr_text(paste0(created_data_path, "/pdfs/", ein, '_', year, '_', num, ".pdf"),
                                        pages = c((num_pages-2):(num_pages+1)),
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
                    
                        # save the objects to individual_ein_list
                        individual_ein_list[[row]] <- list(ein = ein,
                                                       year = year,
                                                       text = text)

                        # get out of while loop
                        found_page <- 1
                        }

                if (contains_both_keywords == 0) {
                    # if the key words were not found, then go to the next page unless you are at page 9,then exit loop
                    if (num_pages <= 9) {
                        found_page <- 1
                        individual_ein_list[[row]] <- list(ein = ein,
                                                           year = year,
                                                           text = "relevant text not found")
                    } else {
                        num_pages <- num_pages - 1
                    }
                }
                
                }
             }
        } 
        
        else {
            individual_ein_list[[row]] <- list(year = year,
                                               text = "document too short")
        }
    }
    # combine each element of individual_ein_list into one list of lists
    all_ein_list_20102015[[i]] <- list(ein_data=individual_ein_list, ein=unique_eins[i])
    saveRDS(all_ein_list_20102015, paste0(created_data_path, "/all_ein_list_2010_2015.rds"))
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

