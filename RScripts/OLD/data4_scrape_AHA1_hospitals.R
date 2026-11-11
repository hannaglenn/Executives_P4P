library(readr)
library(pdftools)
library(tidytext)
library(dplyr)
library(stringr)
library(tesseract)

#### ORDER : 4 #########

# In this script, I find the pdf url of every ein that is matched to an AHA hospital (which is done in 
# data3_match_AHA_hospitals.R". I download these pdfs locally and extract the text from them in a usable format.  

# Run the entire "paths.R" script
source("paths.R")

# Read in "hospital_pdf_locations" dataset
hospital_pdf_locations <- readRDS(paste0(created_data_path, "/hospital_pdf_locations.rds"))
einlist_AHAmatching_1 <- readRDS(paste0(created_data_path, "/einlist_AHAmatching_1.rds")) %>%
    mutate(keep=1)

hospital_pdf_locations <- hospital_pdf_locations %>% 
    left_join(einlist_AHAmatching_1, by="ein") %>%
    filter(keep==1) %>%
    select(-keep)

# I need years 2008-2015 (tax forms are turned in the following year)
hospital_pdf_locations <- hospital_pdf_locations %>% 
    filter(tax_prd_yr >= 2009 & tax_prd_yr <= 2016) %>%
    group_by(ein,tax_prd_yr) %>%
    mutate(num=row_number()) %>%
    ungroup()

# create list of unique eins in hospital_pdf_locations
unique_eins <- unique(hospital_pdf_locations$ein)

# sort unique_eins in ascending order
unique_eins <- sort(unique_eins, decreasing = FALSE)

# make a list where all the data for each ein will be stored
#AHA_ein_list <- vector("list", length(unique_eins))
AHA_ein_list <- read_rds(paste0(created_data_path, "/AHA_ein_list.rds"))

# write a for loop to look at each page and extract the relevant information
for (i in 1:length(unique_eins)){
    tryCatch({

    # filter to only this ein
    ein_locations <- hospital_pdf_locations %>% 
        filter(ein == unique_eins[i])
    
    # create a list the length of the number of pdf urls I have for this ein
    individual_ein_list <- vector("list", length(ein_locations$ein))

    # loop through each row for this ein
    for (row in seq_along(ein_locations$ein)) {
        tryCatch({
            url <- ein_locations$pdf_url[row]
            ein <- ein_locations$ein[row]
            year <- ein_locations$tax_prd_yr[row]
            num <- ein_locations$num[row]

            # download the pdfs locally
            download.file(url, destfile = paste0(created_data_path, "/AHA_pdfs/", ein, '_', year, '_', num, ".pdf"),
                    method="auto", quiet=FALSE, mode="wb")
            total_pages <- pdf_info(paste0(created_data_path, "/AHA_pdfs/", ein, '_', year,  '_', num,".pdf"))$pages

             # create empty list to store data from each page
            individual_page_list <- vector("list", total_pages)

            # loop through each of the pages
            for (page in 1:total_pages){
                # use OCR to extract text
                text <- pdf_ocr_text(paste0(created_data_path, "/AHA_pdfs/", ein, '_', year, '_', num, ".pdf"),
                                pages = page,
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
        
                # detect if there is relevant text on this page
                text <- text %>%
                    mutate(section_keyword=ifelse(str_detect(phrase,"officers, directors, trustees, key employees, and highest compensated employees|
                                                                officers, directors, trustees, key employees, highest compensated employees|
                                                                current officers, directors, trustees, and key employees"), 1, 0))
                contains_keywords <- ifelse(sum(text$section_keyword)>0,1,0)

                if (contains_keywords==1){
                    individual_page_list[[page]] <- text
                }
                if (contains_keywords==0){
                    individual_page_list[[page]] <- NULL
                }
            }
            # combine each element of individual_page_list into one text data set
            text_data <- do.call(rbind, individual_page_list)
            individual_ein_list[[row]] <- list(year=year, text=text_data) 
        }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}) 
    }
    # combine each element of individual_ein_list into one list of lists
    AHA_ein_list[[i]] <- list(text_data=individual_ein_list, ein=unique_eins[i])
    saveRDS(AHA_ein_list, paste0(created_data_path, "/AHA_ein_list.rds"))
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})    
}



