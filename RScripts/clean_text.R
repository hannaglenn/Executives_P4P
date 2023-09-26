library(readxl)
library(readr)
library(stringr)
library(tidyr)
library(dplyr)

# This script loops through the extracted hospital text and creates a data frame with the 
# desired information.

# read the code that creates clean_text()
source("paths.R")
source("clean_text_function.R")

# read in all_ein_list_2016_2020 
all_ein_list_2016_2020 <- readRDS(paste0(created_data_path,"/all_ein_list_2016_2020.rds"))

# create an empty data set to store cleaned text
all_ein_data_frame <- data.frame(first_name = character(), last_name=character(), title=character(), position=character())

#### create the other lists as inputs to clean_text() ####
lastnames <- read_excel(paste0(created_data_path,"raw data/app_c.xlsx")) %>%
  filter(count>7000) %>%
  select(name) %>%
  mutate(name=tolower(name))
firstnames <- read_csv(paste0(created_data_path,"raw data/yob2000.txt"), col_names = FALSE) %>%
  filter(X3>50) %>%
  select(X1) %>%
  rename(name=X1) %>%
  mutate(name=tolower(name))

lastnames_list <- as.list(lastnames)[["name"]]
lastnames_list <- lastnames_list[lastnames_list != 'person']
firstnames_list <- as.list(firstnames)[["name"]]

positions_list <- c('president', 'vice president', 'chair', 'vice chair', 'vice-chair', 'physician', 'trustee', 'director', 'board', 'secretary', 'treasurer', 'ceo', 'cfo', 'ceocfo',
                                      'chairman', 'pres', 'division chief', 'vp', 'vp,', 'pres\\.', 'chairperson', 'cpo', 'coo', 'cmo', 'cno', 'system executive',
                    'ohysician', 'shysician', 'ex-officio', 'executive', 'surgeon', 'anesthesiologist', 'dermatologist', 'medical director', 
                    'chief medical officer', 'chief executive officer', 'chief financial officer', 'chief medical officer')
titles_list <- c('md', 'm\\.d', 'm\\.d\\.', 'do', 'd\\.o', 'd\\.o\\.', "rn", "r\\.n", 'r\\.n\\.', 'np', 'n\\.p', 'n\\.p\\.')




#### loop over list elements and clean data ####
for (i in 1:length(all_ein_list_2016_2020)){
  
  ein <- all_ein_list_2016_2020[[i]][["ein"]]

  
  for (x in 1:length(all_ein_list_2016_2020[[i]][["ein_data"]])) {
    text <- all_ein_list_2016_2020[[i]][["ein_data"]][[x]][["text"]]
    year <- all_ein_list_2016_2020[[i]][["ein_data"]][[x]][["year"]]
    
    if (is.character(text)) {text_data=NULL}
    
    if (is.data.frame(text)) {
      text_data <- clean_text(text, firstnames_list, lastnames_list, positions_list, titles_list)
      text_data <- text_data %>%
        mutate(year=year, ein=ein)
    }
    
    all_ein_data_frame <- rbind(all_ein_data_frame, text_data)
    saveRDS(all_ein_data_frame, paste0(created_data_path, "all_ein_data_frame_20162020.rds"))
    saveRDS(i, paste0(created_data_path,"current_i.rds"))
  }
}

# Further clean text data
all_ein_data_frame <- all_ein_data_frame %>%
  filter(!is.na(first_name) & first_name!="-" & !is.na(last_name)) %>%
  mutate(first_name=str_remove_all(first_name, ","),
         last_name=str_remove_all(last_name, ","))
all_ein_data_frame <- all_ein_data_frame %>%
  filter(!is.na(position1))

