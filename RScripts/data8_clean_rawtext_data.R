library(readxl)
library(readr)
library(stringr)
library(tidyr)
library(dplyr)

# This script loops through the extracted hospital text and creates a data frame with the 
# desired information.

# read the code that creates clean_text()
source("paths.R")
source("function1_clean_text.R")


# read in AHA_ein_list
AHA_ein_list <- readRDS(paste0(created_data_path,"/AHA_ein_list.rds"))

# read in AHA_ein_list_errors (scraped text data from manual pdfs)
AHA_ein_list_errors1a <- readRDS(paste0(created_data_path,"/AHA_ein_list_errors1a.rds"))
AHA_ein_list_errors1b <- readRDS(paste0(created_data_path,"/AHA_ein_list_errors1b.rds"))

# read in data from manually matched AHA hospitals
AHA_ein_list_manualmatched <- readRDS(paste0(created_data_path, "/AHA_ein_list_manualmatched.rds"))

# create an empty data set to store cleaned text
AHA_ein_data_frame <- data.frame(first_name = character(), last_name=character(), title=character(), position=character())

#### create the other lists as inputs to clean_text() ####
lastnames <- read_excel(paste0(created_data_path, "raw data/app_c.xlsx")) %>%
  filter(count>7000) %>%
  select(name) %>%
  mutate(name=tolower(name))
firstnames <- read_csv(paste0(created_data_path, "raw data/yob2000.txt"), col_names = FALSE) %>% 
  filter(X3>50) %>%
  select(X1) %>%
  rename(name=X1) %>%
  mutate(name=tolower(name))

lastnames_list <- as.list(lastnames)[["name"]]
lastnames_list <- lastnames_list[lastnames_list != 'person']
firstnames_list <- as.list(firstnames)[["name"]]

positions_list <- c('vice president', 'peesident', 'president', 'vice chairman', 'chairman','vice chair', 'chairperson', 'chair', 'vice-chair', 'physician', 
                    'trustee', 'director', 'board', 'secretary', 'treasurer', 'ceocfo', 'ceo', 'cfo',
                    'pres\\b', 'division chief', 'vp', 'cpo', 'coo\\b', 'cmo', 'cno', 'cqo',
                    'system executive',
                    'ohysician', 'shysician','prysician', 'ex officio', 'executive', 'surgeon', 'anesthesiologist', 'dermatologist', 
                    'medical director', 
                    'chief medical officer', 'chief executive officer', 'chief financial officer', 'chief medical officer', 'chief quality officer')
titles_list <- c('md', 'do', 'rn', 'np')
former_list <- c('previous', 'former', 'ending', 'end', 'past interim', 'past', 'interim', 'prior') 


#### loop over list elements and clean data ####
for (i in 1:length(AHA_ein_list)){
  
  ein <- AHA_ein_list[[i]][["ein"]]

  for (x in 1:length(AHA_ein_list[[i]][["text_data"]])) {
    text <- AHA_ein_list[[i]][["text_data"]][[x]][["text"]]
    year <- AHA_ein_list[[i]][["text_data"]][[x]][["year"]]
    
    if (length(text)==0) {text_data=NULL}
    
    if (length(text)>0) {
      text_data <- clean_text(text, firstnames_list, lastnames_list, positions_list, titles_list)
      text_data <- text_data %>%
        mutate(year=year, ein=ein)
    }
    
    AHA_ein_data_frame <- rbind(AHA_ein_data_frame, text_data)
  }
}

for (i in 1:length(AHA_ein_list_errors1a)){
  
    ein <- AHA_ein_list_errors1a[[i]][[1]]
  
    text <- as.data.frame(AHA_ein_list_errors1a[[i]][["phrase"]]) %>%
      rename(phrase=`AHA_ein_list_errors1a[[i]][["phrase"]]`)
    year <- AHA_ein_list_errors1a[[i]][[2]]
    
    if (length(text)==0) {text_data=NULL}
    
    if (length(text)>0) {
      text_data <- clean_text(text, firstnames_list, lastnames_list, positions_list, titles_list)
      text_data <- text_data %>%
        mutate(year=year, ein=ein)
    }
    
    AHA_ein_data_frame <- rbind(AHA_ein_data_frame, text_data)
}

for (i in 1:length(AHA_ein_list_errors1b)){
  if (is.list(AHA_ein_list_errors1b[[i]])) {
  ein <- AHA_ein_list_errors1b[[i]][[1]]
  
  text <- as.data.frame(AHA_ein_list_errors1b[[i]][["phrase"]]) %>%
    rename(phrase=`AHA_ein_list_errors1b[[i]][["phrase"]]`)
  year <- AHA_ein_list_errors1b[[i]][[2]]
  
  if (length(text)==0) {text_data=NULL}
  
  if (length(text)>0) {
    text_data <- clean_text(text, firstnames_list, lastnames_list, positions_list, titles_list)
    text_data <- text_data %>%
      mutate(year=year, ein=ein)
  }
  
  AHA_ein_data_frame <- rbind(AHA_ein_data_frame, text_data)
  }
}

for (i in 1:length(AHA_ein_list_manualmatched)){
  if (is.list(AHA_ein_list_manualmatched[[i]])) {
  ein <- AHA_ein_list_manualmatched[[i]][[1]]
  
  text <- as.data.frame(AHA_ein_list_manualmatched[[i]][["phrase"]]) %>%
    rename(phrase=`AHA_ein_list_manualmatched[[i]][["phrase"]]`)
  year <- AHA_ein_list_manualmatched[[i]][[2]]
  
  if (length(text)==0) {text_data=NULL}
  
  if (length(text)>0) {
    text_data <- clean_text(text, firstnames_list, lastnames_list, positions_list, titles_list)
    text_data <- text_data %>%
      mutate(year=year, ein=ein)
  }
  
  AHA_ein_data_frame <- rbind(AHA_ein_data_frame, text_data)
  }
}

# Further clean text data
AHA_ein_data_frame <- AHA_ein_data_frame %>%
  distinct() %>%
  filter(first_name!="" & last_name!="") %>%
  mutate(extra = str_remove(extra, "ae eaeeeeeeeaneeees")) %>%
  filter(first_name!="otherassets")

saveRDS(AHA_ein_data_frame, paste0(created_data_path, "cleaned_text.rds"))


