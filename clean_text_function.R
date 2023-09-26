library(readxl)
library(readr)
library(stringr)
library(tidyr)
library(dplyr)

# write a function to take a data frame of free form text and return a cleaned data frame of first name, last name, title and position. 

clean_text <- function(text_data, firstnames_list, lastnames_list, positions_list, titles_list) {
  #### clean up string in phrase variable ####
  # remove any digits
  text_data$phrase <- str_replace_all(text_data$phrase, "[:digit:]", "")
  
  # remove brackets
  text_data$phrase <- str_remove_all(text_data$phrase, "\\[")
  text_data$phrase <- str_remove_all(text_data$phrase, "\\]")
  
  # remove any letters that occur by themselves (so that names don't have a middle initial)
  text_data$phrase <- str_replace_all(text_data$phrase, "\\s\\w\\s", " ")
  text_data$phrase <- str_replace(text_data$phrase, "^\\w\\s", "")
  
  # remove "letter,"
  text_data$phrase <- str_remove(text_data$phrase, ",\\w\\s")
  text_data$phrase <- str_remove(text_data$phrase, "^\\w,\\s")
  
  # replace "/" with space
  text_data$phrase <- str_replace_all(text_data$phrase, "\\/", " ")
  
  # remove space that occurs at the beginning of the string
  text_data$phrase <- str_remove(text_data$phrase, "^\\s")
  
  # remove "po box"
  text_data$phrase <- str_remove(text_data$phrase, "po box|post")
    
  #### extract positions and titles and then remove them from phrase ####
  # extract the position
  text_data <- text_data %>%
    mutate(position = str_extract_all(phrase, paste0('\\b', paste(positions_list, collapse = '|'), '\\b'))) %>%
    unnest_wider(position, names_sep="") 
  
  # remove all text of positions 
  text_data$phrase <- str_remove_all(text_data$phrase, paste(positions_list, collapse="|"))
  
  if (!("position2" %in% names(text_data))) {
    text_data <- text_data %>%
      mutate(position2=NA)
  }
  
  text_data <- text_data %>%
    mutate(position1=ifelse(position1=="ohysician","physician",position1),
           position1=ifelse(position1=="shysician","physician",position1),
           position2=ifelse(position2=="shysician","physician",position2),
           position2=ifelse(position2=="ohysician","physician",position2))
  
  # extract the title if it is listed
  text_data <- text_data %>%
    mutate(title = str_extract(phrase, paste0('\\b', paste(titles_list, collapse = '\\b|\\b'), '\\b|md')))
  
  # remove all text of positions 
  text_data$phrase <- str_remove_all(text_data$phrase, "md")
  
  # extract any text that indicates it is a former officer
  text_data <- text_data %>%
    mutate(previous = str_extract(phrase, '\\bprevious\\b|\\bformer\\b|\\bend\\b|\\binterim\\b|\\bpast interim\\b')) 
  

  # next, split phrase up into individual words
  text_data <- separate(text_data, phrase, into=c('word1', 'word2', 'word3', 'word4', 'word5'), sep=" ", extra='merge', remove=FALSE)
  
  
  
  #### look for first names ####
  # create indicator for whether a first name is found
  text_data <- text_data %>%
    mutate(firstname_word1 = as.integer(rowSums(sapply(firstnames_list, function(name) str_detect(word1, paste0('\\b',name,'\\b')))) > 0),
           firstname_word2 = as.integer(rowSums(sapply(firstnames_list, function(name) str_detect(word2, paste0('\\b',name,'\\b')))) > 0),
           firstname_word3 = as.integer(rowSums(sapply(firstnames_list, function(name) str_detect(word3, paste0('\\b',name,'\\b')))) > 0),
           firstname_word4 = as.integer(rowSums(sapply(firstnames_list, function(name) str_detect(word4, paste0('\\b',name,'\\b')))) > 0))
  
  # figure out which column has the most frequent occurrence of first names 
  sum_fn_word1 <- sum(text_data$firstname_word1, na.rm=TRUE)
  sum_fn_word2 <- sum(text_data$firstname_word2, na.rm=TRUE)
  sum_fn_word3 <- sum(text_data$firstname_word3, na.rm=TRUE)
  sum_fn_word4 <- sum(text_data$firstname_word4, na.rm=TRUE)
  
  # save the name of the one that occurs most often
  int_names <- c("sum_fn_word1", "sum_fn_word2", "sum_fn_word3", "sum_fn_word4")
  largest_firstname <- int_names[which.max(c(sum_fn_word1, sum_fn_word2, sum_fn_word3, sum_fn_word4))]
  largest_firstname <- str_replace(largest_firstname, "sum_fn", "firstname")
  firstname_word <- str_remove(largest_firstname, "firstname_")

  # rename indicator variable
  text_data <- text_data %>%
    rename(max_firstname = largest_firstname)
    
  
  
  #### look for last names ####
  # create indicator for whether a last name is found
  text_data <- text_data %>%
    mutate(lastname_word1 = as.integer(rowSums(sapply(lastnames_list, function(name) str_detect(word1, paste0('\\b',name,'\\b')))) > 0),
           lastname_word2 = as.integer(rowSums(sapply(lastnames_list, function(name) str_detect(word2, paste0('\\b',name,'\\b')))) > 0),
           lastname_word3 = as.integer(rowSums(sapply(lastnames_list, function(name) str_detect(word3, paste0('\\b',name,'\\b')))) > 0),
           lastname_word4 = as.integer(rowSums(sapply(lastnames_list, function(name) str_detect(word4, paste0('\\b',name,'\\b')))) > 0))
  
  # figure out which column has the most frequent occurrence of last names 
  sum_ln_word1 <- sum(text_data$lastname_word1, na.rm=TRUE)
  sum_ln_word2 <- sum(text_data$lastname_word2, na.rm=TRUE)
  sum_ln_word3 <- sum(text_data$lastname_word3, na.rm=TRUE)
  sum_ln_word4 <- sum(text_data$lastname_word4, na.rm=TRUE)
  
  # save the name of the one that occurs most often
  int_names <- c("sum_ln_word1", "sum_ln_word2", "sum_ln_word3", "sum_ln_word4")
  int_names <- int_names[str_detect(int_names, firstname_word) == FALSE]
  if (largest_firstname=="firstname_word1") {largest_lastname <- c("sum_ln_word2", "sum_ln_word3", "sum_ln_word4")[which.max(c(sum_ln_word2, sum_ln_word3, sum_ln_word4))]}
  if (largest_firstname=="firstname_word2") {largest_lastname <- c("sum_ln_word1", "sum_ln_word3", "sum_ln_word4")[which.max(c(sum_ln_word1, sum_ln_word3, sum_ln_word4))]}
  if (largest_firstname=="firstname_word3") {largest_lastname <- c("sum_ln_word2", "sum_ln_word1", "sum_ln_word4")[which.max(c(sum_ln_word2, sum_ln_word1, sum_ln_word4))]}
  if (largest_firstname=="firstname_word4") {largest_lastname <- c("sum_ln_word2", "sum_ln_word3", "sum_ln_word1")[which.max(c(sum_ln_word2, sum_ln_word3, sum_ln_word1))]}
  largest_lastname <- str_replace(largest_lastname, "sum_ln", "lastname")
  
  # rename indicator variable
  text_data <- text_data %>%
    rename(max_lastname = largest_lastname)
  
  # rename variables to reflect names
  largest_firstname <- str_replace(largest_firstname, "firstname_", "")
  largest_lastname <- str_replace(largest_lastname, "lastname_", "")
  text_data <- text_data %>%
    rename(first_name=largest_firstname, last_name=largest_lastname)
  
  
  # create groups based on names found
  text_data <- text_data %>%
    mutate(name_found=as.integer(max_firstname==1 | max_lastname==1 | str_detect(phrase,"organization|part vii|efile")),
           group=NA) %>%
    mutate(name_found=ifelse(is.na(name_found),0,name_found))
  
  # form groups based on rows around names
  group = 0
  for (row in 1:nrow(text_data)) {
    if (text_data$name_found[row] == 1) {group=group+1}
    text_data$group[row] <- group
  }
  
  # fill title and previous by groups
  text_data <- text_data %>%
    group_by(group) %>%
    fill(title, .direction="up") %>%
    fill(previous, .direction="up") %>%
    fill(position1, .direction="up") %>%
    fill(position2, .direction="up") %>%
    ungroup()
  
  # create variable called "preamble" to get around picking up weird names such as "this box"
  text_data <- text_data %>%
    mutate(preamble=ifelse(str_detect(phrase, "organization|compensation|employees|schedule|section"),1,0))
  
  # filter to rows with either a last name or a first name
  text_data <- text_data %>%
    filter((max_firstname==1 | max_lastname==1) & preamble==0)
  
  # select variables
  text_data <- text_data %>%
    select(first_name, last_name, title, position1, position2, previous)
  
  
  
  #### return cleaned text data ####
  return(text_data=text_data)
}
