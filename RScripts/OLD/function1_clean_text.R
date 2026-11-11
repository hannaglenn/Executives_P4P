clean_text <- function(text_data, firstnames_list, lastnames_list, positions_list, titles_list) {
  #### clean up string in phrase variable ####
  # filter out compensation portion of tax form
  text_data <- text_data %>%
    filter(row_number()<min(which(str_detect(text_data$phrase, "breakdown of w-2"))))
  
  # remove any digits
  text_data$phrase <- str_replace_all(text_data$phrase, "[:digit:]", "")
  
  # remove brackets
  text_data$phrase <- str_remove_all(text_data$phrase, '\\[|\\]|\\)|\\(|\\||\\{|\\}|~|=|\\s-\\s|_|—|>|<|\\.|\\*|,|°|:|\\"|\\”|“|‘|!|\\?|’|\\&|«|\\$|\\+')
  
  # remove any letters that occur by themselves (so that names don't have a middle initial)
  text_data$phrase <- str_replace_all(text_data$phrase, "\\b\\w\\b", " ")
  text_data$phrase <- str_replace_all(text_data$phrase, "^\\w\\s", "")
  
  # replace "/" with space
  text_data$phrase <- str_replace_all(text_data$phrase, "\\/|-|\\\\", " ")
  
  # remove "po box"
  text_data$phrase <- str_remove(text_data$phrase, "po box|post")
  
  # remove random two/three letter words
  text_data$phrase <- str_remove_all(text_data$phrase, "\\bxe\\b|\\brr\\b|\\ber\\b|\\bma\\b|\\brpe\\b|\\bjs\\b|\\bny\\b|\\brt\\b|\\bly\\b|\\brev\\b|\\bwk\\b|\\brd\\b|\\bod\\b|\\bfx\\b|\\biii\\b|\\bfe\\b|\\bxy\\b|\\bea\\b|\\boe\\b|\\bsf\\b|\\btt\\b|\\basa\\b|\\btx\\b|\\bio\\b|\\bpt\\b|\\bxt\\b|\\bsso\\b|\\bsss\\b|\\bxx\\b|\\boo\\b|\\bco\\b|\\biq\\b|\\beo\\b|\\bes\\b|\\bee\\b|\\blr\\b|\\bss\\b|\\bng\\b|\\bec\\b|\\bll\\b|\\bfl\\b|\\bil\\b")

  # remove a lot of space with one space
  text_data$phrase <- str_replace_all(text_data$phrase, "\\s+", " ")
  
  # create indicator for "dr" and remove it from phrase
  text_data <- text_data %>%
    mutate(dr=ifelse(str_detect(phrase, "\\bdr\\b"),1,0))
  text_data$phrase <- str_remove_all(text_data$phrase, "\\bdr\\b")
  
  # remove space that occurs at the beginning of the string
  text_data$phrase <- str_remove(text_data$phrase, "^\\s+")
  
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
  
  
  #### Positions, Titles, and Previous Key Words ####
  # Extract positions from phrase
  text_data <- text_data %>%
    mutate(position = str_extract_all(phrase, paste0('\\b', paste(positions_list, collapse = '|'), '\\b'))) %>%
    mutate(position=ifelse(position=="character(0)", NA, position)) %>%
    unnest_wider(position, names_sep="") 
  
  # only keep observations with either a name or a position
  text_data <- text_data %>%
    mutate(name_found=as.integer((max_firstname==1 | max_lastname==1) & !str_detect(phrase, "employee key|week box|zwayne bennett|second street|bennett drive|academy street|weeks hill|form page|organization|compensation|key employees|section|last day|for bonds|east south|main street|mary st|water blue|freeport south|radiology southern|schedule le|yes pan|individual li|al mobile|emergency new|pine street|street chestnut|employee key|york new|brooklyn ny|th east|troy ny|tot lo|syracuse east|anesthesiologist jefferson|main south|marshall st|america cep|market street|reese ft|state street|porconsustning|rd box|osceola street|dayton oh|er powers|new york|river valley|nd east|oh cleveland|bellefontaine oh|columbus oh|beavercreek oh|tiffin oh|qo box|oh toledo|forest gate|ste law|law firm|fl orlando|st louis|lincoln st|management morrison|pa dallas|me scarborough|cove road|england new|nh keene|burlington south|kirby north|milky way|schedule|plains white|waterhouse price|issuer bonds|stewart nd|billing|boston providence|engineer|construction")),
           group=NA) %>%
    mutate(name_found=ifelse(is.na(name_found),0,name_found)) %>%
    mutate(position_found=ifelse(is.na(position1),0,1)) %>%
    mutate(position_found=ifelse(str_detect(phrase, "form page|organization|compensation|key employees|section"),0,position_found)) %>%
    filter(name_found==1 | position_found==1)
  
  # add up the number of names and the number of positions found
  num_names <- sum(text_data$name_found, na.rm=T)
  num_positions <- sum(text_data$position_found, na.rm=T)
  
  # if there are a lot more positions than names, the text reader may have had trouble getting the names.
  # get rid of all the data when this happens because I can't trust the positions assignment to the names 
  if (num_positions>=1.5*num_names){
    text_data <- text_data %>%
      filter(year==0)
  }
  
  if (nrow(text_data)>0){
  # extract titles
  text_data <- text_data %>%
    mutate(title = str_extract(phrase, paste0('\\b', paste(titles_list, collapse = '\\b|\\b'), '\\b'))) %>%
    mutate(title=ifelse(dr==1,"dr",title))
  
  # extract words that indicate "former"
  text_data <- text_data %>%
    mutate(former = str_extract(phrase, paste0('\\b', paste(former_list, collapse = '\\b|\\b'), '\\b')))
  
  # form groups based on rows around names
  group = 0
  for (row in 1:nrow(text_data)) {
    if (text_data$name_found[row] == 1) {group=group+1}
    text_data$group[row] <- group
  }
  
  # fill positions based on group
  text_data <- text_data %>%
    group_by(group) %>%
    fill(position1, position2, title, former, .direction="downup") %>%
    ungroup()
  
  # mutate NAs to blank
  text_data <- text_data %>%
    mutate(first_name=ifelse(is.na(first_name),"", first_name),
           last_name=ifelse(is.na(last_name), "", last_name))

  for (j in 1:nrow(text_data)){
    if (text_data$name_found[[j]]==1 & text_data$first_name[[j]]!="") {
      text_data$phrase[[j]] <- str_remove(text_data$phrase[[j]],text_data$first_name[[j]])
    }
    if (text_data$name_found[[j]]==1 & text_data$last_name[[j]]!="") {
      text_data$phrase[[j]] <- str_remove(text_data$phrase[[j]],text_data$last_name[[j]])
    }
    
    if (text_data$position_found[[j]]==1){
      text_data$phrase[[j]] <- str_remove(text_data$phrase[[j]],text_data$position1[[j]])
      if (!is.na(text_data$position2[[j]])){
        text_data$phrase[[j]] <- str_remove(text_data$phrase[[j]],text_data$position2[[j]])
      }
    }
    
    if (length(text_data$phrase[[j]])>0 & !is.na(text_data$title[[j]])){
      text_data$phrase[[j]] <- str_remove(text_data$phrase[[j]],text_data$title[[j]])
    }
    if (length(text_data$phrase[[j]])>0){
      text_data$phrase[[j]] <- str_remove_all(text_data$phrase[[j]],paste0('\\b', paste(former_list, collapse = '|'), '\\b|\\bex\\b|\\bbeg\\b'))
    }
  }
  
  # remove anyone that doesn't have a position
  text_data <- text_data %>%
    filter(!is.na(position1))
  
  # rename mistyped physicians
  text_data <- text_data %>%
    mutate(position1=ifelse(position1=="ohysician","physician",position1),
           position1=ifelse(position1=="shysician","physician",position1),
           position1=ifelse(position1=='prysician','physician',position1),
           position2=ifelse(position2=="shysician","physician",position2),
           position2=ifelse(position2=="ohysician","physician",position2),
           position2=ifelse(position2=='prysician','physician',position2))
  

  #### Form final data ####
  # create "extra" variable that hopefully captures any extra information about the person
  text_data$phrase <- str_remove_all(text_data$phrase, "\\bfor\\b|related|from|the|beginning|sure")
  text_data$phrase <- str_replace(text_data$phrase, "^\\s+", "")
  text_data <- text_data %>%
    mutate(phrase=ifelse(phrase=="",NA,phrase)) %>%
    mutate(extra = phrase) %>%
    group_by(group) %>%
    fill(extra, .direction="downup") %>%
    ungroup() %>%
    filter(name_found==1) %>%
    select(first_name, last_name, position1, position2, title, former, extra) %>%
    distinct()
  }
  
  return(text_data)
  }




