library(readxl)
library(readr)
library(stringr)
library(tidyr)
library(dplyr)
library(tibble)

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
cleaned_text <- cleaned_text %>%
  distinct() %>%
  filter(first_name!="" & last_name!="") %>%
  mutate(extra = str_remove(extra, "ae eaeeeeeeeaneeees")) %>%
  filter(!first_name %in% c("otherassets", "ct", "vp", "ac", "eco", "al", "eee", "po")) %>%
  mutate(name = paste0(first_name, " ", last_name)) %>%
  mutate(year = as.double(year)) %>%
  mutate(year = year-1)

# fill in missing information where OCR failed 
cleaned_text <- cleaned_text %>%
  add_row(year = 2011, ein = "10198331", name = "katheryn resinbrink", position1 = "president", extra = "medical staff", title = "md") %>%
  add_row(year = 2011, ein = "10198331", name = "charles therrien", position1 = "ceo") %>%
  add_row(year = 2011, ein = "10198331", name = "kevin sedgwick", position1 = "cfo") %>%
  add_row(year = 2010, ein = "10198331", name = "john mccormick", position1 = "ceo", former = "interim") %>%
  add_row(year = 2010, ein = "10211783", name = "bernard mcadam", position1 = "cfo") %>%
  add_row(year = 2010, ein = "10211783", name = "michael lally", position1 = "ceo") %>%
  add_row(year = 2011, ein = "10211783", name = "michael lally", position1 = "ceo") %>%
  add_row(year = 2011, ein = "10211783", name = "nancy glidden", position1 = "cfo") %>%
  add_row(year = 2010, ein = "10211503", name = "rebecca ryder", position1 = "president", position2 = "ceo") %>%
  add_row(year = 2010, ein = "10211503", name = "david hyde", position1 = "president", extra = "of med staff", title = "md") %>%
  add_row(year = 2010, ein = "10211503", name = "gerald cayer", position1 = "vp", position2 = "executive") %>%
  add_row(year = 2010, ein = "10211503", name = "eric martinsen", position1 = "cfo") %>%
  add_row(year = 2011, ein = "10211503", name = "rebecca ryder", position1 = "president", position2 = "ceo") %>%
  add_row(year = 2011, ein = "10211503", name = "gerald cayer", position1 = "vp", position2 = "executive") %>%
  add_row(year = 2011, ein = "10211503", name = "eric martinsen", position1 = "cfo") %>%
  add_row(year = 2013, ein = "10211783", name = "michael lally", position1 = "ceo") %>%
  add_row(year = 2013, ein = "10211783", name = "nancy glidden", position1 = "cfo") %>%
  add_row(year = 2010, ein = "10215911", name = "james mullen", position1 = "vp", extra = "of medical staff", title = "md") %>%
  add_row(year = 2010, ein = "10215911", name = "paul laprad", position1 = "president", extra = "of medical staff", title = "md") %>%
  add_row(year = 2010, ein = "10215911", name = "robert mccue", position1 = "cfo") %>%
  add_row(year = 2010, ein = "10215911", name = "herbert paris", position1 = "ceo", title = "md") %>%
  add_row(year = 2011, ein = "10215911", name = "james mullen", position1 = "president", extra = "of medical staff", title = "md") %>%
  add_row(year = 2011, ein = "10215911", name = "james rines", position1 = "vp", extra = "of medical staff", title = "md") %>%
  add_row(year = 2011, ein = "10215911", name = "lois skillings", position1 = "ceo") %>%
  add_row(year = 2011, ein = "10215911", name = "robert mccue", position1 = "cfo") %>%
  add_row(year = 2011, ein = "10215911", name = "herbert pans", position1 = "ceo") %>%
  add_row(year = 2013, ein = "10215911", name = "james mullen", position1 = "president", extra = "of medical staff", title = "md") %>%
  add_row(year = 2013, ein = "10215911", name = "james rines", position1 = "vp", extra = "of medical staff", title = "md") %>%
  add_row(year = 2013, ein = "10215911", name = "lois skillings", position1 = "ceo") %>%
  add_row(year = 2013, ein = "10215911", name = "robert mccue", position1 = "cfo") %>%
  add_row(year = 2014, ein = "10215911", name = "patrick keaney", position1 = "vp", extra = "of medical staff", title = "md") %>%
  add_row(year = 2014, ein = "10215911", name = "james rines", position1 = "president", extra = "of medical staff", title = "md") %>%
  add_row(year = 2014, ein = "10215911", name = "robert mccue", position1 = "cfo") %>%
  add_row(year = 2014, ein = "10215911", name = "lois skillings", position1 = "ceo") %>%
  add_row(year = 2010, ein = "10223482", name = "christine mclaughlin", position1 = "cfo") %>%
  add_row(year = 2010, ein = "10223482", name = "marie vienneau", position1 = "ceo") %>%
  add_row(year = 2010, ein = "10234189", name = "martin bernstein", position1 = "ceo") %>%
  add_row(year = 2010, ein = "10234189", name = "roger lagasse", position1 = "cfo") %>%
  add_row(year = 2011, ein = "10234189", name = "martin bernstein", position1 = "ceo") %>%
  add_row(year = 2011, ein = "10234189", name = "roger lagasse", position1 = "cfo") %>%
  add_row(year = 2011, ein = "10234189", name = "peter sirois", position1 = "ceo") %>%
  add_row(year = 2011, ein = "10263198", name = "douglas jones", position1 = "ceo") %>%
  add_row(year = 2011, ein = "10263198", name = "lynette parr", position1 = "cfo") %>%
  add_row(year = 2012, ein = "10263198", name = "douglas jones", position1 = "ceo") %>%
  add_row(year = 2012, ein = "10263198", name = "lynette parr", position1 = "cfo") %>%
  add_row(year = 2013, ein = "10263198", name = "dougles jones", position1 = "ceo") %>%
  add_row(year = 2013, ein = "10263198", name = "lynette parr", position1 = "cfo") %>%
  add_row(year = 2010, ein = "10263628", name = "victoria alexander", position1 = "president", position2 = "ceo") %>%
  add_row(year = 2014, ein = "10372148", name = "john beaulieu", position1 = "vp", extra = "med s", title = "do") %>%
  add_row(year = 2014, ein = "10372148", name = "michael faloon", position1 = "president", extra = "of medical staff", title = "md") %>%
  add_row(year = 2014, ein = "10372148", name = "sylva getman", position1 = "ceo", position2 = "president") %>%
  add_row(year = 2014, ein = "10372148", name = "jay reynolds", position1 = "cmo", title = "md") %>%
  add_row(year = 2014, ein = "10372148", name = "bruce sandstrom", position1 = "cfo") %>%
  add_row(year = 2014, ein = "10372148", name = "roger pelli", position1 = "do") %>%
  add_row(year = 2014, ein = "10372148", name = "david peterson", position1 = "ceo") %>%
  add_row(year = 2010, ein = "10646166", name = "steven west", position1 = "president") %>%
  add_row(year = 2011, ein = "10646166", name = "steven west", position1 = "president") %>%
  add_row(year = 2012, ein = "10646166", name = "steven west", position1 = "president") %>%
  add_row(year = 2010, ein = "20222118", name = "randolph knight", position1 = "president", extra = "medical staff", title = "md") %>%
  add_row(year = 2010, ein = "20222118", name = "steven monette", position1 = "cfo") %>%
  add_row(year = 2011, ein = "20222118", name = "carol boerner", position1 = "president", extra = "medical staff", title = "md") %>%
  add_row(year = 2011, ein = "20222118", name = "steven monette", position1 = "cfo") %>%
  add_row(year = 2012, ein = "20222118", name = "claire bowen", position1 = "president", position2 = "ceo") %>%
  add_row(year = 2012, ein = "20222118", name = "steven monette", position1 = "vp", extra = "finance") %>%
  add_row(year = 2014, ein = "20222118", name = "peter wright", position1 = "president", position2 = "ceo") %>%
  add_row(year = 2014, ein = "20222118", name = "steven monette", position1 = "vp", extra = "finance") %>%
  add_row(year = 2014, ein = "20222157", name = "cynthia mcguire", position1 = "ceo") %>%
  add_row(year = 2014, ein = "20222157", name = "peter gosline", position1 = "ceo") %>%
  add_row(year = 2014, ein = "20222157", name = "richard scheinblum", position1 = "cfo") %>%
  add_row(year = 2010, ein = "20222171", name = "bruce king", position1 = "president", position2 = "ceo") %>%
  add_row(year = 2010, ein = "20222171", name = "james mcguire", position1 = "president", extra = "medical staff", title = "md") %>%
  add_row(year = 2010, ein = "20222171", name = "eileen kirk", position1 = "president", extra = "medical staff", title = "md") %>%
  add_row(year = 2010, ein = "20222171", name = "tina naimie", position1 = "cfo") %>%
  add_row(year = 2010, ein = "20222171", name = "gregory curtis", position1 = "cmo", title = "md") %>%
  add_row(year = 2011, ein = "20222171", name = "bruce king", position1 = "president", position2 = "ceo") %>%
  add_row(year = 2011, ein = "20222171", name = "eileen kirk", position1 = "president", extra = "medical staff", title = "md") %>%
  add_row(year = 2011, ein = "20222171", name = "donald griffin", position1 = "cfo") %>%
  add_row(year = 2011, ein = "20222171", name = "gregory curtis", position1 = "cmo", title = "md") %>%
  add_row(year = 2011, ein = "20222171", name = "teresa leblanc", position1 = "coo") %>%
  add_row(year = 2011, ein = "20222171", name = "tina naimie", position1 = "cfo") %>%
  add_row(year = 2010, ein = "20223332", name = "david tower", position1 = "president", extra="of hosp") %>%
  add_row(year = 2010, ein = "20223332", name = "ernest erick", position1 = "cfo") %>%
  add_row(year = 2011, ein = "20223332", name = "david tower", position1 = "president", extra = "of hosp") %>%
  add_row(year = 2010, ein = "20223332", name = "ernest erick", position1 = "cfo") %>%
  add_row(year = 2014, ein = "20509911", name = "douglas dean", position1 = "president", position2 = "ceo") %>%
  add_row(year = 2014, ein = "20509911", name = "richard elwell", position1 = "cfo") %>%
  add_row(year = 2010, ein = "30179423", name = "joel silverstein", position1 = "president", extra = "of med staff", title = "md") %>%
  add_row(year = 2010, ein = "30179423", name = "melvin patashnick", position1 = "president", position2 = "ceo") %>%
  add_row(year = 2010, ein = "30179423", name = "rassoul rangaviz", position1 = "cfo") %>%
  add_row(year = 2014, ein = "30179423", name = "joseph subasic", position1 = "president", extra = "of med staff", title = "md") %>%
  add_row(year = 2014, ein = "30179423", name = "melvyn patashnick", position1 = "president", position2 = "ceo") %>%
  add_row(year = 2014, ein = "30179423", name = "rassoul rangaviz", position1 = "cfo") %>%
  add_row(year = 2010, ein = "30179437", name = "glenn cordner", position1 = "president", position2 = "ceo") %>%
  add_row(year = 2010, ein = "30179437", name = "andrew majka", position1 = "cfo") %>%
  add_row(year = 2010, ein = "30183721", name = "kevin donovan", position1 = "ceo") %>%
  add_row(year = 2010, ein = "30183721", name = "jeanne mcneal", position1 = "cfo") %>%
  add_row(year = 2010, ein = "30183721", name = "bennett beres", position1 = "coo") %>%
  add_row(year = 2014, ein = "30183721", name = "catherine schneider", position1 = "president", extra = "of med staff", title = "md") %>%
  add_row(year = 2014, ein = "30183721", name = "kevin donovan", position1 = "ceo") %>%
  add_row(year = 2014, ein = "30183721", name = "bennett beres", position1 = "coo") %>%
  add_row(year = 2010, ein = "30219309", name = "melinda estes", position1 = "president", position2 = "ceo", title = "md") %>%
  add_row(year = 2010, ein = "30219309", name = "roger deshaies", position1 = "cfo") %>%
  add_row(year = 2010, ein = "30219309", name = "paul taheri", position1 = "president", position2 = "ceo", title = "md") %>%
  add_row(year = 2011, ein = "30219309", name = "melinda estes", position1 = "president", position2 = "ceo", title = "md") %>%
  add_row(year = 2011, ein = "30219309", name = "john brumstead", position1 = "president", position2 = "ceo", title = "md") %>%
  add_row(year = 2011, ein = "30219309", name = "roger deshaies", position1 = "cfo") %>%
  add_row(year = 2011, ein = "30219309", name = "paul taheri", position1 = "president", position2 = "ceo") %>%
  add_row(year = 2010, ein = "30266986", name = "jill berry", position1 = "ceo") %>%
  add_row(year = 2010, ein = "30266986", name = "ted sirotta", position1 = "cfo") %>%
  add_row(year = 2010, ein = "42103565", name = "christine schuster", position1 = "ceo") %>%
  add_row(year = 2010, ein = "42103565", name = "john wilhelm", position1 = "vp", extra = "finance") %>%
  add_row(year = 2010, ein = "42103565", name = "gregory mertin", position1 = "vp", extra = "clinical affairs", title = "md") %>%
  add_row(year = 2010, ein = "42103602", name = "jeanne lynskey", position1 = "cfo") %>%
  add_row(year = 2010, ein = "42103602", name = "salvatore perla", position1 = "vp", extra = "clinical support") %>%
  add_row(year = 2010, ein = "42103602", name = "cheryl bonasoro", position1 = "vp", extra = "patient care") %>%
  add_row(year = 2010, ein = "42103602", name = "linda yates", position1 = "vp", extra = "human resources") %>%
  add_row(year = 2010, ein = "42769210", name = "clifford breslow", position1 = "president", extra = "medical staff", title = "md") %>%
  add_row(year = 2010, ein = "42769210", name = "michael cullen", position1 = "cfo") %>%
  add_row(year = 2010, ein = "42769210", name = "kathleen hefferman", position1 = "vice president", extra = "chief risk officer") %>%
  add_row(year = 2010, ein = "42769210", name = "richard aubut", position1 = "president", position2 = "ceo") %>%
  add_row(year = 2010, ein = "42769210", name = "rose dipietro", position1 = "vice president", extra = "clinical") %>%
  add_row(year = 2010, ein = "42769210", name = "delroy dixon", position1 = "vice president", extra = "chief information officer") %>%
  add_row(year = 2011, ein = "42769210", name = "clifford breslow", position1 = "president", extra = "medical staff", title = "md") %>%
  add_row(year = 2011, ein = "42769210", name = "richard aubut", position1 = "president", position2 = "ceo") %>%
  add_row(year = 2011, ein = "42769210", name = "michael cullen", position1 = "cfo") %>%
  add_row(year = 2011, ein = "42769210", name = "joseph cahill", position1 = "coo") %>%
  add_row(year = 2011, ein = "42769210", name = "kathleen hefferman", position1 = "vice president", extra = "chief risk officer") %>%
  add_row(year = 2011, ein = "42769210", name = "delroy dixon", position1 = "vice president", extra = "chief information officer") %>%
  add_row(year = 2010, ein = "43341666", name = "darin peck", position1 = "cmo", title = "md") %>%
  add_row(year = 2010, ein = "43341666", name = "eugene murray", position1 = "president", position2 = "ceo") %>%
  add_row(year = 2010, ein = "43341666", name = "edward olivier", position1 = "cfo") %>%
  add_row(year = 2010, ein = "43341666", name = "dennis welsh", position1 = "vp", extra = "of operation") %>%
  add_row(year = 2011, ein = "43341666", name = "darin peck", position1 = "cmo", title = "md") %>%
  add_row(year = 2011, ein = "43341666", name = "eugene murray", position1 = "president", position2 = "ceo") %>%
  add_row(year = 2011, ein = "43341666", name = "edward olivier", position1 = "cfo") %>%
  add_row(year = 2011, ein = "43341666", name = "dennis welsh", position1 = "vp", extra = "of operation") %>%
  add_row(year = 2010, ein = "43369649", name = "scott bullock", position1 = "president", position2 = "ceo") %>%
  add_row(year = 2010, ein = "43369649", name = "michael koziol", position1 = "cfo") %>%
  add_row(year = 2014, ein = "43369649", name = "michael koziol", position1 = "cfo") %>%
  add_row(year = 2014, ein = "43369649", name = "barbara crowley", position1 = "vice president", extra = "executive", title = "md") %>%
  add_row(year = 2014, ein = "43369649", name = "paul stein", position1 = "coo") %>%
  add_row(year = 2010, ein = "50258954", name = "timothy babineau", position1 = "president", position2 = "ceo", title = "md") %>%
  add_row(year = 2010, ein = "50258954", name = "mary wakefield", position1 = "cfo") %>%
  add_row(year = 2010, ein = "50258954", name = "joseph amaral", position1 = "president", position2 = "ceo", title = "md") %>%
  add_row(year = 2010, ein = "50258954", name = "john murphy", position1 = "vp", extra = "medical affairs", title = "md") %>%
  add_row(year = 2010, ein = "50258954", name = "georgie vecchione", position1 = "president", position2 = "ceo") %>%
  add_row(year = 2010, ein = "60646597", name = "john murphy", position1 = "president", position2 = "ceo", title = "md") %>%
  add_row(year = 2010, ein = "60646597", name = "frank kelly", position1 = "president", position2 = "ceo") %>%
  add_row(year = 2010, ein = "60646597", name = "matthew miller", position1 = "cmo", title = "md") %>%
  add_row(year = 2010, ein = "60646597", name = "william roe", position1 = "cfo") %>%
  add_row(year = 2010, ein = "60646715", name = "lucille janatka", position1 = "president", position2 = "ceo") %>%
  add_row(year = 2010, ein = "60646715", name = "kenneth kurtz", position1 = "cmo", title = "md") %>%
  add_row(year = 2010, ein = "60646715", name = "ralph becker", position1 = "cfo") %>%
  add_row(year = 2010, ein = "60646715", name = "kenneth cesca", position1 = "vp", position2 = "hr") %>%
  add_row(year = 2010, ein = "60646715", name = "harold kaplan", position1 = "vp", extra = "med affairs") %>%
  add_row(year = 2010, ein = "60646741", name = "joseph pelaccia", position1 = "president", position2 = "ceo") %>%
  add_row(year = 2010, ein = "60646741", name = "laura smith", position1 = "cfo") %>%
  add_row(year = 2010, ein = "60646741", name = "ted weisman", position1 = "president", extra = "medical staff", title = "md") %>%
  add_row(year = 2011, ein = "60646741", name = "joseph pelaccia", position1 = "president", position2 = "ceo") %>%
  add_row(year = 2011, ein = "60646741", name = "laura smith", position1 = "cfo") %>%
  add_row(year = 2011, ein = "60646741", name = "lloyd friedman", position1 = "coo") %>%
  add_row(year = 2010, ein = "60646917", name = "brian grissler", position1 = "president", position2 = "ceo") %>%
  add_row(year = 2010, ein = "60646917", name = "kevin gage", position1 = "cfo") %>%
  add_row(year = 2010, ein = "60646917", name = "kathleen silard", position1 = "coo") %>%
  add_row(year = 2010, ein = "60646917", name = "john rodis", position1 = "vp", extra = "medical services", title = "md") %>%
  add_row(year = 2010, ein = "61745397", name = "christine wolfeske", position1 = "ceo") %>%
  add_row(year = 2010, ein = "61745397", name = "laura cormier", position1 = "vp", position2 = "of operations") %>%
  add_row(year = 2011, ein = "61745397", name = "christine wolfeske", position1 = "ceo") %>%
  add_row(year = 2011, ein = "61745397", name = "laura cormier", position1 = "vp", extra = "of operations") %>%
  add_row(year = 2012, ein = "61745397", name = "christine wolfeske", position1 = "ceo") %>%
  add_row(year = 2012, ein = "61745397", name = "laura cormier", position1 = "vp", extra = "of operations") %>%
  add_row(year = 2010, ein = "111631759", name = "robert dubicki", position1 = "coo") %>%
  add_row(year = 2010, ein = "111631759", name = "linda brady", position1 = "president", position2 = "ceo", title = "md") %>%
  add_row(year = 2010, ein = "111631759", name = "kevin molloy", position1 = "coo") %>%
  add_row(year = 2010, ein = "111631759", name = "sibte burney", position1 = "cmo", title = "md") %>%
  add_row(year = 2014, ein = "111631781", name = "bruce flanz", position1 = "president", position2 = "ceo") %>%
  add_row(year = 2014, ein = "111631781", name = "mounir doss", position1 = "cfo") %>%
  add_row(year = 2014, ein = "111631781", name = "robert levine", position1 = "coo") %>%
  add_row(year = 2010, ein = "111631837", name = "ravij gaarg", position1 = "ceo") %>%
  add_row(year = 2010, ein = "111631837", name = "leon kozlowski", position1 = "cfo") %>%
  add_row(year = 2010, ein = "111631837", name = "fran heaney", position1 = "coo") %>%
  add_row(year = 2011, ein = "111631837", name = "ravij garg", position1 = "ceo") %>%
  add_row(year = 2011, ein = "111631837", name = "leon kozlowski", position1 = "cfo") %>%
  add_row(year = 2011, ein = "111631837", name = "frances heaney", position1 = "coo") %>%
  add_row(year = 2013, ein = "111633487", name = "michael dowling", position1 = "president", position2 = "ceo") %>%
  add_row(year = 2013, ein = "111633487", name = "kevin lawlor", position1 = "president", position2 = "ceo") %>%
  add_row(year = 2013, ein = "111633487", name = "mark solazzo", position1 = "coo") %>%
  add_row(year = 2013, ein = "111633487", name = "robert shapiro", position1 = "cfo") %>%
  add_row(year = 2014, ein = "111633487", name = "michael dowling", position1 = "president", position2 = "ceo") %>%
  add_row(year = 2014, ein = "111633487", name = "kevin lawlor", position1 = "president", position2 = "ceo") %>%
  add_row(year = 2014, ein = "111633487", name = "mark solazzo", position1 = "coo") %>%
  add_row(year = 2014, ein = "111633487", name = "robert shapiro", position1 = "cfo") %>%
  add_row(year = 2013, ein = "112241326", name = "michael dowling", position1 = "president", position2 = "ceo") %>%
  add_row(year = 2013, ein = "112241326", name = "kevin lawlor", position1 = "president", position2 = "ceo") %>%
  add_row(year = 2013, ein = "112241326", name = "mark solazzo", position1 = "coo") %>%
  add_row(year = 2013, ein = "112241326", name = "robert shapiro", position1 = "cfo") %>%
  add_row(year = 2014, ein = "112241326", name = "michael dowling", position1 = "president", position2 = "ceo") %>%
  add_row(year = 2014, ein = "112241326", name = "kevin lawlor", position1 = "president", position2 = "ceo") %>%
  add_row(year = 2014, ein = "112241326", name = "mark solazzo", position1 = "coo") %>%
  add_row(year = 2014, ein = "112241326", name = "robert shapiro", position1 = "cfo") %>%
  add_row(year = 2013, ein = "113241243", name = "michael dowling", position1 = "president", position2 = "ceo") %>%
  add_row(year = 2013, ein = "113241243", name = "kevin lawlor", position1 = "president", position2 = "ceo") %>%
  add_row(year = 2013, ein = "113241243", name = "mark solazzo", position1 = "coo") %>%
  add_row(year = 2013, ein = "113241243", name = "robert shapiro", position1 = "cfo") %>%
  add_row(year = 2014, ein = "113241243", name = "michael dowling", position1 = "president", position2 = "ceo") %>%
  add_row(year = 2014, ein = "113241243", name = "kevin lawlor", position1 = "president", position2 = "ceo") %>%
  add_row(year = 2014, ein = "113241243", name = "mark solazzo", position1 = "coo") %>%
  add_row(year = 2014, ein = "113241243", name = "robert shapiro", position1 = "cfo") %>%
  add_row(year = 2013, ein = "113438973", name = "alan guerci", position1 = "president", position2 = "ceo", title = "md") %>%
  add_row(year = 2013, ein = "113438973", name = "joel yohai", position1 = "cmo", title = "md") %>%
  add_row(year = 2013, ein = "113438973", name = "drew pallas", position1 = "coo") %>%
  add_row(year = 2013, ein = "113438973", name = "william armstrong", position1 = "cfo") %>%
  add_row(year = 2013, ein = "113438973", name = "howard sussman", position1 = "cmo") %>%
  add_row(year = 2013, ein = "113438973", name = "ihab ibrahim", position1 = "vp", extra = "patient safety") %>%
  add_row(year = 2013, ein = "113438973", name = "john morahan", position1 = "vp", extra = "finance") %>%
  add_row(year = 2013, ein = "113438973", name = "peter chiacchiaro", position1 = "vp", position2 = "finance") %>%
  add_row(year = 2014, ein = "113438973", name = "alan guerci", position1 = "president", position2 = "ceo", title = "md") %>%
  add_row(year = 2014, ein = "113438973", name = "drew pallas", position1 = "coo") %>%
  add_row(year = 2014, ein = "113438973", name = "william armstrong", position1 = "vp", extra = "finance") %>%
  add_row(year = 2014, ein = "113438973", name = "howard sussman", position1 = "cmo") %>%
  add_row(year = 2014, ein = "113438973", name = "john morahan", position1 = "vp", extra = "finance") %>%
  add_row(year = 2014, ein = "113438973", name = "barbara gibbens", position1 = "vp", extra = "patient care") %>%
  add_row(year = 2010, ein = "131740118", name = "joel seligman", position1 = "president", position2 = "ceo") %>%
  add_row(year = 2010, ein = "131740118", name = "marla koroly", position1 = "vp", extra = "medical affairs") %>%
  add_row(year = 2010, ein = "131740118", name = "john partenza", position1 = "cfo") %>%
  add_row(year = 2010, ein = "131740118", name = "kerry flynn", position1 = "vp", extra = "human resources") %>%
  add_row(year = 2011, ein = "131740118", name = "joel seligman", position1 = "president", position2 = "ceo") %>%
  add_row(year = 2011, ein = "131740118", name = "lauraine szekely", position1 = "vp", extra = "medical affairs") %>%
  add_row(year = 2011, ein = "131740118", name = "john partenza", position1 = "cfo") %>%
  add_row(year = 2011, ein = "131740118", name = "kerry flynn", position1 = "vp", extra = "human resources") %>%
  add_row(year = 2010, ein = "131740120", name = "mark webster", position1 = "vp", extra = "finance") %>%
  add_row(year = 2010, ein = "131740120", name = "robert bernasek", position1 = "vp", extra = "med affairs", title = "md") %>%
  add_row(year = 2010, ein = "131740120", name = "jeane costella", position1 = "vp", extra = "hr") %>%
  add_row(year = 2011, ein = "131740120", name = "mark webster", position1 = "vp", extra = "finance") %>%
  add_row(year = 2011, ein = "131740120", name = "jeane costella", position1 = "vp", extra = "hr") %>%
  add_row(year = 2011, ein = "131740120", name = "robert bernasek", position1 = "vp", extra = "med affairs", title = "md") %>%
  add_row(year = 2014, ein = "131740120", name = "mark webster", position1 = "vp", extra = "finance") %>%
  add_row(year = 2014, ein = "131740120", name = "william higgins", position1 = "vp", extra = "medical a") %>%
  add_row(year = 2014, ein = "131740120", name = "jeane costella", position1 = "vp", extra = "hr") %>%
  add_row(year = 2010, ein = "131974191", name = "miguel fuentas", position1 = "president", position2 = "ceo") %>%
  add_row(year = 2011, ein = "131974191", name = "miguel fuentas", position1 = "president", position2 = "ceo") %>%
  add_row(year = 2010, ein = "141338373", name = "jane ehrlich", position1 = "pres", position2 = "ceo") %>%
  add_row(year = 2010, ein = "141338373", name = "jay cahalan", position1 = "coo") %>%
  add_row(year = 2010, ein = "141338373", name = "vincent dingman", position1 = "cfo") %>%
  add_row(year = 2010, ein = "141338413", name = "david kruczlnicki", position1 = "president", position2 = "ceo") %>%
  add_row(year = 2010, ein = "141338413", name = "michael niles", position1 = "cfo") %>%
  add_row(year = 2010, ein = "141338413", name = "dianne shugrue", position1 = "coo") %>%
  add_row(year = 2010, ein = "141338413", name = "donna kirker", position1 = "vp", extra = "patient services") %>%
  add_row(year = 2010, ein = "141338413", name = "jeff treasure", position1 = "cfo") %>%
  add_row(year = 2014, ein = "141338471", name = "joyce rafferty", position1 = "cfo") %>%
  add_row(year = 2014, ein = "141338471", name = "debra donahue", position1 = "coo") %>%
  add_row(year = 2014, ein = "141338471", name = "kent hall", position1 = "cmo") %>%
  add_row(year = 2010, ein = "141347719", name = "victor giulianelli", position1 = "president", position2 = "ceo") %>%
  add_row(year = 2010, ein = "141347719", name = "john sagan", position1 = "cfo") %>%
  add_row(year = 2010, ein = "141347719", name = "scott bruce", position1 = "vp", extra = "of operations") %>%
  add_row(year = 2010, ein = "141347719", name = "timothy shoen", position1 = "vp", extra = "medical affairs") %>%
  add_row(year = 2011, ein = "141347719", name = "victor guilianelli", position1 = "president", position2 = "ceo") %>%
  add_row(year = 2011, ein = "141347719", name = "john sagan", position1 = "cfo") %>%
  add_row(year = 2011, ein = "141347719", name = "scott bruce", position1 = "vp", extra = "of operations") %>%
  add_row(year = 2011, ein = "141347719", name = "timothy shoen", position1 = "vp", extra = "medical affairs") %>%
  add_row(year = 2014, ein = "141364513", name = "rodney boula", position1 = "ceo") %>%
  add_row(year = 2014, ein = "141364513", name = "matthew nolan", position1 = "coo") %>%
  add_row(year = 2014, ein = "141364513", name = "alan chardavoyne", position1 = "cfo") %>%
  add_row(year = 2014, ein = "141364513", name = "rob demuro", position1 = "cmo", title = "md") %>%
  add_row(year = 2014, ein = "141731786", name = "chandler ralph", position1 = "president", position2 = "ceo") %>%
  add_row(year = 2014, ein = "141731786", name = "charles glanville", position1 = "cfo") %>%
  add_row(year = 2014, ein = "141731786", name = "john broderick", position1 = "cmo", title = "md") %>%
  add_row(year = 2010, ein = "150532054", name = "scott berlucchi", position1 = "president", position2 = "ceo") %>%
  add_row(year = 2012, ein = "150533577", name = "thomas carman", position1 = "president", position2 = "ceo") %>%
  add_row(year = 2012, ein = "150533577", name = "paul kraeger", position1 = "cfo") %>%
  add_row(year = 2012, ein = "150533577", name = "mario victoria", position1 = "vp", extra = "medical affairs", title = "md") %>%
  add_row(year = 2013, ein = "150533577", name = "thomas carman", position1 = "president", position2 = "ceo") %>%
  add_row(year = 2013, ein = "150533577", name = "sean mills", position1 = "cfo") %>%
  add_row(year = 2013, ein = "150533577", name = "mario victoria", position1 = "cmo") %>%
  add_row(year = 2013, ein = "150533577", name = "brian ohearn", position1 = "vp", extra = "patient care") %>%
  add_row(year = 2014, ein = "150533577", name = "paul kraeger", position1 = "vp", extra = "finance") %>%
  add_row(year = 2014, ein = "150533577", name = "thomas carman", position1 = "president", position2 = "ceo") %>%
  add_row(year = 2014, ein = "150533577", name = "sean mills", position1 = "cfo") %>%
  add_row(year = 2014, ein = "150533577", name = "mario victoria", position1 = "cmo") %>%
  add_row(year = 2014, ein = "150533577", name = "brian ohearn", position1 = "vp", extra = "patient care") %>%
  add_row(year = 2010, ein = "150622079", name = "walter becker", position1 = "ceo") %>%
  add_row(year = 2010, ein = "150622079", name = "mark hills", position1 = "cfo") %>%
  add_row(year = 2010, ein = "160393490", name = "francis macafee", position1 = "cfo") %>%
  add_row(year = 2010, ein = "160393490", name = "shirley magana", position1 = "coo") %>%
  add_row(year = 2014, ein = "160743037", name = "steven goldstein", position1 = "ceo") %>%
  add_row(year = 2014, ein = "160743037", name = "cindy becker", position1 = "coo") %>%
  add_row(year = 2014, ein = "160743037", name = "adam anolik", position1 = "cfo") %>%
  add_row(year = 2014, ein = "160743037", name = "raymond mayewski", position1 = "cmo", title = "md") %>%
  add_row(year = 2010, ein = "160743163", name = "robert mcnamara", position1 = "cfo") %>%
  add_row(year = 2010, ein = "160743163", name = "kim panosian", position1 = "vp", extra = "med affairs", title = "md") %>%
  add_row(year = 2010, ein = "160743921", name = "nils gunnersen", position1 = "ceo") %>%
  add_row(year = 2011, ein = "160743921", name = "nils gunnersen", position1 = "ceo") %>%
  add_row(year = 2012, ein = "160743921", name = "nils gunnersen", position1 = "ceo") %>%
  add_row(year = 2014, ein = "160960470", name = "kimberly boynton", position1 = "ceo") %>%
  add_row(year = 2014, ein = "160960470", name = "derrick suehs", position1 = "cqo") %>%
  add_row(year = 2014, ein = "160960470", name = "kelli harris", position1 = "cfo") %>%
  add_row(year = 2014, ein = "160960470", name = "ronald stahl", position1 = "cmo", title = "md") %>%
  add_row(year = 2010, ein = "161012691", name = "david patterson", position1 = "president", position2 = "ceo") %>%
  add_row(year = 2011, ein = "161012691", name = "david acker", position1 = "president", position2 = "ceo") %>%
  add_row(year = 2012, ein = "161012691", name = "david acker", position1 = "president", position2 = "ceo") %>%
  add_row(year = 2013, ein = "161012691", name = "david acker", position1 = "president", position2 = "ceo") %>%
  add_row(year = 2011, ein = "200479568", name = "daniel rohrbach", position1 = "ceo") %>%
  add_row(year = 2011, ein = "200479568", name = "jeff peterson", position1 = "cmo", title = "md") %>%
  add_row(year = 2012, ein = "200479568", name = "daniel rohrbach", position1 = "ceo") %>%
  add_row(year = 2010, ein = "208316475", name = "andrew cochrane", position1 = "president") %>%
  add_row(year = 2010, ein = "210634562", name = "richard miller", position1 = "ceo") %>%
  add_row(year = 2010, ein = "210634562", name = "robert segin", position1 = "cfo") %>%
  add_row(year = 2010, ein = "210634562", name = "ninfa saunders", position1 = "coo") %>%
  add_row(year = 2010, ein = "210634562", name = "james dwyer", position1 = "cmo", title = "do") %>%
  add_row(year = 2010, ein = "210634562", name = "stephen kolesk", position1 = "coo", title = "md") %>%
  add_row(year = 2013, ein = "210662542", name = "mark gill", position1 = "cfo") %>%
  add_row(year = 2013, ein = "210662542", name = "richard falivena", position1 = "vp", extra = "medical services", title = "do") %>%
  add_row(year = 2013, ein = "210662542", name = "byron hunter", position1 = "vp", extra = "hr") %>%
  add_row(year = 2014, ein = "210662542", name = "mark gill", position1 = "cfo") %>%
  add_row(year = 2014, ein = "210662542", name = "david tarantino", position1 = "cmo", title = "do") %>%
  add_row(year = 2014, ein = "210662542", name = "deborah baehser", position1 = "vp", extra = "patient services") %>%
  add_row(year = 2014, ein = "210662542", name = "byron hunter", position1 = "vp", extra = "hr") %>%
  add_row(year = 2010, ein = "222458317", name = "elliot sussman", position1 = "ceo", title = "md") %>%
  add_row(year = 2011, ein = "222458317", name = "ronald swinfard", position1 = "ceo", title = "md") %>%
  add_row(year = 2011, ein = "222458317", name = "terry capuano", position1 = "coo") %>%
  add_row(year = 2011, ein = "222458317", name = "thomas whalen", position1 = "cmo", title = "md") %>%
  add_row(year = 2012, ein = "222458317", name = "ronald swinfard", position1 = "ceo", title = "md") %>%
  add_row(year = 2012, ein = "222458317", name = "thomas whalen", position1 = "cmo", title = "md") %>%
  add_row(year = 2012, ein = "222458317", name = "terry capuano", position1 = "coo") %>%
  add_row(year = 2014, ein = "222458317", name = "ronald swinfard", position1 = "ceo") %>%
  add_row(year = 2014, ein = "222458317", name = "thomas whalen", position1 = "cmo", title = "md") %>%
  add_row(year = 2014, ein = "222458317", name = "terry capuano", position1 = "coo") %>%
  add_row(year = 2010, ein = "222517863", name = "william gillepsie", position1 = "vp", extra = "chief technology officer") %>%
  add_row(year = 2010, ein = "222517863", name = "robert batory", position1 = "vp", extra = "hr") %>%
  add_row(year = 2010, ein = "222517863", name = "richard baker", position1 = "vp", extra = "chief information officer") %>%
  add_row(year = 2010, ein = "222517863", name = "michael oconner", position1 = "cfo") %>%
  add_row(year = 2010, ein = "222520073", name = "vijay gandevia", position1 = "president", extra = "medical staff", title = "md") %>%
  add_row(year = 2010, ein = "222520073", name = "paul silva", position1 = "vp", extra = "finance") %>%
  add_row(year = 2011, ein = "222520073", name = "vijay gandevia", position1 = "president", extra = "medical staff", title = "md") %>%
  add_row(year = 2011, ein = "222520073", name = "paul silva", position1 = "vp", extra = "finance") %>%
  add_row(year = 2013, ein = "222520073", name = "vijay gandevia", position1 = "president", extra = "medical staff", title = "md") %>%
  add_row(year = 2013, ein = "222520073", name = "paul silva", position1 = "vp", extra = "finance") %>%
  add_row(year = 2010, ein = "222594672", name = "green michael", position1 = "president", position2 = "ceo") %>%
  add_row(year = 2010, ein = "222594672", name = "imgrund stephen", position1 = "president", extra = "medical staff", title = "md") %>%
  add_row(year = 2010, ein = "222594672", name = "conley joseph", position1 = "coo") %>%
  add_row(year = 2011, ein = "222594672", name = "green michael", position1 = "president", position2 = "ceo") %>%
  add_row(year = 2011, ein = "222594672", name = "imgrund stephen", position1 = "cmo", title = "md") %>%
  add_row(year = 2011, ein = "222594672", name = "conley joseph", position1 = "coo") %>%
  add_row(year = 2010, ein = "222674014", name = "kevin callahan", position1 = "ceo") %>%
  add_row(year = 2010, ein = "223380375", name = "joseph trunfio", position1 = "president", position2 = "ceo") %>%
  add_row(year = 2011, ein = "223380375", name = "joseph trunfio", position1 = "president", position2 = "ceo") %>%
  add_row(year = 2011, ein = "223380375", name = "kevin lenahan", position1 = "cfo") %>%
  add_row(year = 2010, ein = "223524939", name = "richard miller", position1 = "ceo") %>%
  add_row(year = 2010, ein = "223524939", name = "ninfa saunders", position1 = "coo") %>%
  add_row(year = 2010, ein = "223524939", name = "robert segin", position1 = "cfo") %>%
  add_row(year = 2010, ein = "223524939", name = "james dwyer", position1 = "cmo", title = "md") %>%
  add_row(year = 2010, ein = "223524939", name = "michael kotzen", position1 = "coo") %>%
  add_row(year = 2010, ein = "223524939", name = "stephen kolesk", position1 = "coo", title = "md") %>%
  add_row(year = 2014, ein = "230469150", name = "michael duncan", position1 = "president", position2 = "ceo") %>%
  add_row(year = 2014, ein = "230469150", name = "kenneth flickinger", position1 = "cfo") %>%
  add_row(year = 2014, ein = "230469150", name = "michael barber", position1 = "coo") %>%
  add_row(year = 2010, ein = "230596940", name = "bogucki alfred", position1 = "president", extra = "medical staff", title = "md") %>%
  add_row(year = 2010, ein = "230596940", name = "crossin robert", position1 = "cfo") %>%
  add_row(year = 2010, ein = "230596940", name = "wilson linde", position1 = "coo") %>%
  add_row(year = 2010, ein = "230596940", name = "bucci domenic", position1 = "cmo", title = "md") %>%
  add_row(year = 2010, ein = "230596940", name = "degnan william", position1 = "vp", extra = "finance") %>%
  add_row(year = 2010, ein = "231352181", name = "stuart fine", position1 = "ceo", position2 = "president") %>%
  add_row(year = 2010, ein = "231352181", name = "jean keeler", position1 = "vp", extra = "hr") %>%
  add_row(year = 2010, ein = "231352181", name = "gregory wuerstle", position1 = "cfo") %>%
  add_row(year = 2011, ein = "231352181", name = "stuart fine", position1 = "ceo") %>%
  add_row(year = 2011, ein = "231352181", name = "gregory wuerstle", position1 = "cfo") %>%
  add_row(year = 2010, ein = "231352203", name = "thomas lichtenwalner", position1 = "vp", extra = "medical affairs") %>%
  add_row(year = 2010, ein = "231352203", name = "lida young", position1 = "vp", extra = "patient services") %>%
  add_row(year = 2011, ein = "231352203", name = "thomas lichtenwalner", position1 = "cfo") %>%
  add_row(year = 2011, ein = "231352203", name = "thomas filipowicz", position1 = "vp", extra = "medical affairs", title = "md") %>%
  add_row(year = 2010, ein = "231370484", name = "john porter", position1 = "president", position2 = "ceo") %>%
  add_row(year = 2010, ein = "231370484", name = "vincent glielmi", position1 = "vp", extra = "medical affairs", title = "do") %>%
  add_row(year = 2010, ein = "231370484", name = "robert graupensberger", position1 = "coo") %>%
  add_row(year = 2010, ein = "231370484", name = "john holmes", position1 = "cfo") %>%
  add_row(year = 2011, ein = "231401561", name = "jason hawkins", position1 = "ceo") %>%
  add_row(year = 2011, ein = "231401561", name = "deborah shughart", position1 = "cfo") %>%
  add_row(year = 2010, ein = "231401561", name = "jason hawkins", position1 = "ceo") %>%
  add_row(year = 2010, ein = "231401561", name = "deborah shughart", position1 = "cfo") %>%
  add_row(year = 2010, ein = "231996150", name = "steven pierdon", position1 = "cmo", title = "md") %>%
  add_row(year = 2010, ein = "232201344", name = "daniel cochran", position1 = "vp", extra = "finance") %>%
  add_row(year = 2011, ein = "232201344", name = "clint matthews", position1 = "president", position2 = "ceo") %>%
  add_row(year = 2011, ein = "232201344", name = "richard jones", position1 = "cfo") %>%
  add_row(year = 2011, ein = "232201344", name = "daniel cochran", position1 = "vp", extra = "finance") %>%
  add_row(year = 2012, ein = "232201344", name = "therese sucher", position1 = "coo") %>%
  add_row(year = 2012, ein = "232201344", name = "richard jones", position1 = "cfo") %>%
  add_row(year = 2014, ein = "232201344", name = "clint matthews", position1 = "president", position2 = "ceo") %>%
  add_row(year = 2014, ein = "232201344", name = "therese sucher", position1 = "coo") %>%
  add_row(year = 2014, ein = "232201344", name = "richard jones", position1 = "cfo") %>%
  add_row(year = 2010, ein = "237134386", name = "thomas moalker", position1 = "ceo") %>%
  add_row(year = 2010, ein = "237134386", name = "pedro hernandez", position1 = "president", extra = "medical staff", title = "md") %>%
  add_row(year = 2010, ein = "237134386", name = "cindy daigle", position1 = "cfo") %>%
  add_row(year = 2011, ein = "237134386", name = "thomas moalker", position1 = "ceo") %>%
  add_row(year = 2011, ein = "237134386", name = "cindy daigle", position1 = "cfo") %>%
  add_row(year = 2010, ein = "237293874", name = "charles pearce", position1 = "cfo") %>%
  add_row(year = 2010, ein = "237293874", name = "frances laukaitis", position1 = "coo") %>%
  add_row(year = 2014, ein = "237429117", name = "john frye", position1 = "ceo") %>%
  add_row(year = 2014, ein = "237429117", name = "mary farrell", position1 = "vp", extra = "patient care") %>%
  add_row(year = 2014, ein = "237429117", name = "mark foote", position1 = "vp", extra = "finance") %>%
  add_row(year = 2010, ein = "240795682", name = "richard wisniewski", position1 = "cfo") %>%
  add_row(year = 2010, ein = "240795682", name = "david peterson", position1 = "coo") %>%
  add_row(year = 2010, ein = "240795682", name = "janet schachtner", position1 = "vp", extra = "patient services") %>%
  add_row(year = 2010, ein = "240795959", name = "albert bothejr", position1 = "cmo", title = "md") %>%
  add_row(year = 2011, ein = "240795959", name = "kenneth wood", position1 = "cmo", title = "do") %>%
  add_row(year = 2011, ein = "240795959", name = "albert bothe", position1 = "cmo", title = "md") %>%
  add_row(year = 2012, ein = "240795959", name = "kenneth wood", position1 = "cmo", title = "do") %>%
  add_row(year = 2010, ein = "240798681", name = "pravinchandra patel", position1 = "president", extra = "med staff", title = "md") %>%
  add_row(year = 2010, ein = "250965598", name = "john papalia", position1 = "ceo") %>%
  add_row(year = 2010, ein = "250965598", name = "murray marsh", position1 = "cfo") %>%
  add_row(year = 2010, ein = "250965598", name = "randy california", position1 = "coo") %>%
  add_row(year = 2011, ein = "250965598", name = "charles mackenzie", position1 = "president", extra = "medical staff", title = "md") %>%
  add_row(year = 2011, ein = "250965598", name = "john papalia", position1 = "ceo") %>%
  add_row(year = 2011, ein = "250965598", name = "murray marsh", position1 = "cfo") %>%
  add_row(year = 2011, ein = "250965598", name = "randy california", position1 = "coo") %>%
  add_row(year = 2010, ein = "250965600", name = "gary weinstein", position1 = "president", position2 = "ceo") %>%
  add_row(year = 2010, ein = "250965600", name = "michael roney", position1 = "cfo") %>%
  add_row(year = 2010, ein = "250984595", name = "john sutika", position1 = "ceo") %>%
  add_row(year = 2011, ein = "250984595", name = "john sutika", position1 = "ceo") %>%
  add_row(year = 2011, ein = "250984595", name = "julianne peer", position1 = "vp", extra = "finance") %>%
  add_row(year = 2011, ein = "261861676", name = "sandra patrick", position1 = "cfo") %>%
  add_row(year = 2011, ein = "261861676", name = "freda russell", position1 = "ceo") %>%
  add_row(year = 2013, ein = "261861676", name = "sandra patrick", position1 = "cfo") %>%
  add_row(year = 2013, ein = "261861676", name = "freda russell", position1 = "ceo") %>%
  add_row(year = 2010, ein = "261938641", name = "devan johnson", position1 = "ceo") %>%
  add_row(year = 2010, ein = "261938641", name = "rod larsen", position1 = "cfo") %>%
  add_row(year = 2013, ein = "310645626", name = "mina ubbing", position1 = "president", position2 = "ceo") %>%
  add_row(year = 2013, ein = "310645626", name = "sky gettys", position1 = "cfo") %>%
  add_row(year = 2013, ein = "310645626", name = "steven cox", position1 = "cmo", title = "md") %>%
  add_row(year = 2013, ein = "310645626", name = "howard sniderman", position1 = "coo") %>%
  add_row(year = 2014, ein = "310645626", name = "mina ubbing", position1 = "president", position2 = "ceo") %>%
  add_row(year = 2014, ein = "310645626", name = "sky gettys", position1 = "cfo") %>%
  add_row(year = 2014, ein = "310645626", name = "john janoso", position1 = "president", position2 = "ceo") %>%
  add_row(year = 2014, ein = "310645626", name = "steven cox", position1 = "cmo", title = "md") %>%
  add_row(year = 2014, ein = "310645626", name = "howard sniderman", position1 = "coo") %>%
  add_row(year = 2014, ein = "310645626", name = "martha buckley", position1 = "cqo", title = "md") %>%
  add_row(year = 2011, ein = "311156690", name = "brent saunders", position1 = "president", position2 = "ceo") %>%
  add_row(year = 2011, ein = "311156690", name = "kenneth payne", position1 = "cfo") %>%
  add_row(year = 2010, ein = "311524546", name = "michael king", position1 = "ceo") %>%
  add_row(year = 2010, ein = "311524546", name = "allen butcher", position1 = "cfo") %>%
  add_row(year = 2010, ein = "311524546", name = "tom heller", position1 = "vp", extra = "hr") %>%
  add_row(year = 2010, ein = "311524546", name = "cindy neely", position1 = "vp", extra = "patient care") %>%
  add_row(year = 2010, ein = "314413259", name = "stephen smith", position1 = "ceo") %>%
  add_row(year = 2011, ein = "314413259", name = "stephen smith", position1 = "ceo") %>%
  add_row(year = 2010, ein = "330420041", name = "kay lang", position1 = "ceo") %>%
  add_row(year = 2010, ein = "330420041", name = "david recupero", position1 = "cfo") %>%
  add_row(year = 2010, ein = "330420041", name = "mark turner", position1 = "ceo") %>%
  add_row(year = 2012, ein = "330420041", name = "mark turner", position1 = "ceo") %>%
  add_row(year = 2012, ein = "330420041", name = "david recupero", position1 = "cfo") %>%
  add_row(year = 2014, ein = "340714535", name = "danny boggs", position1 = "president", position2 = "ceo") %>%
  add_row(year = 2014, ein = "340714535", name = "matthew bernhard", position1 = "vp", extra = "med staff") %>%
  add_row(year = 2014, ein = "340714535", name = "michael martin", position1 = "president", extra = "med staff", title = "md") %>%
  add_row(year = 2014, ein = "340714535", name = "mary griest", position1 = "cfo") %>%
  add_row(year = 2010, ein = "340827442", name = "barry franklin", position1 = "cfo") %>%
  add_row(year = 2010, ein = "340827442", name = "terrence deis", position1 = "ceo") %>%
  add_row(year = 2013, ein = "340827442", name = "terrence deis", position1 = "ceo") %>%
  add_row(year = 2013, ein = "340827442", name = "david cook", position1 = "cfo") %>%
  add_row(year = 2013, ein = "340827442", name = "dale cowan", position1 = "cmo") %>%
  add_row(year = 2010, ein = "341048666", name = "philip lennan", position1 = "ceo") %>%
  add_row(year = 2010, ein = "341048666", name = "nicholas walz", position1 = "cmo") %>%
  add_row(year = 2010, ein = "341048666", name = "janice david", position1 = "vp", extra = "patient care") %>%
  add_row(year = 2010, ein = "341048666", name = "chad tinkel", position1 = "cfo") %>%
  add_row(year = 2014, ein = "341407259", name = "ronald carmin", position1 = "cfo") %>%
  add_row(year = 2010, ein = "341517671", name = "robert reider", position1 = "cmo", title = "md") %>%
  add_row(year = 2010, ein = "341517671", name = "robert fredrick", position1 = "cmo") %>%
  add_row(year = 2010, ein = "341517671", name = "neeraj kanwal", position1 = "vp", extra = "med affairs") %>%
  add_row(year = 2010, ein = "341517671", name = "lori johnston", position1 = "vp", extra = "finance") %>%
  add_row(year = 2010, ein = "341571750", name = "terry carson", position1 = "ceo") %>%
  add_row(year = 2011, ein = "341571750", name = "terry carson", position1 = "ceo") %>%
  add_row(year = 2013, ein = "344430849", name = "david brewer", position1 = "cfo") %>%
  add_row(year = 2014, ein = "344430849", name = "gary ackenberger", position1 = "vp", extra = "finance") %>%
  add_row(year = 2014, ein = "344446484", name = "gary ackenberger", position1 = "vp", extra = "finance") %>%
  add_row(year = 2010, ein = "344451872", name = "jerry morasko", position1 = "ceo") %>%
  add_row(year = 2010, ein = "344451872", name = "lamar wise", position1 = "ceo") %>%
  add_row(year = 2010, ein = "344451872", name = "robert melargno", position1 = "cfo") %>%
  add_row(year = 2010, ein = "344451872", name = "eric draime", position1 = "cfo") %>%
  add_row(year = 2010, ein = "350868132", name = "philip newbold", position1 = "ceo", position2 = "president") %>%
  add_row(year = 2010, ein = "350868132", name = "jeffrey costello", position1 = "cfo") %>%
  add_row(year = 2010, ein = "350868132", name = "kreg gruber", position1 = "coo") %>%
  add_row(year = 2010, ein = "350868132", name = "cheryl wibbens", position1 = "vp", extra = "med staff", title = "md") %>%
  add_row(year = 2011, ein = "350868132", name = "philip newbold", position1 = "ceo", position2 = "president") %>%
  add_row(year = 2011, ein = "350868132", name = "jeffrey costello", position1 = "cfo") %>%
  add_row(year = 2011, ein = "350868132", name = "kreg gruber", position1 = "coo") %>%
  add_row(year = 2010, ein = "352228583", name = "mike robertson", position1 = "president", position2 = "ceo") %>%
  add_row(year = 2010, ein = "352228583", name = "sheryl kink", position1 = "cfo") %>%
  add_row(year = 2010, ein = "352362438", name = "arnold parial", position1 = "president", extra = "medical staff", title = "md") %>%
  add_row(year = 2011, ein = "352362438", name = "keyur patel", position1 = "president", extra = "medical staff", title = "md") %>%
  add_row(year = 2011, ein = "352363050", name = "roddey gettys", position1 = "ceo") %>%
  add_row(year = 2011, ein = "352363050", name = "larry pope", position1 = "cfo") %>%
  add_row(year = 2011, ein = "361703630", name = "earnon irons", position1 = "ceo") %>%
  add_row(year = 2011, ein = "361703630", name = "anthony puorro", position1 = "president", position2 = "ceo") %>%
  add_row(year = 2011, ein = "361703630", name = "ron krol", position1 = "cfo") %>%
  add_row(year = 2012, ein = "361703630", name = "dian powell", position1 = "president", position2 = "ceo") %>%
  add_row(year = 2012, ein = "361703630", name = "earmon irons", position1 = "ceo") %>%
  add_row(year = 2013, ein = "361703630", name = "dian powell", position1 = "president", position2 = "ceo") %>%
  add_row(year = 2014, ein = "362169179", name = "terrence moisan", position1 = "ceo", title = "md") %>%
  add_row(year = 2014, ein = "362169179", name = "hugh rose", position1 = "cfo") %>%
  add_row(year = 2014, ein = "362169179", name = "mary denisienko", position1 = "vp", extra = "hr") %>%
  add_row(year = 2014, ein = "362169179", name = "gary zmrhal", position1 = "cfo") %>%
  add_row(year = 2010, ein = "362170866", name = "andrew stefo", position1 = "cfo") %>%
  add_row(year = 2010, ein = "362170866", name = "vincent pryor", position1 = "cfo") %>%
  add_row(year = 2012, ein = "362200248", name = "john valles", position1 = "cfo") %>%
  add_row(year = 2010, ein = "363616314", name = "mark rogers", position1 = "cfo") %>%
  add_row(year = 2010, ein = "363616314", name = "frank claudy", position1 = "vp", extra = "med staff", title = "md") %>%
  add_row(year = 2010, ein = "363616314", name = "charles bruhn", position1 = "president", extra = "gmc illini") %>%
  add_row(year = 2010, ein = "363616314", name = "julie manas", position1 = "president", extra = "gmc davenport") %>%
  add_row(year = 2010, ein = "370624255", name = "susie campbell", position1 = "ceo") %>%
  add_row(year = 2010, ein = "370624255", name = "donald brunworth", position1 = "cfo") %>%
  add_row(year = 2011, ein = "370624255", name = "susie campbell", position1 = "ceo") %>%
  add_row(year = 2011, ein = "370624255", name = "donald brunnworth", position1 = "cfo") %>%
  add_row(year = 2010, ein = "370661202", name = "kathy bunting", position1 = "ceo") %>%
  add_row(year = 2010, ein = "370661202", name = "mike brown", position1 = "cfo") %>%
  add_row(year = 2010, ein = "370673512", name = "steven leurck", position1 = "president", position2 = "ceo") %>%
  add_row(year = 2010, ein = "370673512", name = "thomas parish", position1 = "cfo") %>%
  add_row(year = 2010, ein = "370673512", name = "wesley oswald", position1 = "ceo", former = "interim") %>%
  add_row(year = 2010, ein = "370673512", name = "michael barger", position1 = "cfo", former = "interim") %>%
  add_row(year = 2010, ein = "370702309", name = "roby williams", position1 = "ceo") %>%
  add_row(year = 2010, ein = "370702309", name = "janie parker", position1 = "cfo") %>%
  add_row(year = 2012, ein = "370702309", name = "roby williams", position1 = "ceo") %>%
  add_row(year = 2012, ein = "370702309", name = "janie parker", position1 = "cfo") %>%
  add_row(year = 2012, ein = "370813229", name = "gerald mcshane", position1 = "pres", position2 = "ceo", extra = "osfmg", title = "md") %>%
  add_row(year = 2012, ein = "370813229", name = "kevin schoeplein", position1 = "ceo") %>%
  add_row(year = 2013, ein = "370813229", name = "gerald mcshane", position1 = "pres", position2 = "ceo", extra = "osfmg", title = "md") %>%
  add_row(year = 2013, ein = "370813229", name = "kevin schoeplein", position1 = "ceo") %>%
  add_row(year = 2012, ein = "371058692", name = "mary starmann", position1 = "president", position2 = "ceo") %>%
  add_row(year = 2012, ein = "371058692", name = "lawrence schumaker", position1 = "coo") %>%
  add_row(year = 2012, ein = "371058692", name = "michael cottrell", position1 = "cfo") %>%
  add_row(year = 2012, ein = "371058692", name = "robert ritz", position1 = "pres", position2 = "ceo", extra = "springfield ref div") %>%
  add_row(year = 2012, ein = "371058692", name = "stephen ronstrom", position1 = "pres", position2 = "ceo", extra = "western wisconsin div") %>%
  add_row(year = 2012, ein = "371058692", name = "therese pandl", position1 = "pres", position2 = "ceo", extra = "eastern wisconsin div") %>%
  add_row(year = 2012, ein = "371058692", name = "richard rolston", position1 = "pres", position2 = "ceo", extra = "hshs medical group") %>%
  add_row(year = 2013, ein = "371058692", name = "mary starmann", position1 = "president", position2 = "ceo") %>%
  add_row(year = 2013, ein = "371058692", name = "lawrence schumaker", position1 = "coo") %>%
  add_row(year = 2013, ein = "371058692", name = "michael cottrell", position1 = "cfo") %>%
  add_row(year = 2013, ein = "371058692", name = "robert ritz", position1 = "pres", position2 = "ceo", extra = "springfield ref div") %>%
  add_row(year = 2013, ein = "371058692", name = "kevin kast", position1 = "pres", position2 = "ceo", extra = "central il div") %>%
  add_row(year = 2013, ein = "371058692", name = "sonia mehta", position1 = "pres", position2 = "ceo", extra = "med grp") %>%
  add_row(year = 2014, ein = "371058692", name = "mary starmann", position1 = "president", position2 = "ceo") %>%
  add_row(year = 2014, ein = "371058692", name = "lawrence schumaker", position1 = "coo") %>%
  add_row(year = 2014, ein = "371058692", name = "michael cottrell", position1 = "cfo") %>%
  add_row(year = 2014, ein = "371058692", name = "julie manas", position1 = "pres", position2 = "ceo", extra = "division") %>%
  add_row(year = 2014, ein = "371058692", name = "mark reifsteck", position1 = "pres", position2 = "ceo", extra = "division") %>%
  add_row(year = 2014, ein = "371058692", name = "melinda clark", position1 = "pres", position2 = "ceo", extra = "med group") %>%
  add_row(year = 2014, ein = "371058692", name = "therese pandl", position1 = "pres", position2 = "ceo", extra = "eastern wisconsin div") %>%
  add_row(year = 2010, ein = "376062326", name = "roland carlson", position1 = "president", position2 = "ceo") %>%
  add_row(year = 2010, ein = "376062326", name = "trina casner", position1 = "cfo", position2 = "coo") %>%
  add_row(year = 2010, ein = "381218516", name = "mark ohalla", position1 = "president", position2 = "ceo") %>%
  add_row(year = 2010, ein = "381218516", name = "michael smith", position1 = "cmo", title = "do") %>%
  add_row(year = 2010, ein = "381218516", name = "donna kopinski", position1 = "cfo") %>%
  add_row(year = 2011, ein = "381415390", name = "david west", position1 = "coo") %>%
  add_row(year = 2011, ein = "381415390", name = "edward gamache", position1 = "ceo") %>%
  add_row(year = 2011, ein = "381415390", name = "teresa martin", position1 = "cfo") %>%
  add_row(year = 2012, ein = "381415390", name = "david west", position1 = "coo") %>%
  add_row(year = 2012, ein = "381415390", name = "valerie bryant", position1 = "cfo") %>%
  add_row(year = 2013, ein = "381415390", name = "david west", position1 = "coo") %>%
  add_row(year = 2013, ein = "381415390", name = "valerie bryant", position1 = "cfo") %>%
  add_row(year = 2010, ein = "381474929", name = "janet sternberg", position1 = "president", position2 = "ceo") %>%
  add_row(year = 2010, ein = "381474929", name = "jeffrey longbrake", position1 = "cfo") %>%
  add_row(year = 2010, ein = "381474929", name = "cindy gregorich", position1 = "coo", former = "part year") %>%
  add_row(year = 2011, ein = "381507302", name = "dan babcock", position1 = "ceo") %>%
  add_row(year = 2012, ein = "381507302", name = "dan babcock", position1 = "ceo") %>%
  add_row(year = 2012, ein = "382383119", name = "don kody", position1 = "president", position2 = "ceo") %>%
  add_row(year = 2012, ein = "382383119", name = "rick wyles", position1 = "cfo") %>%
  add_row(year = 2011, ein = "382791823", name = "james miller", position1 = "cfo") %>%
  add_row(year = 2011, ein = "382791823", name = "dennis lemanski", position1 = "cmo", title = "md") %>%
  add_row(year = 2012, ein = "382791823", name = "james miller", position1 = "cfo") %>%
  add_row(year = 2012, ein = "382791823", name = "dennis lemanski", position1 = "cmo", title = "md") %>%
  add_row(year = 2010, ein = "383330803", name = "shelleye yaklin", position1 = "president", position2 = "ceo") %>%
  add_row(year = 2010, ein = "383330803", name = "donald longpre", position1 = "cfo") %>%
  add_row(year = 2010, ein = "383330803", name = "susan pawlak", position1 = "ceo") %>%
  add_row(year = 2012, ein = "383330803", name = "shelleye yaklin", position1 = "president", position2 = "ceo") %>%
  add_row(year = 2012, ein = "383330803", name = "donald longpre", position1 = "cfo") %>%
  add_row(year = 2013, ein = "383330803", name = "shelleye yaklin", position1 = "president", position2 = "ceo") %>%
  add_row(year = 2013, ein = "383330803", name = "donald longpre", position1 = "cfo") %>%
  add_row(year = 2011, ein = "390704510", name = "brian theiler", position1 = "ceo") %>%
  add_row(year = 2011, ein = "390704510", name = "patti dockendorff", position1 = "cfo") %>%
  add_row(year = 2012, ein = "390890676", name = "john tremble", position1 = "cfo") %>%
  add_row(year = 2012, ein = "390890676", name = "dave dobosenski", position1 = "ceo") %>%
  add_row(year = 2010, ein = "390964813", name = "gregory olson", position1 = "ceo") %>%
  add_row(year = 2010, ein = "390964813", name = "lori peck", position1 = "cfo") %>%
  add_row(year = 2011, ein = "390992883", name = "james okeefe", position1 = "ceo") %>%
  add_row(year = 2011, ein = "390992883", name = "francis fish", position1 ="cfo")   %>%
  add_row(year = 2010, ein = "391091432", name = "robert van", position1 = "president", position2 = "ceo") %>%
  add_row(year = 2010, ein = "391091432", name = "jospeh svetlik", position1 = "vp", extra = "finance") %>%
  add_row(year = 2010, ein = "391091432", name = "dale turner", position1 = "vp", extra = "hr") %>%
  add_row(year = 2010, ein = "391091432", name = "george johnson", position1 = "president", position2 = "ceo") %>%
  add_row(year = 2011, ein = "391091432", name = "robert van", position1 = "president", position2 = "ceo") %>%
  add_row(year = 2011, ein = "391091432", name = "joseph svetlik", position1 = "vp", extra = "finance") %>%
  add_row(year = 2011, ein = "391091432", name = "dale turner", position1 = "vp", extra = "hr") %>%
  add_row(year = 2011, ein = "391091432", name = "george johnson", position1 = "president", position2 = "ceo") %>%
  add_row(year = 2010, ein = "396105970", name = "william petasnick", position1 = "pres", position2 = "ceo", extra = "fch") %>%
  add_row(year = 2010, ein = "396105970", name = "blaine oconnell", position1 = "cfo", extra = "fch") %>%
  add_row(year = 2010, ein = "396105970", name = "andrew norton", position1 = "cmo") %>%
  add_row(year = 2010, ein = "396105970", name = "catherine buck", position1 = "coo") %>%
  add_row(year = 2011, ein = "396105970", name = "blaine oconnell", position1 = "cfo") %>%
  add_row(year = 2011, ein = "396105970", name = "catherine jacobson", position1 = "cfo") %>%
  add_row(year = 2012, ein = "396105970", name = "william petasnick", position1 = "ceo") %>%
  add_row(year = 2012, ein = "396105970", name = "jeffrey van", position1 = "cfo") %>%
  add_row(year = 2014, ein = "410907986", name = "cynthia vincent", position1 = "cfo") %>%
  add_row(year = 2014, ein = "410907986", name = "brian prokosch", position1 = "vp", extra = "medical affairs", title = "md") %>%
  add_row(year = 2010, ein = "410919153", name = "mary maertens", position1 = "president", position2 = "ceo") %>%
  add_row(year = 2011, ein = "410919153", name = "mary maertens", position1 = "president", position2 = "ceo") %>%
  add_row(year = 2011, ein = "410919153", name = "sharon williams", position1 = "vp", extra = "finance") %>%
  add_row(year = 2010, ein = "411384358", name = "kevin smith", position1 = "ceo") %>%
  add_row(year = 2010, ein = "411384358", name = "mitchell kotrba", position1 = "cfo") %>%
  add_row(year = 2010, ein = "411763968", name = "gordon larson", position1 = "ceo") %>%
  add_row(year = 2010, ein = "411763968", name = "thomas kooiman", position1 = "ceo") %>%
  add_row(year = 2011, ein = "411763968", name = "thomas kooiman", position1 = "ceo") %>%
  add_row(year = 2011, ein = "411804205", name = "cathy huss", position1 = "cfo") %>%
  add_row(year = 2011, ein = "411804205", name = "keith okeson", position1 = "ceo", position2 = "president") %>%
  add_row(year = 2013, ein = "420710268", name = "wally winkler", position1 = "ceo", position2 = "cfo") %>%
  add_row(year = 2010, ein = "420860039", name = "leah marxen", position1 = "ceo") %>%
  add_row(year = 2010, ein = "420860039", name = "jin henkenius", position1 = "cfo") %>%
  add_row(year = 2010, ein = "421081055", name = "james platt", position1 = "ceo") %>%
  add_row(year = 2010, ein = "421081055", name = "bradley kokjohn", position1 = "cfo") %>%
  add_row(year = 2011, ein = "421081055", name = "james platt", position1 = "ceo") %>%
  add_row(year = 2011, ein = "421081055", name = "bradley kokjohn", position1 = "cfo") %>%
  add_row(year = 2012, ein = "421081055", name = "james platt", position1 = "ceo") %>%
  add_row(year = 2012, ein = "421081055", name = "bradley kokjohn", position1 = "cfo") %>%
  add_row(year = 2011, ein = "421320763", name = "debrah chensvold", position1 = "president", position2 = "ceo") %>%
  add_row(year = 2011, ein = "421320763", name = "joni gibleson", position1 = "cfo") %>%
  add_row(year = 2010, ein = "431240629", name = "daxton holcomb", position1 = "ceo") %>%
  add_row(year = 2010, ein = "431240629", name = "steve graddy", position1 = "cfo") %>%
  add_row(year = 2010, ein = "431656689", name = "martin williams", position1 = "cfo") %>%
  add_row(year = 2010, ein = "431656689", name = "greg johnson", position1 = "ceo", position2 = "president") %>%
  add_row(year = 2011, ein = "431656689", name = "maroc genice", position1 = "ceo", position2 = "president") %>%
  add_row(year = 2011, ein = "431656689", name = "martin williams", position1 = "cfo") %>%
  add_row(year = 2011, ein = "431656689", name = "greg johnson", position1 = "ceo", position2 = "president") %>%
  add_row(year = 2010, ein = "431704371", name = "gary duncan", position1 = "president", position2 = "ceo") %>%
  add_row(year = 2010, ein = "431704371", name = "joel kirk", position1 = "vp", extra = "executive") %>%
  add_row(year = 2010, ein = "431704371", name = "steve graddy", position1 = "cfo") %>%
  add_row(year = 2010, ein = "431704371", name = "paula baker", position1 = "ceo") %>%
  add_row(year = 2010, ein = "431704371", name = "richard schooler", position1 = "cmo") %>%
  add_row(year = 2010, ein = "431960221", name = "aaron krane", position1 = "cfo") %>%
  add_row(year = 2011, ein = "431960221", name = "norman gruber", position1 = "president", position2 = "ceo") %>%
  add_row(year = 2011, ein = "431960221", name = "aaron krane", position1 = "cfo") %>%
  add_row(year = 2013, ein = "440546366", name = "randall height", position1 = "cmo", title = "md") %>%
  add_row(year = 2013, ein = "440546366", name = "thomas luebbering", position1 = "vp", extra = "finance") %>%
  add_row(year = 2014, ein = "440584290", name = "william madney", position1 = "ceo") %>%
  add_row(year = 2014, ein = "440584290", name = "david strong", position1 = "cfo") %>%
  add_row(year = 2010, ein = "440668347", name = "rosa patti", position1 = "cfo") %>%
  add_row(year = 2011, ein = "440668347", name = "rosa patti", position1 = "cfo") %>%
  add_row(year = 2012, ein = "440668347", name = "rosa patti", position1 = "cfo") %>%
  add_row(year = 2014, ein = "450119890", name = "bruce bowersox", position1 = "ceo") %>%
  add_row(year = 2014, ein = "450119890", name = "steve forde", position1 = "cfo") %>%
  add_row(year = 2010, ein = "450253272", name = "roger unger", position1 = "ceo") %>%
  add_row(year = 2010, ein = "450253272", name = "beverly vilhauger", position1 = "cfo") %>%
  add_row(year = 2014, ein = "450254692", name = "leslie urvand", position1 = "ceo") %>%
  add_row(year = 2011, ein = "450306787", name = "lawrence blue", position1 = "ceo") %>%
  add_row(year = 2011, ein = "450306787", name = "julie feil", position1 = "cfo") %>%
  add_row(year = 2010, ein = "450310159", name = "everett butler", position1 = "ceo") %>%
  add_row(year = 2010, ein = "450310159", name = "rachel ray", position1 = "cfo") %>%
  add_row(year = 2011, ein = "450310159", name = "everett butler", position1 = "ceo") %>%
  add_row(year = 2011, ein = "450310159", name = "rachel ray", position1 = "cfo") %>%
  add_row(year = 2010, ein = "450447670", name = "ramona edwards", position1 = "cfo") %>%
  add_row(year = 2010, ein = "460255944", name = "angelina svihovec", position1 = "ceo") %>%
  add_row(year = 2010, ein = "460255944", name = "renae tisdall", position1 = "cfo") %>%
  add_row(year = 2011, ein = "460255944", name = "angelina svihovec", position1 = "ceo") %>%
  add_row(year = 2011, ein = "460255944", name = "renae tisdall", position1 = "cfo") %>%
  add_row(year = 2010, ein = "460278210", name = "dan ellis", position1 = "ceo") %>%
  add_row(year = 2010, ein = "460278210", name = "larry moen", position1 = "cfo") %>%
  add_row(year = 2010, ein = "466015787", name = "lee baldwin", position1 = "ceo") %>%
  add_row(year = 2010, ein = "466015787", name = "rita blasius", position1 = "ceo", former = "interim") %>%
  add_row(year = 2010, ein = "466015787", name = "darcy kepplinger", position1 = "cfo") %>%
  add_row(year = 2011, ein = "466015787", name = "lee baldwin", position1 = "ceo") %>%
  add_row(year = 2011, ein = "466015787", name = "darcy kepplinger", position1 = "cfo") %>%
  add_row(year = 2010, ein = "470376552", name = "john woodrich", position1 = "coo") %>%
  add_row(year = 2010, ein = "470376552", name = "kimberly russell", position1 = "president", position2 = "ceo") %>%
  add_row(year = 2010, ein = "470376552", name = "russell gronewold", position1 = "cfo") %>%
  add_row(year = 2010, ein = "470376552", name = "kathleen campbell", position1 = "vp", extra = "patient care") %>%
  add_row(year = 2010, ein = "470376552", name = "carolyn cody", position1 = "vp", extra = "medical affairs", title = "md") %>%
  add_row(year = 2014, ein = "470378779", name = "eric barber", position1 = "president", position2 = "ceo") %>%
  add_row(year = 2014, ein = "470378779", name = "shawn nordby", position1 = "cfo") %>%
  add_row(year = 2014, ein = "470378779", name = "michael skoch", position1 = "cmo", title = "md") %>%
  add_row(year = 2014, ein = "470378779", name = "bruce cutright", position1 = "vp", extra = "hr") %>%
  add_row(year = 2014, ein = "470378779", name = "mark callahan", position1 = "coo") %>%
  add_row(year = 2011, ein = "470379039", name = "charles schulz", position1 = "ceo") %>%
  add_row(year = 2011, ein = "470379039", name = "robert mcquistan", position1 = "vp", extra = "finance") %>%
  add_row(year = 2010, ein = "480799105", name = "clay fetsch", position1 = "president", extra = "medical staff", title = "md") %>%
  add_row(year = 2010, ein = "480799105", name = "james chromik", position1 = "ceo", position2 = "president") %>%
  add_row(year = 2010, ein = "480799105", name = "david buller", position1 = "vp", extra = "medical staff", title = "md") %>%
  add_row(year = 2010, ein = "480799105", name = "cindy brown", position1 = "cfo") %>%
  add_row(year = 2010, ein = "480799105", name = "jill wenger", position1 = "vp", extra = "hr") %>%
  add_row(year = 2011, ein = "480799105", name = "james chromik", position1 = "ceo", position2 = "president") %>%
  add_row(year = 2011, ein = "480799105", name = "cindy brown", position1 = "cfo") %>%
  add_row(year = 2011, ein = "480799105", name = "jill wenger", position1 = "vp", extra = "hr") %>%
  add_row(year = 2010, ein = "486099245", name = "thomas macarones", position1 = "cfo") %>%
  add_row(year = 2010, ein = "486099245", name = "jodi schmidt", position1 = "president", position2 = "ceo") %>%
  add_row(year = 2014, ein = "510069243", name = "steven rose", position1 = "ceo") %>%
  add_row(year = 2014, ein = "510069243", name = "penny short", position1 = "coo") %>%
  add_row(year = 2014, ein = "510069243", name = "denise jester", position1 = "cfo") %>%
  add_row(year = 2010, ein = "540261840", name = "elliot kuida", position1 = "coo") %>%
  add_row(year = 2010, ein = "540261840", name = "marijo lecker", position1 = "vp", extra = "clinical support") %>%
  add_row(year = 2010, ein = "540261840", name = "finlay ashby", position1 = "vp", extra = "medical affairs") %>%
  add_row(year = 2010, ein = "540525802", name = "randall kelly", position1 = "ceo") %>%
  add_row(year = 2010, ein = "540525802", name = "gregory bentz", position1 = "cmo") %>%
  add_row(year = 2010, ein = "540525802", name = "glenn zirbser", position1 = "cfo") %>%
  add_row(year = 2010, ein = "540525802", name = "susan carroll", position1 = "coo") %>%
  add_row(year = 2010, ein = "541453954", name = "mary mannix", position1 = "president", position2 = "ceo") %>%
  add_row(year = 2010, ein = "541453954", name = "john heider", position1 = "cfo") %>%
  add_row(year = 2010, ein = "541453954", name = "fred castello", position1 = "cmo", title = "md") %>%
  add_row(year = 2010, ein = "541453954", name = "susamn krzastek", position1 = "vp", extra = "hr") %>%
  add_row(year = 2014, ein = "541453954", name = "mary mannix", position1 = "president", position2 = "ceo") %>%
  add_row(year = 2014, ein = "541453954", name = "robert riley", position1 = "cfo") %>%
  add_row(year = 2014, ein = "541453954", name = "fred castello", position1 = "cmo", title = "md") %>%
  add_row(year = 2014, ein = "541453954", name = "lisa cline", position1 = "coo") %>%
  add_row(year = 2014, ein = "541453954", name = "dan oconner", position1 = "vp", extra = "hr") %>%
  add_row(year = 2010, ein = "550357045", name = "jay prager", position1 = "ceo") %>%
  add_row(year = 2010, ein = "550357045", name = "william hunt", position1 = "cfo") %>%
  add_row(year = 2011, ein = "550357045", name = "jay prager", position1 = "ceo") %>%
  add_row(year = 2011, ein = "550357045", name = "william hunt", position1 = "cfo") %>%
  add_row(year = 2014, ein = "550357045", name = "jay prager", position1 = "ceo") %>%
  add_row(year = 2014, ein = "550357045", name = "william hunt", position1 = "cfo") %>%
  add_row(year = 2014, ein = "550357045", name = "kevin brent", position1 = "coo") %>%
  add_row(year = 2010, ein = "550359755", name = "kc nau", position1 = "vp", extra = "medical affairs", title = "md") %>%
  add_row(year = 2010, ein = "550372580", name = "vickie gay", position1 = "president", position2 = "ceo") %>%
  add_row(year = 2010, ein = "550372580", name = "sherry murray", position1 = "cfo") %>%
  add_row(year = 2010, ein = "550387249", name = "joseph endrich", position1 = "ceo") %>%
  add_row(year = 2010, ein = "550387249", name = "michael miller", position1 = "cfo") %>%
  add_row(year = 2011, ein = "550387249", name = "joseph endrich", position1 = "ceo") %>%
  add_row(year = 2010, ein = "550440086", name = "thomas schauer", position1 = "cfo") %>%
  add_row(year = 2011, ein = "550440086", name = "alvin lawson", position1 = "president", position2 = "ceo") %>%
  add_row(year = 2011, ein = "550440086", name = "thomas schauer", position1 = "president", position2 = "ceo") %>%
  add_row(year = 2011, ein = "550440086", name = "hugh collins", position1 = "president", position2 = 'ceo') %>%
  add_row(year = 2010, ein = "550462730", name = "sandra elza", position1 = "ceo") %>%
  add_row(year = 2010, ein = "550462730", name = "angela frame", position1 = "cfo") %>%
  add_row(year = 2010, ein = "550462730", name = "stephanie mccoy", position1 = "coo") %>%
  add_row(year = 2011, ein = "550462730", name = "sandra elza", position1 = "ceo") %>%
  add_row(year = 2011, ein = "550462730", name = "stephanie mccoy", position1 = "ceo", former = "interim") %>%
  add_row(year = 2011, ein = "550462730", name = "angela frame", position1 = "cfo") %>%
  add_row(year = 2010, ein = "550491651", name = "deborah hill", position1 = "ceo") %>%
  add_row(year = 2010, ein = "550491651", name = "dora douglas", position1 = "cfo") %>%
  add_row(year = 2011, ein = "550491651", name = "deborah hill", position1 = "ceo") %>%
  add_row(year = 2011, ein = "550491651", name = "dora douglas", position1 = "cfo") %>%
  add_row(year = 2010, ein = "550629032", name = "barbara lay", position1 = "ceo") %>%
  add_row(year = 2010, ein = "550629032", name = "stephen whited", position1 = "coo", position2 = "cfo") %>%
  add_row(year = 2010, ein = "550754713", name = "thomas jones", position1 = "president", position2 = "ceo") %>%
  add_row(year = 2010, ein = "550754713", name = "john yeager", position1 = "cfo") %>%
  add_row(year = 2010, ein = "556000526", name = "jeff lilley", position1 = "ceo") %>%
  add_row(year = 2010, ein = "556000526", name = "brian kelbaugh", position1 = "cfo") %>%
  add_row(year = 2011, ein = "556000526", name = "jeff lilley", position1 = "ceo") %>%
  add_row(year = 2011, ein = "556000526", name = "brian kelbaugh", position1 = "cfo") %>%
  add_row(year = 2011, ein = "556000526", name = "pat shaw", position1 = "ceo") %>%
  add_row(year = 2010, ein = "560554202", name = "laura easton", position1 = "president", position2 = "ceo") %>%
  add_row(year = 2010, ein = "560554202", name = "donald gardner", position1 = "vp", extra = "finance") %>%
  add_row(year = 2010, ein = "560554202", name = "rebecca smith", position1 = "vp", extra = "hr") %>%
  add_row(year = 2010, ein = "560554222", name = "alexander bell", position1 = "ceo", former = "interim") %>%
  add_row(year = 2010, ein = "560554222", name = "robert cress", position1 = "cfo") %>%
  add_row(year = 2011, ein = "560554222", name = "christine martin", position1 = "cfo") %>%
  add_row(year = 2011, ein = "560554222", name = "ken shull", position1 = "ceo") %>%
  add_row(year = 2012, ein = "560619359", name = "david oconner", position1 = "cfo") %>%
  add_row(year = 2012, ein = "560619359", name = "randall kelley", position1 = "ceo") %>%
  add_row(year = 2012, ein = "560619359", name = "doug luckett", position1 = "coo") %>%
  add_row(year = 2012, ein = "560619359", name = "jerry levine", position1 = "cmo") %>%
  add_row(year = 2012, ein = "560619359", name = "k hartwell", position1 = "vp", extra = "patient care") %>%
  add_row(year = 2010, ein = "560845796", name = "michael nagowski", position1 = "ceo") %>%
  add_row(year = 2010, ein = "560845796", name = "sandra william", position1 = "cfo") %>%
  add_row(year = 2010, ein = "560845796", name = "joyce korzen", position1 = "coo") %>%
  add_row(year = 2014, ein = "562276994", name = "danny squires", position1 = "cfo") %>%
  add_row(year = 2014, ein = "562276994", name = "clyde bristow", position1 = "cno") %>%
  add_row(year = 2014, ein = "562276994", name = "brett nicks", position1 = "cmo", title = "md") %>%
  add_row(year = 2011, ein = "580566121", name = "perry mustian", position1 = "president", extra = "hosp") %>%
  add_row(year = 2011, ein = "580566121", name = "kevin taylor", position1 = "coo") %>%
  add_row(year = 2011, ein = "580566121", name = "charles tomlinson", position1 = "vp", extra = "medic aff", title = "md") %>%
  add_row(year = 2011, ein = "581519911", name = "gary howard", position1 = "cfo") %>%
  add_row(year = 2011, ein = "581519911", name = "john bowling", position1 = "president", position2 = "ceo") %>%
  add_row(year = 2010, ein = "581649541", name = "gregory simone", position1 = "ceo", title = "md") %>%
  add_row(year = 2010, ein = "581649541", name = "david anderson", position1 = "vp", extra = "exec") %>%
  add_row(year = 2010, ein = "581649541", name = "darold etheridge", position1 = "vp", extra = "finance") %>%
  add_row(year = 2010, ein = "581649541", name = "kenneth kunze", position1 = "cmo", title = "md") %>%
  add_row(year = 2010, ein = "581649541", name = "james budzinski", position1 = "cfo") %>%
  add_row(year = 2010, ein = "581649541", name = "michael graue", position1 = "coo") %>%
  add_row(year = 2010, ein = "581649541", name = "ellen langford", position1 = "vp", extra = "med affairs") %>%
  add_row(year = 2010, ein = "581649541", name = "robin wilson", position1 = "vp", extra = "medical management", title = "md") %>%
  add_row(year = 2010, ein = "581649541", name = "linda clark", position1 = "coo") %>%
  add_row(year = 2010, ein = "581649541", name = "andy tatnall", position1 = "vp", extra = "clinical services") %>%
  add_row(year = 2011, ein = "581649541", name = "david anderson", position1 = "vp", extra = "exec") %>%
  add_row(year = 2011, ein = "581649541", name = "darold etheridge", position1 = "vp", extra = "finance") %>%
  add_row(year = 2011, ein = "581649541", name = "marcia delk", position1 = "cqo") %>%
  add_row(year = 2011, ein = "581649541", name = "kenneth kunze", position1 = "cmo", title = "md") %>%
  add_row(year = 2011, ein = "581649541", name = "james budzinski", position1 = "cfo", position2 = "ceo") %>%
  add_row(year = 2011, ein = "581649541", name = "ellen langford", position1 = "vp", extra = "med affairs") %>%
  add_row(year = 2011, ein = "581649541", name = "robin wilson", position1 = "vp", extra = "medical management", title = "md") %>%
  add_row(year = 2011, ein = "581649541", name = "robert jansen", position1 = "vp", extra = "medical affairs", title = "md") %>%
  add_row(year = 2011, ein = "581649541", name = "linda clark", position1 = "coo") %>%
  add_row(year = 2012, ein = "581649541", name = "reynold jennings", position1 = "president", position2 = "ceo") %>%
  add_row(year = 2012, ein = "581649541", name = "james budzinski", position1 = "cfo") %>%
  add_row(year = 2012, ein = "581649541", name = "marcia delk", position1 = "cqo") %>%
  add_row(year = 2012, ein = "581649541", name = "kenneth kunze", position1 = "cmo", title = "md") %>%
  add_row(year = 2012, ein = "581649541", name = "michael graue", position1 = "coo") %>%
  add_row(year = 2012, ein = "581649541", name = "ellen langford", position1 = "coo") %>%
  add_row(year = 2012, ein = "581649541", name = "robert jansen", position1 = "pres", extra = "medical group", title = "md") %>%
  add_row(year = 2010, ein = "581694098", name = "james gardner", position1 = "president", position2 = "ceo") %>%
  add_row(year = 2010, ein = "581694098", name = "carrol burrell", position1 = "coo") %>%
  add_row(year = 2010, ein = "581694098", name = "anthony herdener", position1 = "cfo") %>%
  add_row(year = 2010, ein = "581694098", name = "samuel johnson", position1 = "cmo") %>%
  add_row(year = 2010, ein = "581954432", name = "robert quattrocchi", position1 = "president", position2 = "ceo") %>%
  add_row(year = 2010, ein = "581954432", name = "deborah mitcham", position1 = "cfo") %>%
  add_row(year = 2011, ein = "581954432", name = "robert quattrocchi", position1 = "president", position2 = "ceo") %>%
  add_row(year = 2011, ein = "581954432", name = "deborah mitcham", position1 = "cfo") %>%
  add_row(year = 2014, ein = "582149127", name = "ninfa saunders", position1 = "president", position2 = "ceo") %>%
  add_row(year = 2014, ein = "582149127", name = "rhonda perry", position1 = "cfo") %>%
  add_row(year = 2014, ein = "582179986", name = "james moore", position1 = "cmo", title = "md") %>%
  add_row(year = 2010, ein = "582200195", name = "charles scott", position1 = "ceo") %>%
  add_row(year = 2010, ein = "582200195", name = "claude carruth", position1 = "cfo") %>%
  add_row(year = 2010, ein = "582200195", name = "john edwards", position1 = "coo") %>%
  add_row(year = 2011, ein = "582200195", name = "charles scott", position1 = "ceo") %>%
  add_row(year = 2011, ein = "582200195", name = "gary chawk", position1 = "cfo") %>%
  add_row(year = 2011, ein = "582200195", name = "john edward", position1 = "coo") %>%
  add_row(year = 2012, ein = "586002701", name = "douglas keir", position1 = "ceo") %>%
  add_row(year = 2012, ein = "586002701", name = "patricia parris", position1 = "cfo") %>%
  add_row(year = 2010, ein = "590724462", name = "lance anastasio", position1 = "president", position2 = "ceo") %>%
  add_row(year = 2010, ein = "590747311", name = "hugh green", position1 = "president", position2 = "ceo") %>%
  add_row(year = 2010, ein = "590747311", name = "john wilbanks", position1 = "coo") %>%
  add_row(year = 2010, ein = "590747311", name = "michael lukaszewski", position1 = "cfo") %>%
  add_row(year = 2010, ein = "590747311", name = "keith stein", position1 = "cmo", title = "md") %>%
  add_row(year = 2010, ein = "590747311", name = "jerry bridgham", position1 = "cmo", extra = "wch", title = "md") %>%
  add_row(year = 2010, ein = "590872594", name = "guillermo pol", position1 = "president", extra = "medical staff", title = "md") %>%
  add_row(year = 2010, ein = "590872594", name = "lincoln mendez", position1 = "ceo") %>%
  add_row(year = 2010, ein = "590872594", name = "richard freeburg", position1 = "coo") %>%
  add_row(year = 2013, ein = "590872594", name = "yvonne johnson", position1 = "president", extra = "medical staff", title = "md") %>%
  add_row(year = 2013, ein = "590872594", name = "lincoln mendez", position1 = "ceo") %>%
  add_row(year = 2013, ein = "590872594", name = "jeanette stone", position1 = "vp", extra = "operations") %>%
  add_row(year = 2014, ein = "590872594", name = "lincoln mendez", position1 = "ceo") %>%
  add_row(year = 2014, ein = "590872594", name = "jeanette stone", position1 = "vp", extra = "operations") %>%
  add_row(year = 2010, ein = "591987355", name = "elisa brown", position1 = "president", extra = "medical staff", title = "md") %>%
  add_row(year = 2010, ein = "591987355", name = "rick freeburg", position1 = "ceo") %>%
  add_row(year = 2010, ein = "592425149", name = "kerry vermillion", position1 = "cfo") %>%
  add_row(year = 2011, ein = "592425149", name = "kerry vermillion", position1 = "cfo") %>%
  add_row(year = 2010, ein = "592477479", name = "roy wright", position1 = "president", extra = "cch") %>%
  add_row(year = 2010, ein = "592477479", name = "michael mcgoohan", position1 = "president", extra = "medical staff", title = "do") %>%
  add_row(year = 2010, ein = "592477479", name = "robert galloway", position1 = "cfo") %>%
  add_row(year = 2010, ein = "592477479", name = "rodney moore", position1 = "vp", extra = "medical affairs") %>%
  add_row(year = 2011, ein = "592477479", name = "alphonse pecoraro", position1 = "president", extra = "medical staff", title = "md") %>%
  add_row(year = 2010, ein = "592650456", name = "paul powers", position1 = "cfo") %>%
  add_row(year = 2010, ein = "592650456", name = "edwin sammer", position1 = "cmo", title = "md") %>%
  add_row(year = 2010, ein = "592650456", name = "jack stephens", position1 = "ceo") %>%
  add_row(year = 2010, ein = "592650456", name = "jeffrey payne", position1 = "vp", extra = "hr") %>%
  add_row(year = 2010, ein = "592980620", name = "john wilbanks", position1 = "vp", extra = "exec") %>%
  add_row(year = 2010, ein = "592980620", name = "michael lukaszewski", position1 = "cfo") %>%
  add_row(year = 2010, ein = "593051173", name = "ronald gilliard", position1 = "ceo") %>%
  add_row(year = 2010, ein = "593051173", name = "nathan ebersole", position1 = "cfo") %>%
  add_row(year = 2011, ein = "593051173", name = "nathan ebersole", position1 = "cfo") %>%
  add_row(year = 2012, ein = "593051173", name = "nathan ebersole", position1 = "cfo") %>%
  add_row(year = 2011, ein = "593122517", name = "richard huth", position1 = "ceo") %>%
  add_row(year = 2013, ein = "593122517", name = "geri forbes", position1 = "ceo") %>%
  add_row(year = 2011, ein = "610461767", name = "william kindred", position1 = "ceo") %>%
  add_row(year = 2011, ein = "610461767", name = "anthony sudduth", position1 = "cfo", position2 = "coo") %>%
  add_row(year = 2010, ein = "610461940", name = "robert hudson", position1 = "ceo") %>%
  add_row(year = 2010, ein = "610461940", name = "christopher jones", position1 = "ceo") %>%
  add_row(year = 2010, ein = "610461940", name = "chris mcclurg", position1 = "cfo") %>%
  add_row(year = 2011, ein = "610461940", name = "christopher jones", position1 = "ceo") %>%
  add_row(year = 2011, ein = "610461940", name = "chris mcclurg", position1 = "cfo") %>%
  add_row(year = 2010, ein = "610471581", name = "alisa coleman", position1 = "ceo") %>%
  add_row(year = 2010, ein = "610471581", name = "bethany goss", position1 = "cfo") %>%
  add_row(year = 2010, ein = "610624096", name = "steve burns", position1 = "coo") %>%
  add_row(year = 2010, ein = "610624096", name = "richard neikirk", position1 = "ceo") %>%
  add_row(year = 2010, ein = "610624096", name = "richie capps", position1 = "cfo") %>%
  add_row(year = 2010, ein = "611286361", name = "barber jeffrey", position1 = "president", position2 = "ceo") %>%
  add_row(year = 2010, ein = "611286361", name = "hackbarth john", position1 = "cfo") %>%
  add_row(year = 2010, ein = "611286361", name = "medley richard", position1 = "cmo") %>%
  add_row(year = 2010, ein = "611286361", name = "strahan greg", position1 = "coo") %>%
  add_row(year = 2010, ein = "611286361", name = "jones lisa", position1 = "vp", extra = "clinical services") %>%
  add_row(year = 2010, ein = "611286361", name = "ranallo russell", position1 = "vp", extra = "financial services") %>%
  add_row(year = 2011, ein = "620988604", name = "thomas kidd", position1 = "cfo") %>%
  add_row(year = 2011, ein = "620988604", name = "dennis wolford", position1 = "ceo") %>%
  add_row(year = 2010, ein = "621373691", name = "anthony spezia", position1 = "president", position2 = "ceo") %>%
  add_row(year = 2010, ein = "621373691", name = "john gepps", position1 = "cfo") %>%
  add_row(year = 2010, ein = "621373691", name = "greg sommers", position1 = "cfo") %>%
  add_row(year = 2013, ein = "621373691", name = "anthony spezia", position1 = "president", position2 = "ceo") %>%
  add_row(year = 2013, ein = "621373691", name = "john gepps", position1 = "cfo") %>%
  add_row(year = 2013, ein = "621373691", name = "michael hamilton", position1 = "vp", extra = "financial services") %>%
  add_row(year = 2014, ein = "621373691", name = "anthony spezia", position1 = "president", position2 = "ceo") %>%
  add_row(year = 2014, ein = "621373691", name = "john gepps", position1 = "cfo") %>%
  add_row(year = 2014, ein = "621373691", name = "michael hamilton", position1 = "vp", extra = "financial services") %>%
  add_row(year = 2010, ein = "621519754", name = "edwin cade", position1 = "ceo") %>%
  add_row(year = 2010, ein = "621519754", name = "james robertson", position1 = "cfo") %>%
  add_row(year = 2010, ein = "631058174", name = "jim weidner", position1 = "ceo") %>%
  add_row(year = 2010, ein = "631058174", name = "jete edmisson", position1 = "cfo", position2 = "coo") %>%
  add_row(year = 2010, ein = "640333594", name = "alvin hoover", position1 = "ceo") %>%
  add_row(year = 2010, ein = "640333594", name = "randy pirtle", position1 = "cfo") %>%
  add_row(year = 2010, ein = "640362400", name = "william casey", position1 = "president", position2 = "ceo") %>%
  add_row(year = 2010, ein = "640362400", name = "leland ray", position1 = "ceo") %>%
  add_row(year = 2011, ein = "640663760", name = "walter grace", position1 = "ceo") %>%
  add_row(year = 2011, ein = "640663760", name = "josh harmond", position1 = "cfo") %>%
  add_row(year = 2011, ein = "640663760", name = "kyle armstrong", position1 = "ceo") %>%
  add_row(year = 2011, ein = "640663760", name = "donovan leonard", position1 = "cfo") %>%
  add_row(year = 2010, ein = "710329353", name = "robert atkinson", position1 = "ceo") %>%
  add_row(year = 2010, ein = "710329353", name = "walter johnson", position1 = "president", position2 = "ceo") %>%
  add_row(year = 2010, ein = "710329353", name = "thomas harbuck", position1 = "vp", extra = "executive") %>%
  add_row(year = 2014, ein = "710329353", name = "walter johnson", position1 = "president", position2 = "ceo") %>%
  add_row(year = 2014, ein = "710329353", name = "thomas harbuck", position1 = "vp", extra = "executive") %>%
  add_row(year = 2014, ein = "710329353", name = "brian thomas", position1 = "coo") %>%
  add_row(year = 2010, ein = "720423659", name = "james montgomery", position1 = "president", position2 = "ceo") %>%
  add_row(year = 2010, ein = "720423659", name = "kevin jordan", position1 = "cmo") %>%
  add_row(year = 2010, ein = "720423659", name = "robert ficken", position1 = "cfo") %>%
  add_row(year = 2010, ein = "720423659", name = "susan pitoscia", position1 = "coo") %>%
  add_row(year = 2010, ein = "720423659", name = "suzanne haggard", position1 = "cfo") %>%
  add_row(year = 2010, ein = "720423659", name = "chad courrege", position1 = "vp", extra = "hr") %>%
  add_row(year = 2011, ein = "720423659", name = "susan pitoscia", position1 = "coo") %>%
  add_row(year = 2011, ein = "720423659", name = "penny menge", position1 = "vp", extra = "patient care services") %>%
  add_row(year = 2011, ein = "720423659", name = "chad courrege", position1 = "vp", extra = "hr") %>%
  add_row(year = 2010, ein = "720445607", name = "pamela primeaux", position1 = "cfo") %>%
  add_row(year = 2010, ein = "720445607", name = "terry terrebone", position1 = "executive") %>%
  add_row(year = 2010, ein = "720445607", name = "keith simpson", position1 = "coo") %>%
  add_row(year = 2010, ein = "720445607", name = "dana williams", position1 = "coo") %>%
  add_row(year = 2011, ein = "720445607", name = "pamela primeaux", position1 = "cfo") %>%
  add_row(year = 2011, ein = "720445607", name = "keith simpson", position1 = "coo") %>%
  add_row(year = 2011, ein = "720445607", name = "dana williams", position1 = "ceo") %>%
  add_row(year = 2010, ein = "721479692", name = "todd epler", position1 = "ceo") %>%
  add_row(year = 2010, ein = "721479692", name = "layla chase", position1 = "cfo") %>%
  add_row(year = 2011, ein = "730700090", name = "jake henry", position1 = "president", position2 = "ceo") %>%
  add_row(year = 2011, ein = "730700090", name = "barry steichen", position1 = "cfo") %>%
  add_row(year = 2011, ein = "730700090", name = "thomas neff", position1 = "coo") %>%
  add_row(year = 2010, ein = "730790960", name = "james berry", position1 = "ceo") %>%
  add_row(year = 2010, ein = "730790960", name = "jennifer warren", position1 = "cfo") %>%
  add_row(year = 2011, ein = "730790960", name = "jennifer warren", position1 = "cfo") %>%
  add_row(year = 2012, ein = "730790960", name = "victor pascual", position1 = "cmo", title = "md") %>%
  add_row(year = 2010, ein = "731235996", name = "dan clements", position1 = "ceo") %>%
  add_row(year = 2010, ein = "731235996", name = "diane downard", position1 = "cfo") %>%
  add_row(year = 2010, ein = "731235996", name = "bill johnson", position1 = "coo") %>%
  add_row(year = 2011, ein = "731235996", name = "dan clements", position1 = "ceo") %>%
  add_row(year = 2011, ein = "731235996", name = "diane downard", position1 = "cfo") %>%
  add_row(year = 2011, ein = "731235996", name = "bill johnson", position1 = "ceo") %>%
  add_row(year = 2010, ein = "741461220", name = "michael thompson", position1 = "ceo") %>%
  add_row(year = 2010, ein = "741471231", name = "julie butler", position1 = "coo") %>%
  add_row(year = 2011, ein = "741471231", name = "julie butler", position1 = "coo") %>%
  add_row(year = 2014, ein = "741471231", name = "willis reese", position1 = "ceo") %>%
  add_row(year = 2011, ein = "742851819", name = "kenneth randall", position1 = "ceo") %>%
  add_row(year = 2011, ein = "742851819", name = "william zemanek", position1 = "cfo") %>%
  add_row(year = 2010, ein = "750711276", name = "edward wilson", position1 = "cfo") %>%
  add_row(year = 2010, ein = "760698013", name = "cherri waites", position1 = "cfo") %>%
  add_row(year = 2011, ein = "760698013", name = "cherri waites", position1 = "cfo") %>%
  add_row(year = 2010, ein = "810231787", name = "david henry", position1 = "president", position2 = "ceo") %>%
  add_row(year = 2010, ein = "810231787", name = "suzanne swietnicki", position1 = "cmo", title = "md") %>%
  add_row(year = 2010, ein = "810231787", name = "kim lucke", position1 = "vp", extra = "finance") %>%
  add_row(year = 2011, ein = "810231787", name = "david henry", position1 = "president", position2 = "ceo") %>%
  add_row(year = 2011, ein = "810231787", name = "kim lucke", position1 = "vp", extra = "finance") %>%
  add_row(year = 2010, ein = "810236460", name = "pete brekhus", position1 = "cfo") %>%
  add_row(year = 2010, ein = "810236460", name = "jamie davis", position1 = "coo") %>%
  add_row(year = 2010, ein = "810236460", name = "tom mitchell", position1 = "ceo") %>%
  add_row(year = 2012, ein = "810269223", name = "nancy rosaaen", position1 = "ceo") %>%
  add_row(year = 2011, ein = "826000422", name = "max long", position1 = "ceo") %>%
  add_row(year = 2011, ein = "826000422", name = "larry droppers", position1 = "cfo") %>%
  add_row(year = 2011, ein = "840398876", name = "karl gills", position1 = "ceo") %>%
  add_row(year = 2011, ein = "840398876", name = "frank may", position1 = "coo") %>%
  add_row(year = 2011, ein = "840398876", name = "julie johnson", position1 = "cfo") %>%
  add_row(year = 2010, ein = "860208451", name = "richard smith", position1 = "ceo") %>%
  add_row(year = 2010, ein = "860208451", name = "john munson", position1 = "cfo") %>%
  add_row(year = 2010, ein = "860260959", name = "victoria clark", position1 = "ceo") %>%
  add_row(year = 2010, ein = "860260959", name = "james ehasz", position1 = "cfo") %>%
  add_row(year = 2012, ein = "860427850", name = "rona curphy", position1 = "ceo") %>%
  add_row(year = 2012, ein = "860427850", name = "karen francis", position1 = "cfo") %>%
  add_row(year = 2012, ein = "860427850", name = "william stubbs", position1 = "vp", extra = "medical affairs", title = "md") %>%
  add_row(year = 2014, ein = "860427850", name = "rona curphy", position1 = "ceo") %>%
  add_row(year = 2014, ein = "860427850", name = "karen francis", position1 = "cfo") %>%
  add_row(year = 2010, ein = "870276435", name = "bradley lebaron", position1 = "president", position2 = "ceo") %>%
  add_row(year = 2010, ein = "870276435", name = "brent hales", position1 = "cfo") %>%
  add_row(year = 2011, ein = "870276435", name = "brent hales", position1 = "cfo") %>%
  add_row(year = 2011, ein = "870276435", name = "bradley lebaron", position1 = "president", position2 = "ceo") %>%
  add_row(year = 2010, ein = "870543342", name = "roy barraclough", position1 = "ceo") %>%
  add_row(year = 2010, ein = "870543342", name = "ken knight", position1 = "cfo") %>%
  add_row(year = 2010, ein = "870741588", name = "scott kelly", position1 = "ceo") %>%
  add_row(year = 2011, ein = "870741588", name = "randall neely", position1 = "ceo") %>%
  add_row(year = 2010, ein = "910567263", name = "richard linneweh", position1 = "president", position2 = "ceo") %>%
  add_row(year = 2010, ein = "910567263", name = "john vornbrock", position1 = "cfo") %>%
  add_row(year = 2010, ein = "930430029", name = "richard stenson", position1 = "ceo", position2 = "president") %>%
  add_row(year = 2010, ein = "930430029", name = "manuel berman", position1 = "coo") %>%
  add_row(year = 2010, ein = "930430029", name = "tim fleischmann", position1 = "cfo") %>%
  add_row(year = 2010, ein = "930430029", name = "eugene zurbrugg", position1 = "ceo", position2 = "president") %>%
  add_row(year = 2011, ein = "930430029", name = "richard stenson", position1 = "ceo", position2 = "president") %>%
  add_row(year = 2011, ein = "930430029", name = "manuel berman", position1 = "coo") %>%
  add_row(year = 2011, ein = "930430029", name = "tim fleischmann", position1 = "cfo") %>%
  add_row(year = 2011, ein = "930430029", name = "marc lewis", position1 = "president", extra = "medical staff", title = "md") %>%
  add_row(year = 2010, ein = "930602940", name = "james diegel", position1 = "president", position2 = "ceo") %>%
  add_row(year = 2010, ein = "930602940", name = "karen shepard", position1 = "cfo") %>%
  add_row(year = 2010, ein = "930602940", name = "james henry", position1 = "ceo") %>%
  add_row(year = 2010, ein = "930602940", name = "alan ertle", position1 = "vp", extra = "med affairs") %>%
  add_row(year = 2010, ein = "930602940", name = "katherine vitcovich", position1 = "vp", position2 = "hr") %>%
  add_row(year = 2010, ein = "930602940", name = "patrick varga", position1 = "ceo") %>%
  add_row(year = 2010, ein = "952082686", name = "sharon johansson", position1 = "cfo") %>%
  add_row(year = 2010, ein = "952082686", name = "james suver", position1 = "ceo") %>%
  add_row(year = 2010, ein = "952082686", name = "david mechtenberg", position1 = "ceo") %>%
  add_row(year = 2011, ein = "952082686", name = "lois johnson", position1 = "cfo") %>%
  add_row(year = 2011, ein = "952082686", name = "james suver", position1 = "ceo") %>%
  add_row(year = 2013, ein = "952224265", name = "ronald werft", position1 = "ceo") %>%
  add_row(year = 2013, ein = "952224265", name = "steven fellows", position1 = "coo") %>%
  add_row(year = 2013, ein = "952224265", name = "joan bricher", position1 = "cfo") %>%
  add_row(year = 2014, ein = "952224265", name = "joan bricher", position1 = "cfo") %>%
  add_row(year = 2014, ein = "952224265", name = "ronald werft", position1 = "president", position2 = "ceo") %>%
  add_row(year = 2014, ein = "952224265", name = "steven fellows", position1 = "coo") %>%
  add_row(year = 2013, ein = "952413596", name = "ronald werft", position1 = "ceo") %>%
  add_row(year = 2013, ein = "952413596", name = "steven fellows", position1 = "coo") %>%
  add_row(year = 2013, ein = "952413596", name = "joan bricher", position1 = "cfo") %>%
  add_row(year = 2014, ein = "952413596", name = "joan bricher", position1 = "cfo") %>%
  add_row(year = 2014, ein = "952413596", name = "ronald werft", position1 = "president", position2 = "ceo") %>%
  add_row(year = 2014, ein = "952413596", name = "steven fellows", position1 = "coo") %>%
  add_row(year = 2014, ein = "952821104", name = "roger seaver", position1 = "president", position2 = "ceo") %>%
  add_row(year = 2014, ein = "952821104", name = "john schleif", position1 = "coo") %>%
  add_row(year = 2014, ein = "952821104", name = "bob hudson", position1 = "cfo") %>%
  add_row(year = 2014, ein = "952821104", name = "mark puleo", position1 = "vp", extra = "hr") %>%
  add_row(year = 2013, ein = "990260423", name = "lowell johnson", position1 = "ceo") %>%
  add_row(year = 2013, ein = "990260423", name = "jason paret", position1 = "cfo") %>%
  add_row(year = 2013, ein = "990260423", name = "william brown", position1 = "vp", extra = "hr") %>%
  add_row(year = 2013, ein = "990260423", name = "lorie ann", position1 = "vp", extra = "patient care") %>%
  add_row(year = 2014, ein = "", name = "", position1 = ) %>%
  add_row(year = 2010, ein = "", name = "", position1 = ) %>%
  add_row(year = 2010, ein = "", name = "", position1 = ) %>%
  add_row(year = 2010, ein = "", name = "", position1 = ) %>%
  add_row(year = 2010, ein = "", name = "", position1 = ) %>%
  add_row(year = 2010, ein = "", name = "", position1 = ) %>%
  add_row(year = 2010, ein = "", name = "", position1 = ) %>%
  add_row(year = 2010, ein = "", name = "", position1 = ) %>%
  add_row(year = 2010, ein = "", name = "", position1 = ) %>%
  add_row(year = 2010, ein = "", name = "", position1 = ) %>%
  add_row(year = 2010, ein = "", name = "", position1 = ) %>%
  add_row(year = 2010, ein = "", name = "", position1 = ) %>%
  add_row(year = 2010, ein = "", name = "", position1 = ) %>%
  add_row(year = 2010, ein = "", name = "", position1 = ) %>%
  add_row(year = 2010, ein = "", name = "", position1 = ) %>%
  add_row(year = 2010, ein = "", name = "", position1 = ) %>%
  add_row(year = 2010, ein = "", name = "", position1 = ) %>%
  add_row(year = 2010, ein = "", name = "", position1 = ) %>%
  add_row(year = 2010, ein = "", name = "", position1 = ) %>%
  add_row(year = 2010, ein = "", name = "", position1 = ) %>%
  

saveRDS(cleaned_text, paste0(created_data_path, "cleaned_text.rds"))


