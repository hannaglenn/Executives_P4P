library(readr)
library(dplyr)
library(stringr)
library(tidyr)

source("paths.R")

# read in names data
cleaned_text <- read_rds(paste0(created_data_path, "names_data.rds"))

# create common names for the positions that can be grouped
cleaned_text <- cleaned_text %>%
  mutate(position1 = ifelse(position1 %in% c("president", "pres"), "president", position1)) %>%
  mutate(position1 = ifelse(position1 %in% c("ceo", "ceocfo", "chief executive officer"), "ceo", position1)) %>%
  mutate(position1 = ifelse(position1 %in% c("system executive", "medical director", "executive", "treasurer", "secretary", "vice chairman", "chairman", "vice chair", "chairperson", "chair", "vice-chair", "director", "trustee", "board"), "board", position1)) %>%
  mutate(position1 = ifelse(position1 %in% c("vice president", "vp"), "vice president", position1)) %>%
  mutate(position1 = ifelse(position1 %in% c("dermatologist", "anesthesiologist", "surgeon"), "physician", position1)) %>%
  mutate(position1 = ifelse(position1=="chief financial officer", "cfo", position1)) %>%
  mutate(position1 = ifelse(position1=="chief medical officer", "cmo", position1)) %>%
  mutate(position2 = ifelse(position2 %in% c("president", "pres"), "president", position2)) %>%
  mutate(position2 = ifelse(position2 %in% c("ceo", "ceocfo", "chief executive officer"), "ceo", position2)) %>%
  mutate(position2 = ifelse(position2 %in% c("system executive", "medical director", "executive", "treasurer", "secretary", "vice chairman", "chairman", "vice chair", "chairperson", "chair", "vice-chair", "director", "trustee", "board"), "board", position2)) %>%
  mutate(position2 = ifelse(position2 %in% c("vice president", "vp"), "vice president", position2)) %>%
  mutate(position2 = ifelse(position2 %in% c("dermatologist", "anesthesiologist", "surgeon"), "physician", position2)) %>%
  mutate(position2 = ifelse(position2=="chief financial officer", "cfo", position2)) %>%
  mutate(position2 = ifelse(position2=="chief medical officer", "cmo", position2))

# fix year to reflect actual year instead of tax filing year
cleaned_text <- cleaned_text %>%
  mutate(year=year-1)

# get rid of physicians and board members, auxiliary presidents, and vice presidents
cleaned_text <- cleaned_text %>%
  filter(position1 %in% c("ceo", "president", "cfo", "cmo", "coo") | position2 %in% c("ceo", "president", "cfo", "cmo", "coo")) %>%
  filter(!str_detect(extra, "auxiliary|aux"))

# convert separate name variables to one 
cleaned_text <- cleaned_text %>%
  mutate(name = paste0(first_name, " ", last_name)) %>%
  select(-first_name, -last_name)

# get rid of those who are past officers
cleaned_text <- cleaned_text %>%
  filter(is.na(former))

# create one position variable
cleaned_text <- cleaned_text %>%
  mutate(position = ifelse(is.na(position2), position1, NA)) %>%
  mutate(position = ifelse(is.na(position) & (position1=="ceo" | position2=="ceo"), "ceo", position), 
         position = ifelse(is.na(position) & (position1=="president" | position2=="president"), "president", position), 
         position = ifelse(is.na(position) & (position1=="cfo" | position2=="cfo"), "cfo", position), 
         position = ifelse(is.na(position) & (position1=="cmo" | position2=="cmo"), "cmo", position), 
         position = ifelse(is.na(position) & (position1=="coo" | position2=="coo"), "coo", position)) %>%
  select(-position1, -position2) %>%
  distinct() 

# get rid of those with odd names or serve in auxiliary roles
cleaned_text <- cleaned_text %>%
  filter(!str_detect(extra, "auxiliary|senior vice|sr vice") | is.na(extra))

# impute names in years where they are listed before and after that year
# how can I do this when some hospitals have multiple people in the same poition???
# also what do I do with "part year" people?
multiples <- cleaned_text %>%
  group_by(ein, year, position) %>%
  mutate(count=1) %>%
  mutate(sum=sum(count)) %>%
  ungroup() %>%
  filter(sum>1)



# create an indicator for MD
cleaned_text <- cleaned_text %>%
  mutate(doctor = ifelse(title %in% c("md", "dr", "do"), 1, NA))
  # 7.6% of obs. have a doctor title

# first, fill in doctor grouping by ein, name
cleaned_text <- cleaned_text %>%
  group_by(ein, name) %>%
  fill(doctor, .direction="downup") %>%
  ungroup()
  # Now 8.1% of obs. have doctor title






# read in npi data of physician names
npidata <- readRDS(paste0(created_data_path, "npidata_names.rds"))

# Filter only to physicians
npidata <- npidata %>%
  filter(t_group=="Allopathic & Osteopathic Physicians")

# get rid of people who don't have names (I don't know why so many of them have NA for name??)
npidata <- npidata %>%
  filter(!(is.na(firstname) & is.na(lastname)))
  # still have a million observations (lost 300k)

# change enumeration date to enumeration year
npidata <- npidata %>%
  mutate(enum_date = as.Date(enum_date,'%m/%d/%Y')) %>%
  mutate(enum_year = as.numeric(format(enum_date,'%Y'))) %>%
  select(-enum_date)

# I don't need group or tax code anymore, just specialty
npidata <- npidata %>%
  select(-t_code, -t_group)

# convert first name and last name to one name line and make sure both are lowercase
npidata <- npidata %>%
  mutate(name = paste0(firstname, " ", lastname)) %>%
  mutate(name = tolower(name))

# look for the max number of one name occuring in npi data
npidata <- npidata %>%
  mutate(count=1) %>%
  group_by(name) %>%
  mutate(sum=sum(count)) %>%
  ungroup()
max <- max(npidata$sum)

# create a data set that will be used to store matches 
name_matches <- cleaned_text %>%
  distinct(ein, name) %>%
  mutate(num_matches=NA)

# create list of names in the NPPES data
nppes_list <- paste(as.list(npidata)[["name"]], collapse="|")

# First find out how many matches each name has
for (i in 1:dim(name_matches)[1]){
  
  match <- str_extract(nppes_list, paste0("(?<=\\|)(",name_matches$name[[i]],")(?=\\|)"))
  
  if (!is.na(match)) {
    match_info <- npidata %>%
      filter(name==match) %>%
      select(npi, name)
    
    num_matches <- dim(match_info)[1]
    
    name_matches$num_matches[[i]] <- num_matches
  }
  
  if (is.na(match)) {
    name_matches$num_matches[[i]] <- 0
  }
}

# join this to cleaned_text
cleaned_text <- cleaned_text %>%
  left_join(name_matches, by=c("name", "ein"))

# filter to sure matches to get specialty and npi
sure_matches <- cleaned_text %>%
  filter(doctor==1 & num_matches==1)

npidata <- npidata %>%
  select(npi, name, t_class)

sure_matches <- sure_matches %>%
  left_join(npidata, by=c("name"))

# join back to cleaned_text
cleaned_text <- cleaned_text %>%
  left_join(sure_matches) 
cleaned_text <- cleaned_text %>%
  distinct()

# create a variable that indicates the person is a potential MD: they didn't indicate being a doctor but their name is in NPPES at least once
cleaned_text <- cleaned_text %>%
  mutate(potential_doctor = ifelse(is.na(doctor) & num_matches>0,1,NA))

# change doctor to 0 if they don't claim to be a doctor and their name doesn't match anyone in NPPES
cleaned_text <- cleaned_text %>%
  mutate(doctor=ifelse(is.na(doctor) & num_matches==0,0,doctor))

# drop eins that have issues with names
cleaned_text <- cleaned_text %>%
  filter(ein!=10223482)

# create data only containing potential doctors
potential_doctors <- cleaned_text %>%
  filter(potential_doctor==1) %>%
  select(year, name, ein, position1, position2)

# I need to bring hospital name back into this to make googling easier
hosp_pdf_locations <- readRDS(paste0(created_data_path, "hospital_pdf_locations.rds"))

hosp_pdf_locations <- hosp_pdf_locations %>%
  distinct(ein, name) %>%
  rename(hosp_name=name)

potential_doctors <- potential_doctors %>%
  left_join(hosp_pdf_locations, by="ein")

potential_doctors <- potential_doctors %>%
  mutate(doctor=NA, specialty=NA, npi=NA)

# manually enter information about names after looking for them on google (including putting a zero if they are not a doctor)
unique_specialties <- unique(npidata$t_class)


potential_doctors <- potential_doctors %>%
  mutate(doctor = ifelse(name=="john carlson" & ein=="10130427",0,doctor),
         doctor = ifelse(name=="susan keiler" & ein=="10211551",0,doctor),
         doctor = ifelse(name=="michael lally" & ein=="10211783",0,doctor),
         doctor = ifelse(name=="arthur blank" & ein=="10211797",0,doctor),
         doctor = ifelse(name=="christina harding" & ein=="10211797",0,doctor),
         doctor = ifelse(name=="stuart cooper" & ein=="10215227",0,doctor),
         doctor = ifelse(name=="john welsh" & ein=="10215227",0,doctor),
         doctor = ifelse(name=="john dalton" & ein=="10217211",0,doctor),
         doctor = ifelse(name=="mary hood" & ein=="10217211",0,doctor),
         doctor = ifelse(name=="glenn martin" & ein=="10217211",0,doctor),
         doctor = ifelse(name=="john cox" & ein=="10219904",0,doctor),
         doctor = ifelse(name=="patricia cook" & ein=="10219904",0,doctor),
         doctor = ifelse(name=="timothy churchill" & ein=="10219904",0,doctor),
         doctor = ifelse(name=="ronald brown" & ein=="10223482",0,doctor),
         doctor = ifelse(name=="ronald daigle" & ein=="10234189",0,doctor),
         doctor = ifelse(name=="randall clark" & ein=="10263628",0,doctor),
         doctor = ifelse(name=="robert schlager" & ein=="10263628",1,doctor),
         npi = ifelse(name=="robert schlager" & ein=="10263628",1467432955,npi),
         specialty = ifelse(name=="robert schlager" & ein=="10263628","Family Medicine",specialty),
         doctor = ifelse(name=="stuart cooper" & ein=="10215227",0,doctor),
         doctor = ifelse(name=="john welsh" & ein=="10215227",0,doctor),
         doctor = ifelse(name=="john dalton" & ein=="10217211",0,doctor),
         doctor = ifelse(name=="mary hood" & ein=="10217211",0,doctor),
         doctor = ifelse(name=="glenn martin" & ein=="10217211",0,doctor),
         doctor = ifelse(name=="john cox" & ein=="10219904",0,doctor),
         doctor = ifelse(name=="patricia cook" & ein=="10219904",0,doctor)
  )
