library(readr)
library(dplyr)
library(stringr)
library(tidyr)

source("paths.R")

# read in names data
names_data <- read_rds(paste0(created_data_path, "names_data.rds"))

# create an indicator for MD
names_data <- names_data %>%
  mutate(doctor = ifelse(title %in% c("md", "dr", "do"), 1, NA))
  # 3.6% of people have a doctor title


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
  # 136

# create a data set that will be used to store matches 
names_data <- names_data %>%
  distinct(ein, name, title, doctor, years) %>%
  mutate(num_matches=NA)

# create list of names in the NPPES data
nppes_list <- paste(as.list(npidata)[["name"]], collapse="|")

# First find out how many matches each name has
for (i in 1:dim(names_data)[1]){
  
  match <- str_extract(nppes_list, paste0("(?<=\\|)(",names_data$name[[i]],")(?=\\|)"))
  
  if (!is.na(match)) {
    match_info <- npidata %>%
      filter(name==match) %>%
      select(npi, name)
    
    num_matches <- dim(match_info)[1]
    
    names_data$num_matches[[i]] <- num_matches
  }
  
  if (is.na(match)) {
    names_data$num_matches[[i]] <- 0
  }
}

# filter to sure matches to get specialty and npi
sure_matches <- names_data %>%
  filter(doctor==1 & num_matches==1) %>%
  select(name, ein)
  # 76 people

npidata <- npidata %>%
  select(npi, name, t_class)

sure_matches <- sure_matches %>%
  left_join(npidata, by=c("name"))

# join back to cleaned_text
names_data <- names_data %>%
  left_join(sure_matches, by=c("name", "ein")) 

# we don't need names of those who say they aren't a doctor and don't match
# set doctor to zero if they are an RN or they don't have a title + num_matches=0
names_data <- names_data %>%
  mutate(doctor = ifelse(title=="rn",0,doctor)) %>%
  mutate(doctor = ifelse(num_matches==0 & is.na(title), 0, doctor))

# only keep the observations we are unsure about
unsure_names_data <- names_data %>%
  filter(is.na(doctor) | (doctor==1 & is.na(npi))) %>%
  filter(years!="2008")

unsure_names_data_RAs <- names_data %>%
  filter(is.na(doctor)) %>%
  select(ein, name, years)

hosp_names <- hospital_pdf_locations %>%
  distinct(ein, name, sort_name, state) %>%
  rename(hosp_name = name)

unsure_names_data_RAs <- unsure_names_data_RAs %>%
  mutate(ein = as.numeric(ein)) %>%
  left_join(hosp_names, by="ein")

write.csv(unsure_names_data_RAs, paste0(created_data_path, "unsure_names_data.csv"))


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
