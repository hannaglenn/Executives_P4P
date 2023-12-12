library(readr)
library(dplyr)
library(cdlTools)
library(stringr)
library(tidyr)

# This Script uses hospital names from the Nonprofit Explorer API and names from the AHA survey to match hospitals.

##### ORDER : 3 ##########

AHA <- read_csv(paste0(created_data_path, "/raw data/AHA_hosp_names.csv"))
tax <- readRDS(paste0(created_data_path, "hospital_pdf_locations.rds"))

# Get AHA into cleaned data with one row per AHAID ####
# Limit to nonprofit general hospitals
AHA = AHA %>%
  filter(FSTCD<=56 & nchar(MLOCZIP)>4 & (CNTRL==23 | (CNTRL>=12 & CNTRL<=16)) & SERV==10) %>%
  dplyr::rename(name.AHA=MNAME) 
  mutate(name.AHA=tolower(`name.AHA`)) %>%
  mutate(state=fips(FSTCD, to='Abbreviation')) 


AHA <- separate(AHA, MLOCZIP, into=c('zip','extra'), sep="-", extra='merge', remove=FALSE) %>%
  select(-extra) %>%
  mutate(zip=as.double(zip))

# get rid of any AHAID that wasn't present from 2009-2015 at least
AHA <- AHA %>%
  mutate(count=ifelse(YEAR>=2009 & YEAR<=2015,1,0)) %>%
  group_by(ID) %>%
  mutate(sum=sum(count)) %>%
  ungroup() %>%
  filter(sum>5) %>%
  filter(YEAR>=2008 & YEAR<=2019) %>%
  filter(!str_detect(name.AHA, "cancer"))

# take a look at hospitals who have changes in ownership or name
AHA <- AHA %>%
  group_by(ID) %>%
  mutate(lag_sys=lag(SYSID)) %>%
  ungroup() %>%
  mutate(change=ifelse(lag_sys!=SYSID,1,NA)) %>%
  group_by(ID) %>%
  fill(change, .direction="downup") %>%
  ungroup() 

sys_changes <- AHA %>%
  filter(change==1)
no_sys_changes <- AHA %>%
  filter(is.na(change))

num_hosp <- no_sys_changes %>%
  distinct(ID)

# create data where each AHAID has one row that collects all names, zip codes, and system names (wide format)
data_AHA_name <- no_sys_changes %>%
  filter(YEAR!=2008) %>%
  distinct(ID, name.AHA, state)
data_AHA_name <- data_AHA_name %>%
  group_by(ID) %>%
  mutate(num=row_number()) %>%
  ungroup()
data_AHA_name <- pivot_wider(data=data_AHA_name,
                        names_from=num, 
                        values_from=c(name.AHA),
                        id_cols=c(ID, state)) %>%
  rename(name.AHA_1=`1`, name.AHA_2=`2`, name.AHA_3=`3`, name.AHA_4=`4`) %>%
  select(-`5`)

data_AHA_zip <- no_sys_changes %>%
  filter(YEAR!=2008) %>%
  distinct(ID, zip, state)
data_AHA_zip <- data_AHA_zip %>%
  group_by(ID) %>%
  mutate(num=row_number()) %>%
  ungroup()
data_AHA_zip <- pivot_wider(data=data_AHA_zip,
                             names_from=num, 
                             values_from=c(zip),
                             id_cols=c(ID, state)) %>%
  rename(zip_1=`1`, zip_2=`2`, zip_3=`3`) 

data_AHA_sys <- no_sys_changes %>%
  filter(YEAR!=2008) %>%
  distinct(ID, SYSNAME, state)
data_AHA_sys <- data_AHA_sys %>%
  group_by(ID) %>%
  filter(!is.na(SYSNAME)) %>%
  mutate(num=row_number()) %>%
  ungroup()
data_AHA_sys <- pivot_wider(data=data_AHA_sys,
                            names_from=num, 
                            values_from=c(SYSNAME),
                            id_cols=c(ID, state)) %>%
  rename(sys_1=`1`, sys_2=`2`, sys_3=`3`, sys_4=`4`) 

data_AHA <- data_AHA_name %>%
  left_join(data_AHA_zip, by=c("ID","state")) %>%
  left_join(data_AHA_sys, by=c("ID","state"))

rm(data_AHA_name, data_AHA_sys, data_AHA_zip)



# Clean Tax Data ####

tax <- tax %>%
  rename(name.tax=name) %>%
  mutate(name.tax=tolower(name.tax),
         sort_name=tolower(sort_name)) %>%
  rename(name.tax2 = sort_name)

# get rid of foundations and auxiliaries in tax data
tax <- tax %>%
  filter(!str_detect(name.tax,"foundation|auxiliary|auxilliary|cancer|hospice|mental|geriatric|cardiology|associates|nurse|eye|veterinary")) %>%
  filter(!str_detect(name.tax2,"foundation|auxiliary|auxilliary|cancer|hospice|mental|geriatric|cardiology|associates|nurse|eye|veterinary") | is.na(name.tax2)) %>%
  mutate(clinic=ifelse(str_detect(name.tax,"clinic"),1,0),
         hospital=ifelse(str_detect(name.tax,"hospital"),1,0)) %>%
  filter(!(clinic==1 & hospital==0)) %>%
  select(-clinic, -hospital)
# got rid of around 13,000 observations

tax <- separate(tax, zipcode, into=c('zip','extra'), sep="-", extra='merge', remove=FALSE) %>%
  select(-extra) %>%
  mutate(zip=as.double(zip))

tax <- tax %>%
  distinct(ein, name.tax, state, name.tax2, zip) %>%
  mutate(count=1) %>%
  group_by(name.tax, name.tax2, state) %>%
  mutate(sum=sum(count)) %>%
  ungroup() %>%
  filter(sum==1) %>%
  select(-sum, -count)



# Create a data set that will be used to store matches
match_data <- data_AHA %>%
  mutate(name.tax_hosp=NA,
         name.tax_sys=NA,
         ein_hosp=NA,
         ein_sys=NA,
         zip.tax_hosp=NA,
         zip.tax_sys=NA)



# Find any exact matches of hospital names ####
# get rid of common differences 
match_data <- match_data %>%
  mutate(name.AHA1_spaceless=name.AHA_1,
         name.AHA2_spaceless=name.AHA_2,
         name.AHA3_spaceless=name.AHA_3,
         name.AHA4_spaceless=name.AHA_4)
match_data$name.AHA1_spaceless=str_remove_all(match_data$name.AHA1_spaceless,"\\s|,|inc|\\.|'|&|-")
match_data$name.AHA2_spaceless=str_remove_all(match_data$name.AHA2_spaceless,"\\s|,|inc|\\.|'|&|-")
match_data$name.AHA3_spaceless=str_remove_all(match_data$name.AHA3_spaceless,"\\s|,|inc|\\.|'|&|-")
match_data$name.AHA4_spaceless=str_remove_all(match_data$name.AHA4_spaceless,"\\s|,|inc|\\.|'|&|-")
tax <- tax %>%
  mutate(name.tax1_spaceless=name.tax,
         name.tax2_spaceless=name.tax2)
tax$name.tax1_spaceless <- str_remove_all(tax$name.tax1_spaceless,"\\s|,|inc|\\.|&|incorporated|associates|corporation|association")
tax$name.tax2_spaceless <- str_remove_all(tax$name.tax2_spaceless,"\\s|,|inc|\\.|&|incorporated|associates|corporation|association")

# Finding exact matches in any of the names from AHA or tax (in the same state)
for (i in 1:dim(match_data)[1]){
  st <- match_data$state[[i]]
  
  list_tax <- tax %>%
    filter(state==st)
  list_tax1 <- paste(as.list(list_tax)[["name.tax1_spaceless"]], collapse="|")
  list_tax2 <- paste(as.list(list_tax)[["name.tax2_spaceless"]], collapse="|")
  list_tax <- paste0("|", list_tax1,"|", list_tax2, "|", collapse="|")
  
  match <- str_extract(list_tax, paste0("(?<=\\|)(",match_data$name.AHA1_spaceless[[i]],")(?=\\|)"))
  if (is.na(match) & !is.na(match_data$name.AHA2_spaceless[[i]])) {
    match <- str_extract(list_tax, paste0("(?<=\\|)(",match_data$name.AHA2_spaceless[[i]],")(?=\\|)"))
  }
  if (is.na(match) & !is.na(match_data$name.AHA3_spaceless[[i]])) {
    match <- str_extract(list_tax, paste0("(?<=\\|)(",match_data$name.AHA3_spaceless[[i]],")(?=\\|)"))
  }
  if (is.na(match) & !is.na(match_data$name.AHA4_spaceless[[i]])) {
    match <- str_extract(list_tax, paste0("(?<=\\|)(",match_data$name.AHA4_spaceless[[i]], ")(?=\\|)"))
  }
  
  if (!is.na(match)) {
    match_info <- tax %>%
      filter((name.tax1_spaceless==match | name.tax2_spaceless==match) & state==st) %>%
      mutate(one=ifelse(name.tax1_spaceless==match,1,0))
    
    if (match_info$one[1]==1){
    match_data$name.tax_hosp[i] <- match_info$name.tax[1]
    }
    if (match_info$one[1]==0){
      match_data$name.tax_hosp[i] <- match_info$name.tax2[1]
    }
    
    match_data$ein_hosp[i] <- match_info$ein[1]
    match_data$zip.tax_hosp[i] <- match_info$zip[1]
  }
}
  # 31% of hospitals matched using exact names. equivalent to 860 hospitals.

# now lets get rid of common words and look for exact matches again
match_data$name.AHA1_spaceless=str_remove_all(match_data$name.AHA_1,"\\bincorporated\\b|signature\\b|healthcare\\b|\\bhealth center\\b|\\bregional\\b|\\bhospital\\b|\\bhealth systems\\b|\\bthe\\b|\\bhealth network\\b|\\bmedical center\\b|\\bhcsr\\b|\\band\\b|\\bctr\\b|,|inc|\\.|'|&|-|\\bhealth\\b")
match_data$name.AHA1_spaceless=str_remove_all(match_data$name.AHA1_spaceless,"\\s")
match_data$name.AHA2_spaceless=str_remove_all(match_data$name.AHA_2,"\\bincorporated\\b|signature\\b|healthcare\\b|\\bhealth center\\b|\\bregional\\b|\\bhospital\\b|\\bhealth systems\\b|\\bthe\\b|\\bhealth network\\b|\\bmedical center\\b|\\bhcsr\\b|\\band\\b|\\bctr\\b|,|inc|\\.|'|&|-|\\bhealth\\b")
match_data$name.AHA2_spaceless=str_remove_all(match_data$name.AHA2_spaceless,"\\s")
match_data$name.AHA3_spaceless=str_remove_all(match_data$name.AHA_3,"\\bincorporated\\b|signature\\b|healthcare\\b|\\bhealth center\\b|\\bregional\\b|\\bhospital\\b|\\bhealth systems\\b|\\bthe\\b|\\bhealth network\\b|\\bmedical center\\b|\\bhcsr\\b|\\band\\b|\\bctr\\b|,|inc|\\.|'|&|-|\\bhealth\\b")
match_data$name.AHA3_spaceless=str_remove_all(match_data$name.AHA3_spaceless,"\\s")
match_data$name.AHA4_spaceless=str_remove_all(match_data$name.AHA_4,"\\bincorporated\\b|signature\\b|healthcare\\b|\\bhealth center\\b|\\bregional\\b|\\bhospital\\b|\\bhealth systems\\b|\\bthe\\b|\\bhealth network\\b|\\bmedical center\\b|\\bhcsr\\b|\\band\\b|\\bctr\\b|,|inc|\\.|'|&|-|\\bhealth\\b")
match_data$name.AHA4_spaceless=str_remove_all(match_data$name.AHA4_spaceless,"\\s")

tax$name.tax1_spaceless=str_remove_all(tax$name.tax,"\\bincorporated\\b|signature\\b|healthcare\\b|\\bhealth center\\b|\\bassociation\\b|\\bcorporation\\b|\\bassociates\\b|\\bregional\\b|\\bhospital\\b|\\bhealth systems\\b|\\bthe\\b|\\bhealth network\\b|\\bmedical center\\b|\\bhcsr\\b|\\band\\b|\\bctr\\b|,|inc|\\.|'|&|-|\\bhealth\\b")
tax$name.tax1_spaceless=str_remove_all(tax$name.tax1_spaceless,"\\s")
tax$name.tax2_spaceless=str_remove_all(tax$name.tax2,"\\bincorporated\\b|signature\\b|healthcare\\b|\\bhealth center\\b|\\bassociation\\b|\\bcorporation\\b|\\bassociates\\b|\\bregional\\b|\\bhospital\\b|\\bhealth systems\\b|\\bthe\\b|\\bhealth network\\b|\\bmedical center\\b|\\bhcsr\\b|\\band\\b|\\bctr\\b|,|inc|\\.|'|&|-|\\bhealth\\b")
tax$name.tax2_spaceless=str_remove_all(tax$name.tax2_spaceless,"\\s")

# Finding exact matches in any of the names from AHA or tax (in the same state)
for (i in 1:dim(match_data)[1]){
  if (is.na(match_data$name.tax_hosp[[i]])){
  st <- match_data$state[[i]]
  
  list_tax <- tax %>%
    filter(state==st)
  list_tax1 <- paste(as.list(list_tax)[["name.tax1_spaceless"]], collapse="|")
  list_tax2 <- paste(as.list(list_tax)[["name.tax2_spaceless"]], collapse="|")
  list_tax <- paste0("|", list_tax1,"|", list_tax2, "|", collapse="|")
  
  match <- str_extract(list_tax, paste0("(?<=\\|)(",match_data$name.AHA1_spaceless[[i]],")(?=\\|)"))
  if (is.na(match) & !is.na(match_data$name.AHA2_spaceless[[i]])) {
    match <- str_extract(list_tax, paste0("(?<=\\|)(",match_data$name.AHA2_spaceless[[i]],")(?=\\|)"))
  }
  if (is.na(match) & !is.na(match_data$name.AHA3_spaceless[[i]])) {
    match <- str_extract(list_tax, paste0("(?<=\\|)(",match_data$name.AHA3_spaceless[[i]],")(?=\\|)"))
  }
  if (is.na(match) & !is.na(match_data$name.AHA4_spaceless[[i]])) {
    match <- str_extract(list_tax, paste0("(?<=\\|)(",match_data$name.AHA4_spaceless[[i]], ")(?=\\|)"))
  }
  
  if (!is.na(match)) {
    match_info <- tax %>%
      filter((name.tax1_spaceless==match | name.tax2_spaceless==match) & state==st) %>%
      mutate(one=ifelse(name.tax1_spaceless==match,1,0))
    
    if (match_info$one[1]==1){
      match_data$name.tax_hosp[i] <- match_info$name.tax[1]
    }
    if (match_info$one[1]==0){
      match_data$name.tax_hosp[i] <- match_info$name.tax2[1]
    }
    
    match_data$ein_hosp[i] <- match_info$ein[1]
    match_data$zip.tax_hosp[i] <- match_info$zip[1]
  }
  }
}
# 34.5% of hospitals matched using exact names. equivalent to 950 hospitals.









# Find any exact matches of system names ####
match_data <- match_data %>%
  mutate(sys1_spaceless=tolower(sys_1),
         sys2_spaceless=tolower(sys_2),
         sys3_spaceless=tolower(sys_3),
         sys4_spaceless=tolower(sys_4))
match_data$sys1_spaceless=str_remove_all(match_data$sys1_spaceless,"\\s|,|inc|\\.|'|&|-")
match_data$sys2_spaceless=str_remove_all(match_data$sys2_spaceless,"\\s|,|inc|\\.|'|&|-")
match_data$sys3_spaceless=str_remove_all(match_data$sys3_spaceless,"\\s|,|inc|\\.|'|&|-")
match_data$sys4_spaceless=str_remove_all(match_data$sys4_spaceless,"\\s|,|inc|\\.|'|&|-")

tax <- tax %>%
  mutate(name.tax1_spaceless=name.tax,
         name.tax2_spaceless=name.tax2)
tax$name.tax1_spaceless <- str_remove_all(tax$name.tax1_spaceless,"\\s|,|inc|\\.|&|incorporated|associates|corporation|association")
tax$name.tax2_spaceless <- str_remove_all(tax$name.tax2_spaceless,"\\s|,|inc|\\.|&|incorporated|associates|corporation|association")


for (i in 1:dim(match_data)[1]){
  if (!is.na(match_data$sys_1[[i]])){
    st <- match_data$state[[i]]
    
    list_tax <- tax %>%
      filter(state==st & (is.na(name.tax2) | name.tax2=="groupreturn"))
    list_tax1 <- paste(as.list(list_tax)[["name.tax1_spaceless"]], collapse="|")
    list_tax2 <- paste(as.list(list_tax)[["name.tax2_spaceless"]], collapse="|")
    list_tax <- paste0("|", list_tax1, list_tax2, "|", collapse="|")
    
    match <- str_extract(list_tax, paste0("(?<=\\|)(",match_data$sys1_spaceless[[i]],")(?=\\|)"))
    if (is.na(match) & !is.na(match_data$sys2_spaceless[[i]])) {
      match <- str_extract(list_tax, paste0("(?<=\\|)(",match_data$sys2_spaceless[[i]],")(?=\\|)"))
    }
    if (is.na(match) & !is.na(match_data$sys3_spaceless[[i]])) {
      match <- str_extract(list_tax, paste0("(?<=\\|)(",match_data$sys3_spaceless[[i]],")(?=\\|)"))
    }
    if (is.na(match) & !is.na(match_data$sys4_spaceless[[i]])) {
      match <- str_extract(list_tax, paste0("(?<=\\|)(",match_data$sys4_spaceless[[i]], ")(?=\\|)"))
    }
    
    if (!is.na(match) & match!="NA") {
      match_info <- tax %>%
        filter((name.tax1_spaceless==match | name.tax2_spaceless==match) & state==st) %>%
        mutate(one=ifelse(name.tax1_spaceless==match,1,0))
      
      if (match_info$one[1]==1){
        match_data$name.tax_sys[i] <- match_info$name.tax[1]
      }
      if (match_info$one[1]==0){
        match_data$name.tax_sys[i] <- match_info$name.tax2[1]
      }
      
      match_data$ein_sys[i] <- match_info$ein[1]
      match_data$zip.tax_sys[i] <- match_info$zip[1]
    }
  }
}
  # 41% total have hospital match, system match, or both. (1136 hospitals)

# save any distinct eins in this data to download from NonProfit Explorer
ein1 <- match_data %>%
  distinct(ein_hosp) %>%
  rename(ein=ein_hosp)
ein2 <- match_data %>%
  distinct(ein_sys) %>%
  rename(ein=ein_sys)
ein_list <- rbind(ein1, ein2) %>%
  filter(!is.na(ein))
saveRDS(ein_list, paste0(created_data_path, "/einlist_AHAmatching_1.rds"))


# save AHA IDs matched to hospital and system eins
AHA_ein_matches <- match_data %>%
  select(ID, ein_hosp, ein_sys) %>%
  filter(!is.na(ein_hosp) | !is.na(ein_sys))

saveRDS(AHA_ein_matches, paste0(created_data_path, "/AHA_ein_matches.rds"))




