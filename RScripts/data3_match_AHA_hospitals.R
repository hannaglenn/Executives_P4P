library(readr)
library(dplyr)
library(cdlTools)
library(stringr)
library(tidyr)

# This Script uses hospital names from the Nonprofit Explorer API and names from the AHA survey to match hospitals.

##### ORDER : 3 ##########

AHA <- read_csv(paste0(created_data_path, "/raw data/AHA_hosp_names.csv"))
tax <- readRDS(paste0(created_data_path, "hospital_pdf_locations.rds"))

AHA$MNAME <- iconv(AHA$MNAME,from="ISO-8859-1")

# Get AHA into cleaned data with one row per AHAID ####
# Limit to nonprofit general hospitals
AHA = AHA %>%
  filter(FSTCD<=56 & nchar(MLOCZIP)>4 & (CNTRL==23 | (CNTRL>=12 & CNTRL<=16)) & SERV==10) %>%
  dplyr::rename(name.AHA=MNAME) %>%
  mutate(state=fips(FSTCD, to='Abbreviation')) %>%
  mutate(name.AHA=tolower(name.AHA))

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


# take a look at hospitals who have changes in ownership or system name
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

no_matches <- match_data %>%
  filter(is.na(ein_hosp))
  # still missing 1700 matches. 


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

# now create a match data set where I add manual matches
manual_matches <- match_data %>%
  filter(is.na(ein_hosp)) %>%
  select(ID, state, name.AHA_1, name.AHA_2, name.AHA_3, name.AHA_4, zip_1, zip_2, zip_3, sys_1, sys_2, sys_3, sys_4, ein_hosp, ein_sys)

manual_matches <- manual_matches %>%
  mutate(ein_hosp = ifelse(ID=='6120080', 510491062, ein_hosp),
         ein_hosp = ifelse(ID=='6120190', 20222150, ein_hosp),
         ein_hosp = ifelse(ID=='6120380', 20222131, ein_hosp),
         ein_hosp = ifelse(ID=='6140465', 237000827, ein_hosp),
         ein_hosp = ifelse(ID=='6141890', 42103577, ein_hosp),
         ein_sys = ifelse(ID=='6141300', 912155626, ein_sys),
         ein_hosp = ifelse(ID=='6150001', 50259004, ein_hosp),
         ein_hosp = ifelse(ID=='6160013', 60646768, ein_hosp),
         ein_hosp = ifelse(ID=='6160650', 60646696, ein_hosp),
         ein_hosp = ifelse(ID=='6160750', 60665979, ein_hosp),
         ein_hosp = ifelse(ID=='6210058', 146049030, ein_hosp),
         ein_hosp = ifelse(ID=='6210400', 131740110, ein_hosp),
         ein_hosp = ifelse(ID=='6210740', 111986351, ein_hosp),
         ein_hosp = ifelse(ID=='6211180', 160743024, ein_hosp),
         ein_hosp = ifelse(ID=='6211290', 160743966, ein_hosp),
         ein_hosp = ifelse(ID=='6211470', 160743301, ein_hosp),
         ein_hosp = ifelse(ID=='6211500', 134111638, ein_hosp),
         ein_hosp = ifelse(ID=='6211545', 161165049, ein_hosp),
         ein_hosp = ifelse(ID=='6211690', 141338465, ein_hosp),
         ein_hosp = ifelse(ID=='6213830', 150539039, ein_hosp),
         ein_hosp = ifelse(ID=='6213870', 131725076, ein_hosp),
         ein_hosp = ifelse(ID=='6215260', 222807681, ein_hosp),
         ein_hosp = ifelse(ID=='6215330', 131740130, ein_hosp),
         ein_hosp = ifelse(ID=='6220280', 221487173, ein_hosp),
         ein_hosp = ifelse(ID=='6220322', 221750190, ein_hosp),
         ein_hosp = ifelse(ID=='6220425', 223693169, ein_hosp),
         ein_hosp = ifelse(ID=='6230019', 230794160, ein_hosp),
         ein_hosp = ifelse(ID=='6230022', 311538725, ein_hosp),
         ein_hosp = ifelse(ID=='6230040', 231352208, ein_hosp),
         ein_hosp = ifelse(ID=='6230041', 251054206, ein_hosp),
         ein_hosp = ifelse(ID=='6230270', 250965274, ein_hosp),
         ein_hosp = ifelse(ID=='6230297', 251865142, ein_hosp),
         ein_hosp = ifelse(ID=='6231345', 231534300, ein_hosp),
         ein_hosp = ifelse(ID=='6231460', 232720289, ein_hosp),
         ein_hosp = ifelse(ID=='6233100', 251801532, ein_hosp),
         ein_hosp = ifelse(ID=='6310120', 510103684, ein_hosp),
         ein_hosp = ifelse(ID=='6320020', 521169362, ein_hosp),
         ein_hosp = ifelse(ID=='6320420', 520679694, ein_hosp),
         ein_hosp = ifelse(ID=='6320670', 520619006, ein_hosp),
         ein_hosp = ifelse(ID=='6320735', 521372665, ein_hosp),
         ein_sys = ifelse(ID=='6340020'|ID=='6340030', 540620889, ein_sys),
         ein_hosp = ifelse(ID=='6340040', 540505989, ein_hosp),
         ein_hosp = ifelse(ID=='6340145', 237424835, ein_hosp),
         ein_hosp = ifelse(ID=='6340330', 521271901, ein_hosp),
         ein_hosp = ifelse(ID=='6340360', 540505913, ein_hosp),
         ein_hosp = ifelse(ID=='6340454', 540696355, ein_hosp),
         ein_hosp = ifelse(ID=='6350555', 237441353, ein_hosp),
         ein_hosp = ifelse(ID=='6350710', 550404900, ein_hosp),
         ein_hosp = ifelse(ID=='6350800', 550422958, ein_hosp),
         ein_hosp = ifelse(ID=='6360001', 566017737, ein_hosp),
         ein_hosp = ifelse(ID=='6360012', 561376368, ein_hosp),
         ein_hosp = ifelse(ID=='6360021', 562112733, ein_hosp),
         ein_hosp = ifelse(ID=='6360355', 562070036, ein_hosp),
         ein_hosp = ifelse(ID=='6360920', 560543238, ein_hosp),
         ein_hosp = ifelse(ID=='6361340', 560547479, ein_hosp),
         ein_hosp = ifelse(ID=='6361375', 560642846, ein_hosp),
         ein_hosp = ifelse(ID=='6361686', 561340424, ein_hosp),
         ein_hosp = ifelse(ID=='6370375', 570873845, ein_hosp),
         ein_hosp = ifelse(ID=='6370640', 570343398, ein_hosp),
         ein_hosp = ifelse(ID=='6380020', 582224545, ein_hosp),
         ein_hosp = ifelse(ID=='6380360', 262037695, ein_hosp),
         ein_hosp = ifelse(ID=='638080A', 580593388, ein_hosp),
         ein_hosp = ifelse(ID=='6380885', 582510435, ein_hosp),
         ein_hosp = ifelse(ID=='6380950', 581973570, ein_hosp),
         ein_hosp = ifelse(ID=='6381170', 586001667, ein_hosp),
         ein_hosp = ifelse(ID=='6381215', 581790149, ein_hosp),
         ein_hosp = ifelse(ID=='6381225', 711045290, ein_hosp),
         ein_hosp = ifelse(ID=='6389065', 586025393, ein_hosp),
         ein_hosp = ifelse(ID=='6390050', 592447554, ein_hosp),
         ein_hosp = ifelse(ID=='6390200', 593234721, ein_hosp),
         ein_hosp = ifelse(ID=='6390499', 592319288, ein_hosp),
         ein_hosp = ifelse(ID=='6390640', 590624424, ein_hosp),
         ein_hosp = ifelse(ID=='6390669', 592314655, ein_hosp),
         ein_hosp = ifelse(ID=='6410012', 341887844, ein_hosp),
         ein_hosp = ifelse(ID=='6410015', 344428218, ein_hosp),
         ein_hosp = ifelse(ID=='6410012', 341887844, ein_hosp),
         ein_hosp = ifelse(ID=='6410015', 344428218, ein_hosp), 
         ein_hosp = ifelse(ID=='6410217', 344440884, ein_hosp),
         ein_hosp = ifelse(ID=='6410382', 310537122, ein_hosp),
         ein_hosp = ifelse(ID=='6411370', 311156690, ein_hosp),
         ein_hosp = ifelse(ID=='6411375', 341408846, ein_hosp),
         ein_hosp = ifelse(ID=='6411400', 344428598, ein_hosp),
         ein_hosp = ifelse(ID=='6411440', 311765550, ein_hosp),
         ein_hosp = ifelse(ID=='6411540', 341883284, ein_hosp),
         ein_hosp = ifelse(ID=='6411579', 311458827, ein_hosp),
         ein_hosp = ifelse(ID=='6411870', 341425870, ein_hosp),
         ein_hosp = ifelse(ID=='351955872', 351955872, ein_hosp),
         ein_hosp = ifelse(ID=='6420130', 351720796, ein_hosp),
         ein_hosp = ifelse(ID=='6420210', 350900741, ein_hosp),
         ein_hosp = ifelse(ID=='6420605', 350983617, ein_hosp),
         ein_hosp = ifelse(ID=='6420728', 351088640, ein_hosp),
         ein_hosp = ifelse(ID=='6420870', 474673365, ein_hosp),
         ein_hosp = ifelse(ID=='6430337', 370645239, ein_hosp),
         ein_hosp = ifelse(ID=='6431927', 363637465, ein_hosp),
         ein_hosp = ifelse(ID=='6431970', 370661230, ein_hosp),
         ein_hosp = ifelse(ID=='6432000', 362174832, ein_hosp),
         ein_hosp = ifelse(ID=='6432615', 370681540, ein_hosp),
         ein_hosp = ifelse(ID=='6432640', 371396010, ein_hosp),
         ein_hosp = ifelse(ID=='6440015', 383236977, ein_hosp),
         ein_hosp = ifelse(ID=='6441011', 381426919, ein_hosp),
         ein_hosp = ifelse(ID=='6441231', 380593405, ein_hosp),
         ein_hosp = ifelse(ID=='6441350', 381619577, ein_hosp),
         ein_hosp = ifelse(ID=='6441360', 382800065, ein_hosp),
         ein_hosp = ifelse(ID=='6441450', 382908586, ein_hosp),
         ein_hosp = ifelse(ID=='6441595', 381360584, ein_hosp),
         ein_hosp = ifelse(ID=='6441830', 382947657, ein_hosp),
         ein_hosp = ifelse(ID=='6442327', 381738615, ein_hosp),
         ein_hosp = ifelse(ID=='6442410', 382317300, ein_hosp),
         ein_hosp = ifelse(ID=='6450330', 390807060, ein_hosp),
         ein_hosp = ifelse(ID=='6451300', 390806828, ein_hosp),
         ein_hosp = ifelse(ID=='6451370', 390848401, ein_hosp),
         ein_hosp = ifelse(ID=='6451630', 390837206, ein_hosp),
         ein_hosp = ifelse(ID=='6451840', 390832914, ein_hosp),
         ein_hosp = ifelse(ID=='6452020', 390910727, ein_hosp),
         ein_hosp = ifelse(ID=='6510049', 610601267, ein_hosp),
         ein_hosp = ifelse(ID=='6510255', 204474637, ein_hosp),
         ein_hosp = ifelse(ID=='6510266', 610525158, ein_hosp),
         ein_hosp = ifelse(ID=='6510350', 452696517, ein_hosp),
         ein_hosp = ifelse(ID=='6510550', 611293786, ein_hosp),
         ein_hosp = ifelse(ID=='6510745', 610523304, ein_hosp),
         ein_hosp = ifelse(ID=='6520785', 620479367, ein_hosp),
         ein_hosp = ifelse(ID=='6520895', 620545814, ein_hosp),
         ein_hosp = ifelse(ID=='6530142', 630754793, ein_hosp),
         ein_hosp = ifelse(ID=='6540170', 640926753, ein_hosp),
         ein_hosp = ifelse(ID=='6540235', 640770155, ein_hosp),
         ein_hosp = ifelse(ID=='6540580', 640655993, ein_hosp),
         ein_hosp = ifelse(ID=='6610097', 410841441, ein_hosp),
         ein_hosp = ifelse(ID=='6610400', 410714079, ein_hosp),
         ein_hosp = ifelse(ID=='6610590', 410724034, ein_hosp),
         ein_hosp = ifelse(ID=='6610810', 411865315, ein_hosp),
         ein_hosp = ifelse(ID=='6611585', 270052697, ein_hosp),
         ein_hosp = ifelse(ID=='6611860', 205617275, ein_hosp),
         ein_hosp = ifelse(ID=='6612090', 410713914, ein_hosp),
         ein_hosp = ifelse(ID=='6620300', 420680355, ein_hosp),
         ein_hosp = ifelse(ID=='6620670', 420738969, ein_hosp),
         ein_hosp = ifelse(ID=='6621202', 420932564, ein_hosp),
         ein_hosp = ifelse(ID=='6629060', 420710268, ein_hosp),
         ein_hosp = ifelse(ID=='6630230', 430662495, ein_hosp),
         ein_hosp = ifelse(ID=='6630234', 436004544, ein_hosp),
         ein_hosp = ifelse(ID=='6630650', 440655986, ein_hosp),
         ein_hosp = ifelse(ID=='6630788', 431741457, ein_hosp),
         ein_hosp = ifelse(ID=='6631320', 440577118, ein_hosp),
         ein_hosp = ifelse(ID=='6631378', 436005776, ein_hosp),
         ein_hosp = ifelse(ID=='6640003', 261175213, ein_hosp),
         ein_hosp = ifelse(ID=='6640004', 331007002, ein_hosp),
         ein_hosp = ifelse(ID=='6640055', 450458242, ein_hosp),
         ein_hosp = ifelse(ID=='6640062', 456013474, ein_hosp),
         ein_hosp = ifelse(ID=='6640267', 450340688, ein_hosp),
         ein_hosp = ifelse(ID=='6640383', 450232743, ein_hosp),
         ein_hosp = ifelse(ID=='6640475', 450358986, ein_hosp),
         ein_hosp = ifelse(ID=='6650080', 460246437, ein_hosp),
         ein_hosp = ifelse(ID=='6650160', 460450523, ein_hosp),
         ein_hosp = ifelse(ID=='6650345', 460239781, ein_hosp),
         ein_hosp = ifelse(ID=='6659005', 460380552, ein_hosp),
         ein_hosp = ifelse(ID=='6660112', 470426285, ein_hosp),
         ein_hosp = ifelse(ID=='6670013', 481140505, ein_hosp),
         ein_hosp = ifelse(ID=='6670050', 480561974, ein_hosp),
         ein_hosp = ifelse(ID=='6670325', 481226833, ein_hosp),
         ein_hosp = ifelse(ID=='6670366', 480577658, ein_hosp),
         ein_hosp = ifelse(ID=='6670515', 480761700, ein_hosp),
         ein_hosp = ifelse(ID=='6670692', 481226856, ein_hosp),
         ein_hosp = ifelse(ID=='6670913', 481226830, ein_hosp),
         ein_hosp = ifelse(ID=='6671210', 486005089, ein_hosp),
         ein_hosp = ifelse(ID=='6710040', 710411459, ein_hosp),
         ein_hosp = ifelse(ID=='6710045', 710772959, ein_hosp),
         ein_hosp = ifelse(ID=='6710100', 710403278, ein_hosp),
         ein_hosp = ifelse(ID=='6710157', 954896822, ein_hosp),
         ein_hosp = ifelse(ID=='6720217', 720491106, ein_hosp),
         ein_hosp = ifelse(ID=='6730106', 10603214, ein_hosp),
         ein_hosp = ifelse(ID=='6730110', 731506316, ein_hosp),
         ein_hosp = ifelse(ID=='6730187', 263778478, ein_hosp),
         ein_hosp = ifelse(ID=='6741690', 741548089, ein_hosp),
         ein_hosp = ifelse(ID=='6742045', 203069241, ein_hosp),
         ein_hosp = ifelse(ID=='6742135', 742557820, ein_hosp),
         ein_hosp = ifelse(ID=='6743189', 272814620, ein_hosp),
         ein_hosp = ifelse(ID=='6810040', 810232121, ein_hosp),
         ein_hosp = ifelse(ID=='6810100', 810286525, ein_hosp),
         ein_hosp = ifelse(ID=='6810123', 810373589, ein_hosp),
         ein_hosp = ifelse(ID=='6810129', 810469886, ein_hosp),
         ein_hosp = ifelse(ID=='6810150', 810264548, ein_hosp),
         ein_hosp = ifelse(ID=='6810160', 810405434, ein_hosp),
         ein_hosp = ifelse(ID=='6810380', 237169043, ein_hosp),
         ein_hosp = ifelse(ID=='6810405', 816016152, ein_hosp),
         ein_hosp = ifelse(ID=='6810481', 810221486, ein_hosp),
         ein_hosp = ifelse(ID=='6820111', 814065632, ein_hosp),
         ein_hosp = ifelse(ID=='6820212', 273311774, ein_hosp),
         ein_hosp = ifelse(ID=='6840018', 841262971, ein_hosp),
         ein_hosp = ifelse(ID=='6840940', 840586742, ein_hosp),
         ein_hosp = ifelse(ID=='6840950', 261167922, ein_hosp),
         ein_hosp = ifelse(ID=='6850140', 850442957, ein_hosp),
         ein_hosp = ifelse(ID=='6860374', 860171900, ein_hosp),
         ein_hosp = ifelse(ID=='6864000', 860334996, ein_hosp),
         ein_hosp = ifelse(ID=='6880050', 880252723, ein_hosp),
         ein_hosp = ifelse(ID=='6910669', 910637400, ein_hosp),
         ein_hosp = ifelse(ID=='6920070', 930602940, ein_hosp),
         ein_hosp = ifelse(ID=='6930119', 953782169, ein_hosp),
         ein_hosp = ifelse(ID=='6930120', 952477294, ein_hosp),
         ein_hosp = ifelse(ID=='6932010', 951816005, ein_hosp),
         ein_hosp = ifelse(ID=='6933310', 942637032, ein_hosp),
         ein_hosp = ifelse(ID=='6940010', 920162721, ein_hosp),
         ein_hosp = ifelse(ID=='6940040', 920041414, ein_hosp))

manual_matches <- manual_matches %>%
  filter(!is.na(ein_hosp))
manual_matches <- manual_matches %>%
  select(ID, ein_hosp ,ein_sys)
saveRDS(manual_matches, paste0(created_data_path, "manual_matched_eins.rds"))



