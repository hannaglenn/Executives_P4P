library(readr)
library(dplyr)
library(tidyr)
library(stringr)

# Read in Data ##################

# Data on which hospitals in the tax data have AHA matches
AHA_ein_matches <- read_rds(paste0(created_data_path,"AHA_ein_matches.rds"))
manual_matched_eins <- read_rds(paste0(created_data_path, "manual_matched_eins.rds"))
# Data from AHA 
AHA_hosp_names <- read_csv(paste0(created_data_path, "raw data/AHA_hosp_names.csv"))
# HCRIS data that has HRRP penalty information (and beds)
# final_HCRIS_v2010 <- read_csv(paste0(created_data_path, "raw data/final_HCRIS_v2010.csv"))
# final_HCRIS_v1996 <- read_csv(paste0(created_data_path, "raw data/final_HCRIS_v1996.csv"))
# Hospital Compare data that also has penalty information
hc_readm_2012 <- read_csv(paste0(created_data_path, "raw data/Hospital Compare/vwhqi_readm_reduction_2012.csv"))
hc_readm_2013 <- read_csv(paste0(created_data_path, "raw data/Hospital Compare/vwhqi_readm_reduction_2013.csv"))
hc_readm_2014 <- read_csv(paste0(created_data_path, "raw data/Hospital Compare/vwhqi_readm_reduction_2014.csv"))
hc_readm_2015 <- read_csv(paste0(created_data_path, "raw data/Hospital Compare/vwhqi_readm_reduction_2015.csv"))
# Data that I created on leadership teams from the tax forms
ein_leadership_data <- read_rds(paste0(created_data_path, "ein_leadership_changes_data.rds"))
# Data that I created on outcomes from Hospital Compare
outcomes_data <- read_rds(paste0(created_data_path, "hosp_outcomes.rds"))

# Set up Main data #############

# Start with the sample of hospitals that have AHA matches and merge in the relevant info from all data sets
# complete to include years 2008-2015
# for now, only consider hospitals with a direct hospital match (not just system)
hospital_data <- rbind(AHA_ein_matches, manual_matched_eins)
hospital_data <- hospital_data %>%
  mutate(year=2008) %>%
  filter(!is.na(ein_hosp)) %>%
  select(-ein_sys)
hospital_data <- complete(hospital_data,ID, year=2008:2015) %>%
  group_by(ID) %>%
  fill(ein_hosp, .direction="downup") %>%
  ungroup()

# join ein leadership data
hospital_data <- hospital_data %>%
  mutate(ein_hosp = as.character(ein_hosp)) %>%
  left_join(ein_leadership_data, by=c("year", "ein_hosp"="ein"))

observe <- hospital_data %>%
  distinct(ein_hosp)

# only keep rows that had values in that data set
hospital_data <- hospital_data %>%
  filter(!is.na(no_changes_ever))

# join AHA data to get medicare number
hospital_data <- hospital_data %>%
  mutate(ID=as.character(ID)) %>%
  left_join(AHA_hosp_names, by=c("ID", "year"="YEAR"))
hospital_data <- hospital_data %>%
  group_by(ID) %>%
  fill(MCRNUM, .direction="downup") %>%
  ungroup() %>%
  select(ID, ein_hosp, MCRNUM, year, no_changes_ever, no_changes_2010_2014, no_changes_2011_2013, no_small_changes_ever, no_small_changes_2010_2014,
         no_small_changes_2011_2013, no_md_changes_ever, no_md_changes_2010_2014, no_md_changes_2011_2013, total_execs, total_docs)

# # Join HCRIS data ############
# # first, investigate duplicates in the cost report data
# HCRIS_dups <- final_HCRIS_v2010 %>%
#   mutate(count=1) %>%
#   group_by(provider_number, year) %>%
#   mutate(sum=sum(count)) %>%
#   ungroup() %>%
#   filter(sum>1)
#   # A lot of duplicates have multiple reports for the same year. I add together the penalties in this case to get one line per provider, year
#   # Any duplicates left are due to differences in bed count. This isnt a huge deal, just pick one of the years to keep
# 
# final_HCRIS_v2010 <- final_HCRIS_v2010 %>%
#   group_by(provider_number, year) %>%
#   mutate(penalty_sum = sum(hrrp_payment, NA.rm=T),
#          beds_distinct = max(beds, na.rm=T)) %>%
#   ungroup() %>%
#   distinct(provider_number, year, penalty_sum, beds_distinct) 
#   # no more duplicates! 
# 
# final_HCRIS_v2010 <- final_HCRIS_v2010 %>%
#   rename(hrrp_payment = penalty_sum, beds = beds_distinct) %>%
#   mutate(hrrp_payment = abs(hrrp_payment),
#          beds=ifelse(beds=="-Inf",NA,beds))
# 
# # join to main dataset
# hospital_data <- hospital_data %>%
#   left_join(final_HCRIS_v2010, by=c("year", "MCRNUM"="provider_number"))
# 
# # I only need beds from v1996 for years 2008 and 2009. First, get rid of duplicates.
# final_HCRIS_v1996 <- final_HCRIS_v1996 %>%
#   select(year, provider_number, beds) %>%
#   filter(year==2008 | year==2009 | year==2010) %>%
#   group_by(provider_number, year) %>%
#   mutate(beds_distinct = max(beds, na.rm=T)) %>%
#   ungroup() %>%
#   distinct(year, provider_number, beds_distinct) %>%
#   rename(beds = beds_distinct) %>%
#   mutate(beds=ifelse(beds=="-Inf",NA,beds))
# 
# hospital_data <- hospital_data %>%
#   left_join(final_HCRIS_v1996, by=c("year", "MCRNUM"="provider_number")) %>%
#   mutate(beds.x=ifelse(is.na(beds.x),beds.y,beds.x)) %>%
#   rename(beds=beds.x) %>%
#   select(-beds.y) 
# 
# drop data I don't need anymore
rm(final_HCRIS_v1996, final_HCRIS_v2010, HCRIS_dups, AHA_ein_matches, AHA_hosp_names, ein_leadership_data)

# # create indicator for whether the hospital was penalized (from HCRIS data)
# hospital_data <- hospital_data %>%
#   mutate(penalized_HCRIS=ifelse(hrrp_payment>0,1,0)) %>%
#   mutate(penalized_HCRIS=ifelse(year>=2012 & is.na(hrrp_payment),0,penalized_HCRIS))

# Join Hospital Compare HRRP Data ##################
hc_readm_2012 <- hc_readm_2012 %>%
  select(provider, measurename, readratexc) %>%
  mutate(year=2012)
hc_readm_2013 <- hc_readm_2013 %>%
  select(provider, measurename, readratexc) %>%
  mutate(year=2013)
hc_readm_2014 <- hc_readm_2014 %>%
  select(provider, measurename, readratexc) %>%
  mutate(year=2014)
hc_readm_2015 <- hc_readm_2015 %>%
  select(provider_number, measure_name, excess_readmission_ratio) %>%
  mutate(year=2015) %>%
  rename(provider=provider_number, measurename=measure_name, readratexc=excess_readmission_ratio)

hc_readm <- rbind(hc_readm_2012, hc_readm_2013, hc_readm_2014, hc_readm_2015)

# Create penalty variable that equals 1 if the hospital goes over readmissions in any category in pneumonia, heart failure, or AMI
hc_readm <- hc_readm %>%
  filter(!(measurename %in% c("READM-30-HIP-KNEE-HRRP", "READM-30-COPD-HRRP"))) %>%
  mutate(penalized_HC = ifelse(readratexc>1,1,NA)) %>%
  group_by(provider, year) %>%
  fill(penalized_HC, .direction="downup") %>%
  ungroup() %>%
  distinct(provider, year, penalized_HC) %>%
  mutate(penalized_HC=ifelse(is.na(penalized_HC),0,penalized_HC))

# any duplicates?
hc_dups <- hc_readm %>%
  mutate(count=1) %>%
  group_by(provider, year) %>%
  mutate(sum=sum(count)) %>%
  ungroup() %>%
  filter(sum>1)
  # nope! 

# join to main data
hospital_data <- hospital_data %>%
  left_join(hc_readm, by=c("MCRNUM"="provider", "year"))

observe <- hospital_data %>%
  select(year, MCRNUM, penalized_HC) %>%
  filter(year>=2012)


# # some hospitals not found in the hospital compare data?
# hospital_data <- hospital_data %>%
#   mutate(penalized_HC = ifelse(is.na(penalized_HC), penalized_HCRIS, penalized_HC))


# # Look at mean of penalized over time to compare
# penalized_means <- hospital_data %>%
#   group_by(year) %>%
#   summarise_at(c("penalized_HCRIS", "penalized_HC"), list(mean), na.rm=T)

rm(hc_readm, hc_readm_2012, hc_readm_2013, hc_readm_2014, hc_readm_2015, hc_dups, observe)

# Join outcomes data ###############
hospital_data <- hospital_data %>%
  left_join(outcomes_data, by=c("year", "MCRNUM"="providerid"))

# limit to hospitals who were penalized at some point
penalized_hospital_data <- hospital_data %>%
  group_by(MCRNUM) %>%
  mutate(ever_penalized = sum(penalized_HC, na.rm=T)) %>%
  ungroup() %>%
  filter(ever_penalized>0)

num_pen_hospitals <- penalized_hospital_data %>%
  distinct(ein_hosp)

# save the data #####
saveRDS(penalized_hospital_data, paste0(created_data_path, "penalized_hospital_data.rds"))

observe <- penalized_hospital_data %>%
  filter(no_md_changes_2010_2014==1) %>%
  distinct(ein_hosp)


# Summary Statistics ######


# create big summary stats table comparing MD and non-MD hospitals over time (using any MD on leadership team)
anyMD_table <- hospital_data %>%
  select(year, beds, penalized, hrrp_payment, has_any_md, heartfailure_mortality, heartfailure_readmission, pneum_mortality, pneum_readmission) %>%
  group_by(year, has_any_md) %>%
  summarise_at(c("beds", "penalized", "hrrp_payment", "heartfailure_mortality", "heartfailure_readmission", "pneum_mortality", "pneum_readmission"),
               list(mean), na.rm=TRUE) %>%
  filter(!is.na(has_any_md)) %>%
  mutate(has_any_md=ifelse(has_any_md==1,"md","no_md"))

anyMD_table <- anyMD_table %>%
  pivot_wider(id_cols = c("year"), names_from = "has_any_md", values_from = c("beds", "penalized", "hrrp_payment", "heartfailure_mortality", "heartfailure_readmission", "pneum_mortality", "pneum_readmission"))

anyMD_table <- anyMD_table %>%
  select(year, beds_no_md, beds_md, penalized_no_md, penalized_md, hrrp_payment_no_md, hrrp_payment_md, pneum_mortality_no_md, pneum_mortality_md, pneum_readmission_no_md, 
         pneum_readmission_md, heartfailure_mortality_no_md, heartfailure_mortality_md, heartfailure_readmission_no_md, heartfailure_readmission_md) 

knitr::kable(anyMD_table, format = "latex",
             align=c("ccccccccccccc"),
             table.envir="table",
             booktabs=TRUE,
             col.names = c("Year", "No MD", "MD", "No MD", "MD", "No MD", "MD", "No MD", "MD", "No MD", "MD", "No MD", "MD","No MD", "MD"),
             digits=c(0,0,0,2,2,0,0,1,1,1,1,1,1,1,1),
             caption="Averages for MD and non-MD Hospitals Over Time",
             position="h") %>%
  column_spec(1:12,width=".9cm") %>%
  add_header_above(c(" ", "Beds" = 2,"Penalized"=2, "HRRP Amount" = 2, "Pneum. Mortality" = 2, "Pneum. Readm." = 2, "HF Mort." = 2, "HF Readm." = 2)) 


# create big summary stats table comparing MD and non-MD hospitals over time (using ceo MD on leadership team)
ceoMD_table <- hospital_data %>%
  select(year, beds, penalized, hrrp_payment, has_ceo_md, heartfailure_mortality, heartfailure_readmission, pneum_mortality, pneum_readmission) %>%
  group_by(year, has_ceo_md) %>%
  summarise_at(c("beds", "penalized", "hrrp_payment", "heartfailure_mortality", "heartfailure_readmission", "pneum_mortality", "pneum_readmission"),
               list(mean), na.rm=TRUE) %>%
  filter(!is.na(has_ceo_md)) %>%
  mutate(has_ceo_md=ifelse(has_ceo_md==1,"md","no_md"))

ceoMD_table <- ceoMD_table %>%
  pivot_wider(id_cols = c("year"), names_from = "has_ceo_md", values_from = c("beds", "penalized", "hrrp_payment", "heartfailure_mortality", "heartfailure_readmission", "pneum_mortality", "pneum_readmission"))

ceoMD_table <- ceoMD_table %>%
  select(year, beds_no_md, beds_md, penalized_no_md, penalized_md, hrrp_payment_no_md, hrrp_payment_md, pneum_mortality_no_md, pneum_mortality_md, pneum_readmission_no_md, 
         pneum_readmission_md, heartfailure_mortality_no_md, heartfailure_mortality_md, heartfailure_readmission_no_md, heartfailure_readmission_md) 

knitr::kable(ceoMD_table, format = "latex",
             align=c("ccccccccccccc"),
             table.envir="table",
             booktabs=TRUE,
             col.names = c("Year", "No MD", "MD", "No MD", "MD", "No MD", "MD", "No MD", "MD", "No MD", "MD", "No MD", "MD","No MD", "MD"),
             digits=c(0,0,0,2,2,0,0,1,1,1,1,1,1,1,1),
             caption="Averages for MD and non-MD Hospitals Over Time",
             position="h") %>%
  column_spec(1:12,width=".9cm") %>%
  add_header_above(c(" ", "Beds" = 2,"Penalized"=2, "HRRP Amount" = 2, "Pneum. Mortality" = 2, "Pneum. Readm." = 2, "HF Mort." = 2, "HF Readm." = 2)) 

# create big summary stats table comparing MD and non-MD hospitals over time (using president MD)
presMD_table <- hospital_data %>%
  select(year, beds, penalized, hrrp_payment, has_pres_md, heartfailure_mortality, heartfailure_readmission, pneum_mortality, pneum_readmission) %>%
  group_by(year, has_pres_md) %>%
  summarise_at(c("beds", "penalized", "hrrp_payment", "heartfailure_mortality", "heartfailure_readmission", "pneum_mortality", "pneum_readmission"),
               list(mean), na.rm=TRUE) %>%
  filter(!is.na(has_pres_md)) %>%
  mutate(has_pres_md=ifelse(has_pres_md==1,"md","no_md"))

presMD_table <- presMD_table %>%
  pivot_wider(id_cols = c("year"), names_from = "has_pres_md", values_from = c("beds", "penalized", "hrrp_payment", "heartfailure_mortality", "heartfailure_readmission", "pneum_mortality", "pneum_readmission"))

presMD_table <- presMD_table %>%
  select(year, beds_no_md, beds_md, penalized_no_md, penalized_md, hrrp_payment_no_md, hrrp_payment_md, pneum_mortality_no_md, pneum_mortality_md, pneum_readmission_no_md, 
         pneum_readmission_md, heartfailure_mortality_no_md, heartfailure_mortality_md, heartfailure_readmission_no_md, heartfailure_readmission_md) 

knitr::kable(presMD_table, format = "latex",
             align=c("ccccccccccccc"),
             table.envir="table",
             booktabs=TRUE,
             col.names = c("Year", "No MD", "MD", "No MD", "MD", "No MD", "MD", "No MD", "MD", "No MD", "MD", "No MD", "MD","No MD", "MD"),
             digits=c(0,0,0,2,2,0,0,1,1,1,1,1,1,1,1),
             caption="Averages for President MD and non-MD Hospitals Over Time",
             position="h") %>%
  column_spec(1:12,width=".9cm") %>%
  add_header_above(c(" ", "Beds" = 2,"Penalized"=2, "HRRP Amount" = 2, "Pneum. Mortality" = 2, "Pneum. Readm." = 2, "HF Mort." = 2, "HF Readm." = 2)) 


# create big summary stats table comparing hospitals with a CMO and hospitals without a CMO
cmo_table <- hospital_data %>%
  select(year, beds, penalized, hrrp_payment, has_cmo, heartfailure_mortality, heartfailure_readmission, pneum_mortality, pneum_readmission) %>%
  group_by(year, has_cmo) %>%
  summarise_at(c("beds", "penalized", "hrrp_payment", "heartfailure_mortality", "heartfailure_readmission", "pneum_mortality", "pneum_readmission"),
               list(mean), na.rm=TRUE) %>%
  filter(!is.na(has_cmo)) %>%
  mutate(has_cmo=ifelse(has_cmo==1,"cmo","no_cmo"))

cmo_table <- cmo_table %>%
  pivot_wider(id_cols = c("year"), names_from = "has_cmo", values_from = c("beds", "penalized", "hrrp_payment", "heartfailure_mortality", "heartfailure_readmission", "pneum_mortality", "pneum_readmission"))

cmo_table <- cmo_table %>%
  select(year, beds_no_cmo, beds_cmo, penalized_no_cmo, penalized_cmo, hrrp_payment_no_cmo, hrrp_payment_cmo, pneum_mortality_no_cmo, pneum_mortality_cmo, pneum_readmission_no_cmo, 
         pneum_readmission_cmo, heartfailure_mortality_no_cmo, heartfailure_mortality_cmo, heartfailure_readmission_no_cmo, heartfailure_readmission_cmo) 

knitr::kable(cmo_table, format = "latex",
             align=c("ccccccccccccc"),
             table.envir="table",
             booktabs=TRUE,
             col.names = c("Year", "No CMO", "CMO", "No CMO", "CMO", "No CMO", "CMO", "No CMO", "CMO", "No CMO", "CMO", "No CMO", "CMO","No CMO", "CMO"),
             digits=c(0,0,0,2,2,0,0,1,1,1,1,1,1,1,1),
             caption="Averages for Hospitals with and without CMOs",
             position="h") %>%
  column_spec(1:12,width=".9cm") %>%
  add_header_above(c(" ", "Beds" = 2,"Penalized"=2, "HRRP Amount" = 2, "Pneum. Mortality" = 2, "Pneum. Readm." = 2, "HF Mort." = 2, "HF Readm." = 2)) 

# create big summary stats table comparing MD and non-MD hospitals over time (using coo MD on leadership team)
cooMD_table <- hospital_data %>%
  select(year, beds, penalized, hrrp_payment, has_coo_md, heartfailure_mortality, heartfailure_readmission, pneum_mortality, pneum_readmission) %>%
  group_by(year, has_coo_md) %>%
  summarise_at(c("beds", "penalized", "hrrp_payment", "heartfailure_mortality", "heartfailure_readmission", "pneum_mortality", "pneum_readmission"),
               list(mean), na.rm=TRUE) %>%
  filter(!is.na(has_coo_md)) %>%
  mutate(has_coo_md=ifelse(has_coo_md==1,"md","no_md"))

cooMD_table <- cooMD_table %>%
  pivot_wider(id_cols = c("year"), names_from = "has_coo_md", values_from = c("beds", "penalized", "hrrp_payment", "heartfailure_mortality", "heartfailure_readmission", "pneum_mortality", "pneum_readmission"))

cooMD_table <- cooMD_table %>%
  select(year, beds_no_md, beds_md, penalized_no_md, penalized_md, hrrp_payment_no_md, hrrp_payment_md, pneum_mortality_no_md, pneum_mortality_md, pneum_readmission_no_md, 
         pneum_readmission_md, heartfailure_mortality_no_md, heartfailure_mortality_md, heartfailure_readmission_no_md, heartfailure_readmission_md) 

knitr::kable(cooMD_table, format = "latex",
             align=c("ccccccccccccc"),
             table.envir="table",
             booktabs=TRUE,
             col.names = c("Year", "No MD", "MD", "No MD", "MD", "No MD", "MD", "No MD", "MD", "No MD", "MD", "No MD", "MD","No MD", "MD"),
             digits=c(0,0,0,2,2,0,0,1,1,1,1,1,1,1,1),
             caption="Averages for COO MD and non-MD Hospitals Over Time",
             position="h") %>%
  column_spec(1:12,width=".9cm") %>%
  add_header_above(c(" ", "Beds" = 2,"Penalized"=2, "HRRP Amount" = 2, "Pneum. Mortality" = 2, "Pneum. Readm." = 2, "HF Mort." = 2, "HF Readm." = 2)) 




#### Create matrix that has the number of hospitals in each category of ever penalized, never penalized, ever have MD, never have MD
number_hospital_data_anyMD <- hospital_data %>%
  mutate(ever_in_name_data = ifelse(has_any_md %in% c(1,0),1,NA)) %>%
  group_by(ID) %>%
  fill(ever_in_name_data, .direction="downup") %>%
  ungroup() %>%
  filter(ever_in_name_data==1)

number_hospital_data_anyMD <- number_hospital_data_anyMD %>%
  mutate(ever_penalized = ifelse(hrrp_payment>0,1,NA),
         ever_has_md = ifelse(has_any_md==1,1,NA)) %>%
  group_by(ID) %>%
  fill(ever_penalized, ever_has_md, .direction = "downup") %>%
  ungroup() %>%
  mutate(ever_penalized = ifelse(is.na(ever_penalized),0,ever_penalized),
         ever_has_md = ifelse(is.na(ever_has_md),0,ever_has_md)) %>%
  distinct(ID, ever_penalized, ever_has_md)

freq_matrix_anyMD <- table(number_hospital_data_anyMD$ever_penalized, number_hospital_data_anyMD$ever_has_md)
freq_matrix_anyMD <- as.data.frame(freq_matrix_anyMD)

knitr::kable(freq_matrix_anyMD, format = "latex",
             align=c("ccc"),
             table.envir="table",
             booktabs=TRUE,
             col.names = c("Ever Penalized", "Ever has MD in Leadership", "Number of Hospitals"),
             digits=c(0,0,0,0,0,2,2,2,2,2,2,2,2),
             caption="Number of Hospitals with or without penalities/any MDs",
             position="h") %>%
  column_spec(1:12,width="3cm")

#### Create matrix that has the number of hospitals in each category of ever penalized, never penalized, ever have ceo MD, never have ceo MD 
number_hospital_data_ceoMD <- hospital_data %>%
  mutate(ever_in_name_data = ifelse(has_any_md %in% c(1,0),1,NA)) %>%
  group_by(ID) %>%
  fill(ever_in_name_data, .direction="downup") %>%
  ungroup() %>%
  filter(ever_in_name_data==1)

number_hospital_data_ceoMD <- number_hospital_data_ceoMD %>%
  mutate(ever_penalized = ifelse(hrrp_payment>0,1,NA),
         ever_has_md = ifelse(has_ceo_md==1,1,NA)) %>%
  group_by(ID) %>%
  fill(ever_penalized, ever_has_md, .direction = "downup") %>%
  ungroup() %>%
  mutate(ever_penalized = ifelse(is.na(ever_penalized),0,ever_penalized),
         ever_has_md = ifelse(is.na(ever_has_md),0,ever_has_md)) %>%
  distinct(ID, ever_penalized, ever_has_md)

freq_matrix_ceoMD <- table(number_hospital_data_ceoMD$ever_penalized, number_hospital_data_ceoMD$ever_has_md)
freq_matrix_ceoMD <- as.data.frame(freq_matrix_ceoMD)

knitr::kable(freq_matrix_ceoMD, format = "latex",
             align=c("ccc"),
             table.envir="table",
             booktabs=TRUE,
             col.names = c("Ever Penalized", "Ever has MD in ceo Role", "Number of Hospitals"),
             digits=c(0,0,0,0,0,2,2,2,2,2,2,2,2),
             caption="Number of Hospitals with or without penalities/ceo MDs",
             position="h") %>%
  column_spec(1:12,width="3cm") 


#### Create matrix that has the number of hospitals in each category of ever penalized, never penalized, ever have CMO, never have CMO 
number_hospital_data_cmo <- hospital_data %>%
  mutate(ever_in_name_data = ifelse(has_any_md %in% c(1,0),1,NA)) %>%
  group_by(ID) %>%
  fill(ever_in_name_data, .direction="downup") %>%
  ungroup() %>%
  filter(ever_in_name_data==1)

number_hospital_data_cmo <- number_hospital_data_cmo %>%
  mutate(ever_penalized = ifelse(hrrp_payment>0,1,NA),
         ever_has_cmo = ifelse(has_cmo==1,1,NA)) %>%
  group_by(ID) %>%
  fill(ever_penalized, ever_has_cmo, .direction = "downup") %>%
  ungroup() %>%
  mutate(ever_penalized = ifelse(is.na(ever_penalized),0,ever_penalized),
         ever_has_cmo = ifelse(is.na(ever_has_cmo),0,ever_has_cmo)) %>%
  distinct(ID, ever_penalized, ever_has_cmo)

freq_matrix_cmo <- table(number_hospital_data_cmo$ever_penalized, number_hospital_data_cmo$ever_has_cmo)
freq_matrix_cmo <- as.data.frame(freq_matrix_cmo)

knitr::kable(freq_matrix_cmo, format = "latex",
             align=c("ccc"),
             table.envir="table",
             booktabs=TRUE,
             col.names = c("Ever Penalized", "Ever has CMO", "Number of Hospitals"),
             digits=c(0,0,0,0,0,2,2,2,2,2,2,2,2),
             caption="Number of Hospitals with or without penalities/CMOs",
             position="h") %>%
  column_spec(1:12,width="3cm") 

#### Create matrix that has the number of hospitals in each category of ever penalized, never penalized, ever have pres, never have pres 
number_hospital_data_pres <- hospital_data %>%
  mutate(ever_in_name_data = ifelse(has_any_md %in% c(1,0),1,NA)) %>%
  group_by(ID) %>%
  fill(ever_in_name_data, .direction="downup") %>%
  ungroup() %>%
  filter(ever_in_name_data==1)

number_hospital_data_pres <- number_hospital_data_pres %>%
  mutate(ever_penalized = ifelse(hrrp_payment>0,1,NA),
         ever_has_pres = ifelse(has_pres==1,1,NA)) %>%
  group_by(ID) %>%
  fill(ever_penalized, ever_has_pres, .direction = "downup") %>%
  ungroup() %>%
  mutate(ever_penalized = ifelse(is.na(ever_penalized),0,ever_penalized),
         ever_has_pres = ifelse(is.na(ever_has_pres),0,ever_has_pres)) %>%
  distinct(ID, ever_penalized, ever_has_pres)

freq_matrix_pres <- table(number_hospital_data_pres$ever_penalized, number_hospital_data_pres$ever_has_pres)
freq_matrix_pres <- as.data.frame(freq_matrix_pres)

knitr::kable(freq_matrix_pres, format = "latex",
             align=c("ccc"),
             table.envir="table",
             booktabs=TRUE,
             col.names = c("Ever Penalized", "Ever has MD in ceo Role", "Number of Hospitals"),
             digits=c(0,0,0,0,0,2,2,2,2,2,2,2,2),
             caption="Number of Hospitals with or without penalities/ceo MDs",
             position="h") %>%
  column_spec(1:12,width="3cm") 

#### Create matrix that has the number of hospitals in each category of ever penalized, never penalized, ever have coo, never have coo 
number_hospital_data_coo <- hospital_data %>%
  mutate(ever_in_name_data = ifelse(has_any_md %in% c(1,0),1,NA)) %>%
  group_by(ID) %>%
  fill(ever_in_name_data, .direction="downup") %>%
  ungroup() %>%
  filter(ever_in_name_data==1)

number_hospital_data_coo <- number_hospital_data_coo %>%
  mutate(ever_penalized = ifelse(hrrp_payment>0,1,NA),
         ever_has_coo = ifelse(has_coo==1,1,NA)) %>%
  group_by(ID) %>%
  fill(ever_penalized, ever_has_coo, .direction = "downup") %>%
  ungroup() %>%
  mutate(ever_penalized = ifelse(is.na(ever_penalized),0,ever_penalized),
         ever_has_coo = ifelse(is.na(ever_has_coo),0,ever_has_coo)) %>%
  distinct(ID, ever_penalized, ever_has_coo)

freq_matrix_coo <- table(number_hospital_data_coo$ever_penalized, number_hospital_data_coo$ever_has_coo)
freq_matrix_coo <- as.data.frame(freq_matrix_coo)

knitr::kable(freq_matrix_coo, format = "latex",
             align=c("ccc"),
             table.envir="table",
             booktabs=TRUE,
             col.names = c("Ever Penalized", "Ever has MD in ceo Role", "Number of Hospitals"),
             digits=c(0,0,0,0,0,2,2,2,2,2,2,2,2),
             caption="Number of Hospitals with or without penalities/ceo MDs",
             position="h") %>%
  column_spec(1:12,width="3cm") 


