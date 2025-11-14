library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(fabricatr)
library(knitr)
library(kableExtra)
library(MatchIt)

# Read in Data ##################

# Data on which hospitals in the tax data have AHA matches
AHA_ein_matches <- read_rds(paste0(created_data_path,"AHA_ein_matches.rds"))
manual_matched_eins <- read_rds(paste0(created_data_path, "manual_matched_eins.rds"))
# Data from AHA 
AHA <- read_csv(paste0(raw_data_path, "/AHAdata_20052023.csv"))
# HCRIS data that has HRRP penalty information (and beds)
HCRIS <- read_csv(paste0(raw_data_path, "/final_HCRIS_data.csv"))
# Hospital Compare data that also has penalty information
hc_readm_2012 <- read_csv(paste0(raw_data_path, "/Hospital Compare/vwhqi_readm_reduction_2012.csv"))
hc_readm_2013 <- read_csv(paste0(raw_data_path, "/Hospital Compare/vwhqi_readm_reduction_2013.csv"))
hc_readm_2014 <- read_csv(paste0(raw_data_path, "/Hospital Compare/vwhqi_readm_reduction_2014.csv"))
hc_readm_2015 <- read_csv(paste0(raw_data_path, "/Hospital Compare/vwhqi_readm_reduction_2015.csv"))
# Data that I created on leadership teams from the tax forms
ein_leadership_data <- read_rds(paste0(created_data_path, "ein_leadership_changes_data.rds"))
# Data that I created on outcomes from Hospital Compare
outcomes_data <- read_rds(paste0(created_data_path, "hosp_outcomes.rds"))
# read in case mix index
impact2009 <- read_csv(paste0(raw_data_path, "/HospCaseMix/impact2009.csv")) %>%
  select(provider, cmiv) %>%
  mutate(year=2009)
impact2010 <- read_csv(paste0(raw_data_path, "/HospCaseMix/impact2010.csv"))%>%
  select(provider, cmiv) %>%
  mutate(year=2010)
impact2011 <- read_csv(paste0(raw_data_path, "/HospCaseMix/impact2011.csv"))%>%
  select(provider, cmiv) %>%
  mutate(year=2011)
impact2012 <- read_csv(paste0(raw_data_path, "/HospCaseMix/impact2012.csv"))%>%
  select(provider, cmiv) %>%
  mutate(year=2012)
impact2013 <- read_csv(paste0(raw_data_path, "/HospCaseMix/impact2013.csv"))%>%
  select(provider, cmiv) %>%
  mutate(year=2013)
impact2014 <- read_csv(paste0(raw_data_path, "/HospCaseMix/impact2014.csv"))%>%
  select(provider, cmiv) %>%
  mutate(year=2014)
impact2015 <- read_csv(paste0(raw_data_path, "/HospCaseMix/impact2015.csv"))%>%
  select(provider, cmiv) %>%
  mutate(year=2015)

# Set up Main data #############

# start with AHA hospitals to include both for-profit and not-for-profit
AHA <- AHA %>%
  filter(FSTCD<=56 & (CNTRL==23 | (CNTRL>=12 & CNTRL<=16) | (CNTRL>=31 & CNTRL<=33)) & SERV==10)
# 4,475 hospitals

AHA <- AHA %>%
  select(ID, MCRNUM, YEAR, CNTRL, MSTATE, MAPP5, MAPP8, FTMT, PHYGP, FTRNTF, SUBS, SYSID, EHLTH, MNGT, MADMIN)

# Remove hospitals not in the data for enough relevant years
AHA <- AHA %>%
  mutate(count=ifelse(YEAR %in% c(2010,2011,2012,2013,2014),1,0)) %>%
  group_by(ID) %>%
  mutate(sum=sum(count)) %>%
  filter(sum>=5) %>%
  ungroup() %>%
  select(-count, -sum)
# 3,766

AHA <- complete(AHA, MCRNUM, YEAR=2009:2014) %>%
  group_by(MCRNUM) %>%
  fill(CNTRL, .direction="downup") %>%
  ungroup()

# for now, only consider hospitals with a direct hospital match (not just system)
matches <- rbind(AHA_ein_matches, manual_matched_eins)

matches <- matches %>%
  mutate(year=2010) %>%
  filter(!is.na(ein_hosp)) %>%
  select(-ein_sys)

matches <- complete(matches,ID, year=2010:2014) %>%
  group_by(ID) %>%
  fill(ein_hosp, .direction="downup") %>%
  ungroup()

# join ein leadership data
matches <- matches %>%
  mutate(ein_hosp = as.character(ein_hosp)) %>%
  left_join(ein_leadership_data, by=c("year", "ein_hosp"="ein"))

observe <- matches %>%
  distinct(ein_hosp)
# 1133 eins

# only keep rows that had values in that data set
matches <- matches %>%
  mutate(drop=ifelse(is.na(num_execs),1,0)) %>%
  group_by(ein_hosp) %>%
  mutate(drop=sum(drop, na.rm=T)) %>%
  ungroup() %>%
  filter(drop==0) %>%
  select(-drop)
#850 EINs

# join matches to AHA data
hospital_data <- AHA %>%
  mutate(ID=as.character(ID)) %>%
  left_join(matches, by=c("ID", "YEAR"="year"))

hospital_data <- hospital_data %>%
  group_by(ID) %>%
  fill(MCRNUM, .direction="downup") %>%
  ungroup() %>%
  rename(year=YEAR)

# Join HCRIS data ############
HCRIS <- HCRIS %>%
  select(provider_number, year, beds, hrrp_payment, hvbp_payment, tot_discharges, mcare_discharges, mcaid_discharges, labor_costs, movableequipment_purch,
         fixedequipment_purch, build_purch, land_purch, landimpr_purch, tot_operating_exp)

hospital_data <- hospital_data %>%
  left_join(HCRIS, by=c("year", "MCRNUM"="provider_number"))

# drop data I don't need anymore
rm(AHA_ein_matches, AHA, ein_leadership_data, manual_matched_eins)

# create indicator for whether the hospital was HRRP penalized (from HCRIS data)
hospital_data <- hospital_data %>%
  mutate(penalized_hrrp_HCRIS=ifelse(hrrp_payment>0,1,0)) %>%
  mutate(penalized_hrrp_HCRIS=ifelse(year>=2012 & is.na(hrrp_payment),0,penalized_hrrp_HCRIS))

# create indicator for whether the hospital was HVBP incentivized (from HCRIS data)
hospital_data <- hospital_data %>%
  mutate(hvbp_payment = abs(hvbp_payment)) %>%
  mutate(payment_hvbp_HCRIS=ifelse(hvbp_payment>0,1,0)) %>%
  mutate(payment_hvbp_HCRIS=ifelse(year>=2012 & is.na(hvbp_payment),0,payment_hvbp_HCRIS))

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

hc_readm <- rbind(hc_readm_2012, hc_readm_2013, hc_readm_2014, hc_readm_2015) %>%
  mutate(readratexc = ifelse(readratexc=="Not Available", NA, readratexc)) %>%
  mutate(readratexc = as.numeric(readratexc))

# create variables that indicate being penalized for a certain condition
hc_readm <- hc_readm %>%
  mutate(pen_hf = ifelse(measurename %in% c("Heart Failure (HF) 30-Day Readmissions", "READM-30-HF-HRRP") & readratexc>1, 1, NA),
         pen_ha = ifelse(measurename %in% c("Acute Myocardial Infarction (AMI) 30-Day Readmissions", "READM-30-AMI-HRRP") & readratexc>1, 1, NA),
         pen_pnem = ifelse(measurename %in% c("Pneumonia (PN) 30-Day Readmissions", "READM-30-PN-HRRP") & readratexc>1, 1, NA))

# create variables for penalties
hc_readm <- hc_readm %>%
  mutate(hf_rate = ifelse(measurename %in% c("Heart Failure (HF) 30-Day Readmissions", "READM-30-HF-HRRP"), readratexc, NA),
         ha_rate = ifelse(measurename %in% c("Acute Myocardial Infarction (AMI) 30-Day Readmissions", "READM-30-AMI-HRRP"), readratexc, NA),
         pnem_rate = ifelse(measurename %in% c("Pneumonia (PN) 30-Day Readmissions", "READM-30-PN-HRRP"), readratexc, NA)) %>%
  group_by(provider, year) %>%
  fill(hf_rate, ha_rate, pnem_rate, .direction="downup")

# Create penalty variable that equals 1 if the hospital goes over readmissions in any category in pneumonia, heart failure, or AMI
hc_readm <- hc_readm %>%
  filter(!(measurename %in% c("READM-30-HIP-KNEE-HRRP", "READM-30-COPD-HRRP"))) %>%
  group_by(provider, year) %>%
  fill(pen_hf, pen_ha, pen_pnem, .direction="downup") %>%
  ungroup() %>%
  mutate(penalized_HC = ifelse(pen_hf==1 | pen_ha==1 | pen_pnem==1, 1, 0)) %>%
  distinct(provider, year, pen_hf, pen_ha, pen_pnem, penalized_HC, hf_rate, ha_rate, pnem_rate) %>%
  mutate(penalized_HC=ifelse(is.na(penalized_HC),0,penalized_HC),
         pen_hf = ifelse(is.na(pen_hf), 0, pen_hf),
         pen_ha = ifelse(is.na(pen_ha), 0, pen_ha),
         pen_pnem = ifelse(is.na(pen_pnem), 0, pen_pnem))

# join to main data
hospital_data <- hospital_data %>%
  left_join(hc_readm, by=c("MCRNUM"="provider", "year"))

# case mix index data
impact <- rbind(impact2009, impact2010, impact2011, impact2012, impact2013, impact2014, impact2015)
rm(impact2009, impact2010, impact2011, impact2012, impact2013, impact2014, impact2015)

hospital_data <- hospital_data %>%
  left_join(impact, by=c("year", "MCRNUM"="provider"))

observe <- hospital_data %>%
  select(MCRNUM, year, cmiv)


# # some hospitals not found in the hospital compare data?
# hospital_data <- hospital_data %>%
#   mutate(penalized_HC = ifelse(is.na(penalized_HC), penalized_HCRIS, penalized_HC))


rm(hc_readm, hc_readm_2012, hc_readm_2013, hc_readm_2014, hc_readm_2015, hc_dups, observe, HCRIS, matches, impact)

# Join outcomes data ###############
hospital_data <- hospital_data %>%
  left_join(outcomes_data, by=c("year", "MCRNUM"="providerid"))

# create penalty variables
hospital_data <- hospital_data %>%
  group_by(MCRNUM) %>%
  mutate(ever_penalized = sum(penalized_HC, na.rm=T)) %>%
  mutate(ever_pen_ha = sum(pen_ha, na.rm=T),
         ever_pen_hf = sum(pen_hf, na.rm=T),
         ever_pen_pnem = sum(pen_pnem, na.rm=T)) %>%
  ungroup() 

# create other penalty combinations
hospital_data <- hospital_data %>%
  mutate(ever_pen_ha_only=ifelse(ever_pen_ha>=1 & ever_pen_hf==0 & ever_pen_pnem==0,1,0), 
         ever_pen_hf_only=ifelse(ever_pen_hf>=1 & ever_pen_ha==0 & ever_pen_pnem==0,1,0), 
         ever_pen_pnem_only=ifelse(ever_pen_pnem>=1 & ever_pen_hf==0 & ever_pen_ha==0,1,0), 
         ever_pen_ami_hf = ifelse(ever_pen_hf>=1 & ever_pen_ha>=1 & ever_pen_pnem==0,1,0),
         ever_pen_ami_pneum = ifelse(ever_pen_hf==0 & ever_pen_pnem>=1 & ever_pen_ha==1,1,0),
         ever_pen_hf_pneum = ifelse(ever_pen_hf>=1 & ever_pen_pnem>=1 & ever_pen_ha>=0,1,0),
         ever_pen_all = ifelse(ever_pen_hf>=1 & ever_pen_ha>=1 & ever_pen_pnem>=1,1,0)) %>%
  mutate(academic = ifelse(MAPP5==1,1,0)) %>%
  select(-MAPP5)

# remove hospitals with less than 15 beds
hospital_data <- hospital_data %>%
  group_by(ID) %>%
  mutate(maxbeds=max(beds, na.rm=T)) %>%
  ungroup() %>%
  filter(maxbeds>=15) %>%
  select(-maxbeds)

# fill ID
hospital_data <- hospital_data %>%
  group_by(MCRNUM) %>%
  fill(ID, .direction="downup") %>%
  ungroup() %>%
  filter(!is.na(ID)) 

# Remove hospitals that change from nonprofit to for-profit or vice versa
hospital_data <- hospital_data %>%
  mutate(profit_status = ifelse(CNTRL %in% c(12,13,14,15,16,23),"nonprofit","forprofit"))

hospital_data <- hospital_data %>%
  mutate(NFP = ifelse(profit_status=="nonprofit", 1, 0),
         FP = ifelse(profit_status=="forprofit",1,0)) %>%
  group_by(ID) %>%
  arrange(year) %>%
  mutate(lag_NFP = dplyr::lag(NFP)) %>%
  ungroup() %>%
  filter(NFP==lag_NFP | !is.na(num_execs))

# Create ever received payment
hospital_data <- hospital_data %>%
  group_by(ID) %>%
  mutate(ever_hvbp = ifelse(sum(payment_hvbp_HCRIS, na.rm=T)>0,1,0)) %>%
  ungroup()

# create ever variables from AHA
hospital_data <- hospital_data %>%
  mutate(part_of_system = ifelse(is.na(SYSID),0,1)) %>%
  mutate(ever_subs = ifelse(SUBS==1,1,NA)) %>%
  mutate(ever_part_of_system=ifelse(part_of_system==1,1,NA)) %>%
  group_by(ID) %>%
  fill(ever_part_of_system, ever_subs, .direction = "downup") %>%
  ungroup() %>%
  mutate(ever_part_of_system=ifelse(is.na(ever_part_of_system),0,ever_part_of_system)) %>%
  mutate(ever_subs = ifelse(is.na(ever_subs),0,ever_subs))



# find out the percentile of the hospital's highest rate in the first year they were penalized
perc <- hospital_data %>%
  filter(penalized_HC==1) %>%
  group_by(MCRNUM) %>%
  mutate(minyr = min(year)) %>%
  ungroup() %>%
  filter(year==minyr) %>%
  group_by(MCRNUM) %>%
  mutate(max_rate = max(hf_rate, ha_rate, pnem_rate, na.rm=T)) %>%
  ungroup() %>%
  distinct(MCRNUM, max_rate) %>%
  mutate(rate_tercile = split_quantile(max_rate, 3)) %>%
  distinct(MCRNUM, rate_tercile)

hospital_data <- hospital_data %>%
  left_join(perc, by = "MCRNUM") %>%
  mutate(has_any_md = ifelse(num_doctors>0, 1, 0)) %>%
  mutate(ever_penalized = ifelse(ever_penalized>=1,1,0)) 

hospital_data <- hospital_data %>%
  mutate(FP=ifelse(profit_status=="forprofit",1,0))

hospital_data <- hospital_data %>%
  group_by(ID) %>%
  mutate(ever_has_md = ifelse(sum(has_any_md, na.rm=T)>0,1,0)) %>%
  ungroup() %>%
  mutate(ever_has_md = ifelse(profit_status=="forprofit",NA,ever_has_md)) 

# only keep years 2010 - 2015
hospital_data <- hospital_data %>%
  filter(year<2016 & year>2008)

# matching for FP - NFP
match_data <- hospital_data %>%
  filter(year==2010) %>%
  select(MCRNUM, FP, beds, 
         patnum_heartfailure_readmission, patnum_heartattack_readmission, patnum_pneum_readmission,
         ever_penalized, ever_pen_hf, ever_pen_ha, ever_pen_pnem) %>%
  na.omit()

match <- matchit(FP ~ beds + ever_pen_hf + ever_pen_ha + ever_pen_pnem + patnum_heartfailure_readmission + patnum_heartattack_readmission + patnum_pneum_readmission,
                 data = match_data,
                 method = "cem")

matched_data <- match.data(match) %>%
  select(MCRNUM, weights) %>%
  mutate(matched_FP_NFP=1)

hospital_data <- hospital_data %>%
  left_join(matched_data, by="MCRNUM") %>%
  mutate(matched_FP_NFP = ifelse(is.na(matched_FP_NFP),0,matched_FP_NFP))

# matching for FP - MD NFP
match_data <- hospital_data %>%
  filter(profit_status=="forprofit" | ever_has_md==1) %>%
  filter(year==2010) %>%
  select(MCRNUM, FP, beds, 
         patnum_heartfailure_readmission, patnum_heartattack_readmission, patnum_pneum_readmission,
         ever_penalized, ever_pen_hf, ever_pen_ha, ever_pen_pnem) %>%
  na.omit()

match <- matchit(FP ~ beds + ever_pen_hf + ever_pen_ha + ever_pen_pnem + patnum_heartfailure_readmission + patnum_heartattack_readmission + patnum_pneum_readmission,
                 data = match_data,
                 method = "cem")

matched_data <- match.data(match) %>%
  select(MCRNUM, weights) %>%
  mutate(matched_FP_nfpMD=1)

hospital_data <- hospital_data %>%
  left_join(matched_data, by="MCRNUM") %>%
  mutate(matched_FP_nfpMD = ifelse(is.na(matched_FP_nfpMD),0,matched_FP_nfpMD))

# matching for FP - no MD NFP
match_data <- hospital_data %>%
  filter(profit_status=="forprofit" | ever_has_md==0) %>%
  filter(year==2010) %>%
  select(MCRNUM, FP, beds, 
         patnum_heartfailure_readmission, patnum_heartattack_readmission, patnum_pneum_readmission,
         ever_penalized, ever_pen_hf, ever_pen_ha, ever_pen_pnem) %>%
  na.omit()

match <- matchit(FP ~ beds + ever_pen_hf + ever_pen_ha + ever_pen_pnem + patnum_heartfailure_readmission + patnum_heartattack_readmission + patnum_pneum_readmission,
                 data = match_data,
                 method = "cem")

matched_data <- match.data(match) %>%
  select(MCRNUM, weights) %>%
  mutate(matched_FP_nfpnoMD=1)

hospital_data <- hospital_data %>%
  left_join(matched_data, by="MCRNUM") %>%
  mutate(matched_FP_nfpnoMD = ifelse(is.na(matched_FP_nfpnoMD),0,matched_FP_nfpnoMD))

# matching for MD NFP - no MD NFP
match_data <- hospital_data %>%
  filter(!is.na(num_execs)) %>%
  filter(year==2010) %>%
  select(MCRNUM, has_any_md, beds, 
         patnum_heartfailure_readmission, patnum_heartattack_readmission, patnum_pneum_readmission,
         ever_penalized, ever_pen_hf, ever_pen_ha, ever_pen_pnem) %>%
  na.omit()

match <- matchit(has_any_md ~ beds + ever_pen_hf + ever_pen_ha + ever_pen_pnem + patnum_heartfailure_readmission + patnum_heartattack_readmission + patnum_pneum_readmission,
                 data = match_data,
                 method = "cem")

matched_data <- match.data(match) %>%
  select(MCRNUM, weights) %>%
  mutate(matched_nfpMD_nfpnoMD=1)

hospital_data <- hospital_data %>%
  left_join(matched_data, by="MCRNUM") %>%
  mutate(matched_nfpMD_nfpnoMD = ifelse(is.na(matched_nfpMD_nfpnoMD),0,matched_nfpMD_nfpnoMD))

# look at means for different ownership types 
means <- hospital_data %>%
  group_by(year, CNTRL) %>%
  summarise_if(is.numeric, list(mean), na.rm=T) %>%
  ungroup()

# look at number of hospitals in each ownership type
num <- hospital_data %>%
  distinct(MCRNUM, CNTRL) %>%
  group_by(CNTRL) %>%
  summarise(n=n())

num_leadership <- hospital_data %>%
  filter(!is.na(num_execs)) %>%
  distinct(MCRNUM, CNTRL) %>%
  group_by(CNTRL) %>%
  summarise(n=n())

# now combine all nonprofits and for-profits into a single category and compare means
means <- hospital_data %>%
  group_by(year, profit_status) %>%
  summarise_if(is.numeric, list(mean), na.rm=T) %>%
  ungroup()

# look at number of hospitals in each ownership type
num <- hospital_data %>%
  distinct(MCRNUM, profit_status) %>%
  group_by(profit_status) %>%
  summarise(n=n())

num_leadership <- hospital_data %>%
  filter(!is.na(num_execs)) %>%
  distinct(MCRNUM, profit_status) %>%
  group_by(profit_status) %>%
  summarise(n=n())

hospital_data <- hospital_data %>%
  mutate(ever_ceo_md = ifelse(ever_ceo_md>0,1,0))



# save the data #####
saveRDS(hospital_data, paste0(created_data_path, "all_hospital_data.rds"))

observe <- hospital_data %>%
  filter(no_md_change_2010_2014==1) %>%
  distinct(ein_hosp)