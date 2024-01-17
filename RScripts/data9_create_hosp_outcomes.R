library(readr)
library(dplyr)
library(tidyr)
library(stringr)

# Read in data from hospital compare
# focus first and mortality and readmissions data
for (i in 2008){
  year <- read_csv(paste0(created_data_path, "raw data/Hospital Compare/dbo_vwhqi_hosp_mortality_xwlk_",i,".csv"))
  assign(paste0("mort",i), year)
}
for (i in 2009:2011){
  year <- read_csv(paste0(created_data_path, "raw data/Hospital Compare/dbo_vwhqi_hosp_mortality_readm_xwlk_",i,".csv"))
  assign(paste0("mort",i), year)
}
for (i in 2012){
  year <- read_csv(paste0(created_data_path, "raw data/Hospital Compare/vwhqi_hosp_mortality_",i,".csv"))
  assign(paste0("mort",i), year)
}
for (i in 2013){
  year <- read_csv(paste0(created_data_path, "raw data/Hospital Compare/dbo_vwhqi_hosp_mortality_readm_xwlk_",i,".csv"))
  assign(paste0("mort",i), year)
}
for (i in 2014){
  year <- read_csv(paste0(created_data_path, "raw data/Hospital Compare/hqi_hosp_readmcompdeath_",i,".csv"))
  assign(paste0("mort",i), year)
}
for (i in 2015){
  year <- read_csv(paste0(created_data_path, "raw data/Hospital Compare/hqi_hosp_readmdeath_",i,".csv"))
  assign(paste0("mort",i), year)
}

mort2008$rate <- str_extract(mort2008$mortalityrate, "^\\d\\d")
mort2009$rate <- str_extract(mort2009$mortality_readmrate, "^\\d\\d")

# create common variables
mort2008 <- mort2008 %>%
  select(provider, condition, measurename, rate, patnum) %>%
  mutate(year=2008) %>%
  rename(providerid=provider) %>%
  mutate(rate=as.numeric(rate))
mort2009 <- mort2009 %>%
  select(provider, condition, measurename, rate, patnum)  %>%
  mutate(year=2009) %>%
  rename(providerid=provider) %>%
  mutate(rate=as.numeric(rate))
mort2010 <- mort2010 %>%
  select(provider, condition, measurename, mortality_readmrate, patnum)  %>%
  mutate(year=2010) %>%
  rename(providerid=provider, rate=mortality_readmrate) %>%
  mutate(rate=ifelse(rate=="N/A", NA, rate)) %>%
  mutate(rate=as.numeric(rate))
mort2011 <- mort2011 %>%
  select(provider, condition, measurename, mortality_readmrate, patnum)  %>%
  mutate(year=2011) %>%
  rename(providerid=provider, rate=mortality_readmrate) %>%
  mutate(rate=ifelse(rate=="N/A", NA, rate)) %>%
  mutate(rate=as.numeric(rate))
mort2012 <- mort2012 %>%
  select(provider, condition, measurename, mortality_readmrate, patnum)  %>%
  mutate(year=2012) %>%
  rename(providerid=provider, rate=mortality_readmrate) %>%
  mutate(rate=ifelse(rate=="Not Available", NA, rate)) %>%
  mutate(rate=as.numeric(rate))
mort2013 <- mort2013 %>%
  select(provider, condition, measurename, morreadr, patnum)  %>%
  mutate(year=2013) %>%
  rename(providerid=provider, rate=morreadr)  %>%
  mutate(rate=ifelse(rate=="Not Available", NA, rate)) %>%
  mutate(rate=as.numeric(rate))
mort2014 <- mort2014 %>%
  select(providerid, measurename, score, denominator)  %>%
  mutate(year=2014, condition=NA) %>%
  rename(rate=score, patnum = denominator)
mort2015 <- mort2015 %>%
  select(providerid, measurename, score, denominator) %>%
  mutate(year=2015, condition=NA) %>%
  rename(rate=score, patnum = denominator)

mortality_data <- rbind(mort2008, mort2009, mort2010, mort2011, mort2012, mort2013, mort2014, mort2015)
rm(mort2008, mort2009, mort2010, mort2011, mort2012, mort2013, mort2014, mort2015)

# fix condition variable for years that didn't have it
mortality_data <- mortality_data %>%
  mutate(condition = ifelse(is.na(condition) & str_detect(measurename, fixed('pneumonia', ignore_case=T)), "Pneumonia", condition)) %>%
  mutate(condition = ifelse(is.na(condition) & str_detect(measurename, fixed('heart failure', ignore_case=T)), "Heart Failure", condition)) %>%
  mutate(condition = ifelse(is.na(condition) & str_detect(measurename, fixed('heart attack', ignore_case=T)), "Heart Attack", condition),
         condition = ifelse(is.na(condition) & str_detect(measurename, fixed('ami', ignore_case=T)), "Heart Attack", condition)) %>%
  filter(condition %in% c("Pneumonia", "Heart Failure", "Heart Attack")) %>%
  mutate(condition = ifelse(condition=="Pneumonia", "pneum", condition)) %>%
  mutate(condition = ifelse(condition=="Heart Attack", "heartattack", condition)) %>%
  mutate(condition = ifelse(condition=="Heart Failure", "heartfailure", condition))

# create variable for whether its a readmission rate or mortality rate
mortality_data <- mortality_data %>%
  mutate(patnum = as.numeric(patnum)) %>%
  mutate(rate_type = ifelse(str_detect(measurename, regex("readmission", ignore_case=T)), "readmission", "mortality")) %>%
  select(providerid, year, condition, rate_type, rate, patnum) %>%
  group_by(providerid, year, rate_type) %>%
  mutate(weighted_avg = weighted.mean(rate, patnum, na.rm = T)) %>%
  ungroup()

mortality_data <- mortality_data %>%
  pivot_wider(id_cols=c("providerid", "year"), names_from = c("condition", "rate_type"), values_from = c("rate", "weighted_avg")) %>%
  select(-weighted_avg_heartattack_mortality, -weighted_avg_heartfailure_mortality, -weighted_avg_heartfailure_readmission, -weighted_avg_heartattack_readmission) %>%
  rename(weightedavg_read = weighted_avg_pneum_readmission, weightedavg_mort = weighted_avg_pneum_mortality)

# save data 
saveRDS(mortality_data, paste0(created_data_path, "hosp_outcomes.rds"))

num_hosp <- mortality_data %>%
  distinct(providerid)






