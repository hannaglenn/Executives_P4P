library(readr)
library(dplyr)
library(tidyr)
library(knitr)
library(kableExtra)

# Read in all data
names_data <- read_rds(paste0(created_data_path, "names_data.rds"))
#AHA_ein_matches <- read_rds(paste0(created_data_path,"AHA_ein_matches.rds"))


  


# How many hospitals only have one name for a given year?
names_data <- names_data %>%
  group_by(ein, year) %>%
  mutate(count=1) %>%
  mutate(sum=sum(count)) %>%
  ungroup()

observe <- names_data %>%
  filter(sum==1)
  # 128 hospitals

# create a separate data set without physicians or board members
leadership_names_data <- names_data %>%
  mutate(position1=ifelse(position1=="board" | position1=="physician" | position1=="division chief" | position1=="ex officio",NA,position1), 
         position2=ifelse(position2=="board" | position2=="physician" | position2=="division chief" | position2=="ex officio",NA,position2)) %>%
  filter(!(is.na(position1) & is.na(position2)))

# get rid of people who were past members
leadership_names_data <- leadership_names_data %>%
  filter(is.na(former))

yearly_counts_leaders <- leadership_names_data %>%
  mutate(count=1,
         count_ceo=ifelse(position1=="ceo"|position2=="ceo",1,NA),
         count_cfo=ifelse(position1=="cfo"|position2=="cfo",1,NA),
         count_cmo=ifelse(position1=="cmo"|position2=="cmo",1,NA),
         count_pres=ifelse(position1=="president"|position2=="president",1,NA),
         count_ceopres=ifelse(position1=="ceo"|position2=="ceo"|position1=="president"|position2=="president",1,NA)) %>%
  group_by(year, ein) %>%
  fill(count_ceo, count_cfo, count_cmo, count_pres, count_ceopres, .direction="downup") %>%
  ungroup() %>%
  distinct(ein, year, count, count_ceo, count_cfo, count_cmo, count_pres, count_ceopres) %>%
  group_by(year) %>%
  mutate(num_hosp=sum(count, na.rm=T),
         num_hosp_ceo=sum(count_ceo, na.rm=T),
         num_hosp_cfo=sum(count_cfo, na.rm=T),
         num_hosp_cmo=sum(count_cfo, na.rm=T),
         num_hosp_pres=sum(count_cfo, na.rm=T),
         num_hosp_ceopres=sum(count_ceopres, na.rm=T)) %>%
  ungroup() %>%
  distinct(year, num_hosp, num_hosp_ceo, num_hosp_cfo, num_hosp_cmo, num_hosp_pres, num_hosp_ceopres)




# how many clinical experience people do we see?
leadership_names_data <- leadership_names_data %>%
  mutate(clinician=ifelse(is.na(title),0,1))
leadership_names_data <- leadership_names_data %>%
  mutate(ceo_clinician=ifelse(clinician==1 & (position1=="ceo" | position2=="ceo"),1,0))

ceo_clinician <- leadership_names_data %>%
  filter(ceo_clinician==1) %>%
  distinct(ein)

# create one position variable based on importance
officers_data <- names_data %>%
  filter(officer==1) %>%
  mutate(position=ifelse(is.na(position2), position1, NA)) %>%
  mutate(position=ifelse(is.na(position) & (position1=="ceo"|position2=="ceo"), "ceo", position)) %>%
  mutate(position=ifelse(is.na(position) & (position1=="president"|position2=="president"), "president", position)) %>%
  mutate(position=ifelse(is.na(position) & position1 %in% c("board", "physician"), position2, position)) %>%
  mutate(position=ifelse(is.na(position) & position2 %in% c("board", "physician"), position1, position)) %>%
  mutate(position=ifelse(is.na(position) & (position1=="cfo"|position2=="cfo"), "cfo", position)) %>%
  mutate(position=ifelse(is.na(position) & (position1=="cno"|position2=="cno"), "cno", position)) %>%
  mutate(position=ifelse(is.na(position) & (position1=="cmo"|position2=="cmo"), "cmo", position)) %>%
  mutate(position=ifelse(is.na(position) & (position1=="coo"|position2=="coo"), "coo", position))

pres_ceo_data <- officers_data %>%
  filter(position %in% c("ceo", "president")) %>%
  mutate(name=paste0(first_name, " ", last_name)) %>%
  select(-first_name, -last_name, -position1, -position2, -officer)


# make sure every ein, year has president and ceo so i can fill in missing years when relevant
pres_ceo_data <- complete(pres_ceo_data, ein, year=2009:2016, position = c("president", "ceo"))

pres_ceo_data <- pres_ceo_data %>%
  mutate(former=ifelse(str_detect(extra, "part year"), "part year", former))

# For now, limit to the simplest observations where there is only one president, ceo per year
# after removing "former"
pres_ceo_data <- pres_ceo_data %>%
  filter(is.na(former)) %>%
  group_by(ein, year, position) %>%
  mutate(number=row_number()) %>%
  ungroup() %>%
  mutate(multiple=ifelse(number>1,1,NA)) %>%
  group_by(ein) %>%
  fill(multiple, .direction="downup") %>%
  ungroup() %>%
  filter(is.na(multiple))

# fill in missing years when applicable
pres_ceo_data <- pres_ceo_data %>%
  group_by(ein, position) %>%
  mutate(lag1 = lag(name)) %>%
  mutate(lag2 = lag(lag1)) %>%
  mutate(lag3 = lag(lag2)) %>%
  mutate(lag4 = lag(lag3)) %>%
  mutate(lag5 = lag(lag4)) %>%
  mutate(lag6 = lag(lag5)) %>%
  mutate(lag7 = lag(lag6)) %>%
  mutate(lead1 = lead(name)) %>%
  mutate(lead1 = lag(name)) %>%
  mutate(lead2 = lag(lead1)) %>%
  mutate(lead3 = lag(lead2)) %>%
  mutate(lead4 = lag(lead3)) %>%
  mutate(lead5 = lag(lead4)) %>%
  mutate(lead6 = lag(lead5)) %>%
  mutate(lead7 = lag(lead6)) %>%
  ungroup()
  
# Record the year of matching name, position
pres_ceo_data <- pres_ceo_data %>%
  mutate(match_year=ifelse(name==lag1 | name==lag2 | name==lag3 | name==lag4 | name==lag5 | name==lag6 | name==lag7 |
                             name==lead1 | name==lead2 | name==lead3 | name==lead4 | name==lead5 | name==lead6 | name==lead7,year,NA)) %>%
  group_by(ein, position) %>%
  mutate(min_matchyr=min(match_year, na.rm=T),
         max_matchyr = max(match_year, na.rm=T)) %>%
  mutate(min_matchname = ifelse(year==min_matchyr, name, NA),
         max_matchname = ifelse(year==max_matchyr, name, NA)) %>%
  group_by(ein, position) %>%
  fill(min_matchname, max_matchname, .direction="downup") %>%
  ungroup()

# calculate how many missing name observations there are before filling
observe <- pres_ceo_data %>%
  mutate(missing_name=ifelse(is.na(name), 1, 0)) 
summary(observe$missing_name)
  # 82%

pres_ceo_data <- pres_ceo_data %>%
  mutate(name = ifelse(year>min_matchyr & year<max_matchyr & min_matchname==max_matchname, min_matchname, name)) %>%
  select(-starts_with("lag"), -starts_with("lead"))
  # now 81%, didn't help much 






# get a sense of validity of MD titles
names_data <- names_data %>%
  mutate(clinician=ifelse(!is.na(title),1,NA),
         physician=ifelse(position1=="physician" | position2=="physician", 1, NA)) %>%
  group_by(ein, year) %>%
  fill(clinician, physician, .direction="downup") %>%
  ungroup() %>%
  mutate(clinician=ifelse(is.na(clinician),0,clinician),
         physician=ifelse(is.na(physician),0,physician)) 
names_data <- names_data %>%
  mutate(phys_noMD=ifelse(clinician==0 & physician==1,1,0))

observe <- names_data %>%
  filter(phys_noMD==1)












