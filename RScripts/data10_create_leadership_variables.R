library(readr)
library(dplyr)
library(stringr)
library(tidyr)


# read in names data
cleaned_text <- read_rds(paste0(created_data_path, "cleaned_text.rds"))

# First objective is to identify whether someone is part of the board or part of the actual day to day decision making
# if the only position is physician, drop them as they are just a high compensated employee
cleaned_text <- cleaned_text %>%
  filter(!(position1 %in% c("physician", "anesthesiologist", "dermatologist", "surgeon") & is.na(position2)))

# ceo sometimes got stuck in the "extra" text, move it to position 1 (this is always an exec person)
cleaned_text <- cleaned_text %>%
  mutate(position1 = ifelse(!is.na(extra) & str_detect(extra,"ceo\\b"),"ceo",position1)) %>%
  mutate(position1 = ifelse(!is.na(extra) & str_detect(extra,"cfo\\b"),"cfo",position1)) %>%
  mutate(position1 = ifelse(!is.na(extra) & str_detect(extra,"cmo\\b"),"cmo",position1)) %>%
  mutate(position1 = ifelse(!is.na(extra) & str_detect(extra,"cqo\\b"),"cqo",position1))

# First goal: categorize each name as board member or executive (decision maker) ####

# buckets I need to make sure my categories are correct:
# 1. do they have a board member title?
# 2. do they have an "executive" title?
# 3. are they a physician on top of another position?
# 4. president is tricky. have a different indicator
# 5. Keep an indicator for whether they are a vice president *I may drop these as robustness check later)

# make indicators for whether position 1 and position 2 are board or not
cleaned_text <- cleaned_text %>%
  mutate(pos1_board = ifelse(position1 %in% c("board", "trustee", "director", "chair", "vice chair", "chairman", "vice chairman", "treasurer", "secretary", "division chief", "chairperson", "ex officio"),1,0)) %>%
  mutate(pos2_board = ifelse(position2 %in% c("board", "trustee", "director", "chair", "vice chair", "chairman", "vice chairman", "treasurer", "secretary", "division chief", "chairperson", "ex officio"),1,0))

# make indicators for whether position1 and position2 are for sure executive members (leave president out for now)
cleaned_text <- cleaned_text %>%
  mutate(pos1_exec = ifelse(position1 %in% c("medical director", "ceo", "cfo", "system executive", "executive", "coo", "chief medical officer", "chief executive officer", "chief financial officer", "cpo", "cmo", "cno", "cqo", "chief quality officer"), 1, 0)) %>%
  mutate(pos2_exec = ifelse(position2 %in% c("medical director", "ceo", "cfo", "system executive", "executive", "coo", "chief medical officer", "chief executive officer", "chief financial officer", "cpo", "cmo", "cno", "cqo", "chief quality officer"), 1, 0))

# make indicators for whether position1 or position2 indicate president or vice president
cleaned_text <- cleaned_text %>%
  mutate(pos1_pres = ifelse(position1 %in% c("president", "pres", "vp", "vice president", "peesident"), 1, 0)) %>%
  mutate(pos2_pres = ifelse(position2 %in% c("president", "pres", "vp", "vice president", "peesident"), 1, 0)) %>%
  mutate(pos1_vp = ifelse(position1 %in% c("vp", "vice president"), 1, 0)) %>%
  mutate(pos2_vp = ifelse(position2 %in% c("vp", "vice president"), 1, 0))

# make indicators for whether position1 or position2 indicate physician
cleaned_text <- cleaned_text %>%
  mutate(pos1_phys = ifelse(position1 %in% c("physician", "anesthesiologist", "dermatologist", "surgeon"), 1, 0)) %>%
  mutate(pos2_phys = ifelse(position2 %in% c("physician", "anesthesiologist", "dermatologist", "surgeon"), 1, 0)) %>%
  filter(!(pos1_phys==1 & pos2_phys==1))

# create indicator for extra text being informative of hospital leadership
cleaned_text <- cleaned_text %>%
  mutate(extra_text_hosp = ifelse(!is.na(extra) & str_detect(extra, "\\bof\\b|^of\\b|medical staff|mc\\b|officer|senior|center|professional|network|project|hhc|clinic|qual|relation|facilit|manage|support|revenue|strategic|affairs|acute|planning|legal|oficer|region|development|info|develop|counsel|external|hosp|community|chief|finan|service|admin|health|business|care|general|tech|exec|oper|practice|market|med|fiscal|nursing|human|hs\\b|corporate|hr|services|medical|patient"),1,0))

# Conditions on the position of the person
# 1. If either position is exec, the person is exec
# 2. If its board + anything other than exec or president, the person is board
# 3. If it's president + nothing else, with extra text that indicates hospital, they are exec
# 4. If it's president + board member, classify as board
# 5. Anyone left we don't know if they are board or decision makers

cleaned_text <- cleaned_text %>%
  mutate(position = ifelse(pos1_exec==1 | pos2_exec==1, "executive", NA)) %>% #1
  mutate(position = ifelse(is.na(position) & pos1_board==1 & pos2_pres==0, "board", position)) %>% #2
  mutate(position = ifelse(is.na(position) & pos2_board==1 & pos1_pres==0, "board", position)) %>% #2
  mutate(position = ifelse(is.na(position) & pos1_pres==1 & pos2_board==0 & extra_text_hosp==1, "executive", position)) %>% #3
  mutate(position = ifelse(is.na(position) & pos2_pres==1 & pos1_board==0 & extra_text_hosp==1, "executive", position)) %>% #3
  mutate(position = ifelse(is.na(position) & pos1_pres==1 & pos2_board==1, "board", position)) %>% #4
  mutate(position = ifelse(is.na(position) & pos2_pres==1 & pos1_board==1, "board", position)) %>% #4
  mutate(position = ifelse(is.na(position), "president (board or executive)", position)) #5

cleaned_text <- cleaned_text %>%
  mutate(ceo = ifelse(position1=="ceo" | position2 == "ceo" | position1 == "chief executive officer" | position2 == "chief executive officer", 1,0)) %>%
  mutate(ceo = ifelse(is.na(ceo),0,ceo))

cleaned_text <- cleaned_text %>%
  mutate(cmo = ifelse(position1=="cmo" | position2 == "cmo" | position1 == "chief medical officer" | position2 == "chief medical officer", 1,0)) %>%
  mutate(cmo = ifelse(is.na(cmo),0,cmo))
  
executive_data <- cleaned_text %>%
  mutate(vp = ifelse(pos1_vp==1 | pos2_vp==1, 1, 0)) %>%
  filter(position == "executive") %>%
  select(ein, year, name, position, title, former, vp, ceo, cmo) %>%
  filter(!(former %in% c("past", "former", "past interim")))

# loop through eins to fill in any missing years that happen between nonmissing years
eins <- unique(executive_data$ein)

# create empty data to fill
executive_data_filled <- executive_data %>% filter(year==0)

for (i in seq_along(eins)){
  data <- executive_data %>%
    mutate(year=as.numeric(year)) %>%
    filter(ein == eins[[i]]) %>%
    group_by(name, position) %>%
    mutate(id = cur_group_id()) %>%
    arrange(year) %>%
    ungroup() 
  
  # complete so that each group has a row in each year (only for now)
  data <- complete(data, year=2009:2016, id)
  
  max_id <- max(data$id)

  for (j in 1:max_id){
    name_data <- data %>%
      filter(id==j) %>%
      mutate(lag = lag(ein),
             lead = lead(ein)) %>%
      fill(lag, .direction="down") %>%
      fill(lead, .direction="up") %>%
      mutate(ein = ifelse(is.na(ein) & !is.na(lag) & !is.na(lead), lag, ein)) 
    
    name_data <- name_data %>%
      select(-lag, -lead) %>%
      filter(!is.na(ein)) %>%
      fill(name, position, .direction="downup")
    
    executive_data_filled <- rbind(executive_data_filled, name_data)
  }
}  

# fill MD
executive_data_filled <- executive_data_filled %>%
  group_by(ein, id) %>%
  fill(title, vp, ceo, cmo, .direction="downup") %>%
  ungroup()

# now summarize executive teams
executive_data <- executive_data_filled %>%
  group_by(ein, year) %>%
  mutate(num_execs = n())

rm(executive_data_filled, name_data, data, cleaned_text)

# goal 2: identify whether changes occur from year to year ####

# get rid of hospitals who are not present in the data at least for 2010-2014
executive_data <- executive_data %>%
  mutate(in_2010 = ifelse(year==2010,1,NA),
         in_2011 = ifelse(year==2011,1,NA),
         in_2012 = ifelse(year==2012,1,NA),
         in_2013 = ifelse(year==2013,1,NA),
         in_2014 = ifelse(year==2014,1,NA)) %>%
  group_by(ein) %>%
  fill(in_2010, in_2011, in_2012, in_2013, in_2014, .direction="downup") %>%
  ungroup() %>%
  mutate(present_2010_2014 = ifelse(in_2010==1 & in_2011==1 & in_2012==1 & in_2013==1 & in_2014==1, 1, 0)) %>%
  mutate(present_2010_2014 = ifelse(is.na(present_2010_2014), 0, present_2010_2014)) 
  # 91% of observations are in the data

# keep a record of those that aren't to see if we can manually fill in some missing info caused by OCR
#executive_data_notin20102014 <- executive_data_filled %>%
  #filter(present_2010_2014==0)

# saveRDS(executive_data_notin20102014, paste0(created_data_path, "executive_data_notin20102014.rds"))
# write.csv(executive_data_notin20102014, paste0(created_data_path, "executive_data_notin20102014.csv"))

# now filter out those who are not in the data for this time period
executive_data <- executive_data %>%
  filter(present_2010_2014==1)

executive_data <- executive_data %>%
  select(-in_2010, -in_2011, -in_2012, -in_2013, -in_2014, -present_2010_2014)

# figure out whether the leadership team has changed at all
detect_changes <- executive_data %>% 
  group_by(ein, year) %>% 
  dplyr::summarize(list_of_ids = paste(sort(unique(id)),collapse=", ")) %>%
  group_by(ein) %>%
  arrange(year) %>%
  mutate(lag_list = dplyr::lag(list_of_ids)) %>%
  ungroup %>%
  mutate(any_change = ifelse(list_of_ids!=lag_list, 1, 0)) %>%
  select(ein, year, any_change)

# merge back to executive data filled
executive_data <- executive_data %>%
  left_join(detect_changes, by=c("ein", "year")) %>%
  mutate(no_changes_2010_2014 = ifelse(any_change==1 & year %in% c(2010, 2011, 2012, 2013), 1, NA)) %>%
  group_by(ein) %>%
  mutate(no_changes_2010_2014 = ifelse(sum(no_changes_2010_2014, na.rm=T)==0,1,0)) %>%
  ungroup() 


# find changes number of doctors on exec team
doctors_over_time <- executive_data %>%
  mutate(doctor = ifelse(title %in% c("md", "do"), 1, 0)) %>%
  group_by(ein, year) %>%
  mutate(num_doctors = sum(doctor)) %>%
  ungroup() %>%
  distinct(ein, year, num_doctors)

doctors_over_time <- doctors_over_time %>%
  group_by(ein) %>%
  arrange(year) %>%
  mutate(lag_num_doctors = dplyr::lag(num_doctors)) %>%
  ungroup() %>%
  mutate(num_doc_change = ifelse(num_doctors!=lag_num_doctors, 1, 0)) %>%
  select(ein, year, num_doctors, num_doc_change)

executive_data <- executive_data %>%
  left_join(doctors_over_time, by=c("ein", "year")) %>%
  mutate(no_num_md_change_2010_2014 = ifelse(num_doc_change==1 & year %in% c(2010,2011,2012,2013,2014),1,NA)) %>%
  group_by(ein) %>%
  mutate(no_num_md_change_2010_2014 = ifelse(sum(no_num_md_change_2010_2014,na.rm=T)==0,1,0)) %>%
  ungroup() 

# find changes in propensity to hire a doctor at all
doctors_over_time <- executive_data %>%
  group_by(ein, year) %>%
  ungroup() %>%
  distinct(ein, year, num_doctors) %>%
  mutate(any_doctor = ifelse(num_doctors>0,1,0)) %>%
  select(-num_doctors)

doctors_over_time <- doctors_over_time %>%
  group_by(ein) %>%
  arrange(year) %>%
  mutate(lag_any_doctors = dplyr::lag(any_doctor)) %>%
  ungroup() %>%
  mutate(md_change = ifelse(any_doctor!=lag_any_doctors, 1, 0)) %>%
  select(ein, year, md_change)

executive_data <- executive_data %>%
  left_join(doctors_over_time, by=c("ein", "year")) %>%
  mutate(no_md_change_2010_2014 = ifelse(md_change==1 & year %in% c(2010,2011,2012,2013,2014),1,NA)) %>%
  group_by(ein) %>%
  mutate(no_md_change_2010_2014 = ifelse(sum(no_md_change_2010_2014,na.rm=T)==0,1,0)) %>%
  ungroup() 

# find changes number of execs
execs_over_time <- executive_data %>%
  distinct(ein, year, num_execs) %>%
  group_by(ein) %>%
  arrange(year) %>%
  mutate(lag_num_execs = dplyr::lag(num_execs)) %>%
  ungroup() %>%
  mutate(num_exec_change = ifelse(abs(lag_num_execs-num_execs>2), 1, 0)) %>%
  select(ein, year, num_exec_change)

executive_data <- executive_data %>%
  left_join(execs_over_time, by=c("ein", "year")) %>%
  mutate(no_num_execs_change_2010_2014 = ifelse(num_exec_change==1 & year %in% c(2010,2011,2012,2013,2014),1,NA)) %>%
  group_by(ein) %>%
  mutate(no_num_execs_change_2010_2014 = ifelse(sum(no_num_execs_change_2010_2014,na.rm=T)==0,1,0)) %>%
  ungroup() 

# figure out whether ceo changed over time
detect_changes <- executive_data %>% 
  filter(ceo==1) %>%
  group_by(ein, year) %>% 
  dplyr::summarize(list_of_ids = paste(sort(unique(id)),collapse=", ")) %>%
  group_by(ein) %>%
  arrange(year) %>%
  mutate(lag_list = dplyr::lag(list_of_ids)) %>%
  ungroup %>%
  mutate(ceo_change = ifelse(list_of_ids!=lag_list, 1, 0)) 

# merge back to executive data
executive_data <- executive_data %>%
  left_join(detect_changes, by=c("ein", "year")) %>%
  mutate(no_ceo_change_2010_2014 = ifelse(ceo_change==1 & year %in% c(2010,2011,2012,2013,2014),1,NA)) %>%
  group_by(ein) %>%
  mutate(no_ceo_change_2010_2014 = ifelse(sum(no_ceo_change_2010_2014, na.rm=T)==0,1,0)) %>%
  ungroup()

# create variable for ever has CMO
executive_data <- executive_data %>%
  group_by(ein) %>%
  mutate(ever_cmo=ifelse(sum(cmo)>0,1,0)) %>%
  ungroup()



# make a separate data set of ein, name, title, and years in data to be used for physician name matching
# only keep the ones with no MD changes from 2010-2014
names_data <- executive_data_filled %>%
  filter(no_md_changes_2010_2014==1) %>%
  group_by(ein, name, title) %>%
  summarize(years = paste(sort(unique(year)), collapse=", ")) %>%
  ungroup()

saveRDS(names_data, paste0(created_data_path, "names_data.rds"))

# aggregate to the hospital level
ein_leadership_changes_data <- executive_data %>%
  distinct(ein, year, num_execs, num_doctors, ever_cmo, any_change, no_changes_2010_2014, num_doc_change, no_num_md_change_2010_2014,
           md_change, no_md_change_2010_2014, num_exec_change, no_num_execs_change_2010_2014, ceo_change, no_ceo_change_2010_2014)

saveRDS(ein_leadership_changes_data, paste0(created_data_path, "ein_leadership_changes_data.rds"))

num <- ein_leadership_changes_data %>%
  distinct(ein)
  # 911







