library(readr)
library(dplyr)
library(tidyr)
library(knitr)
library(kableExtra)
library(weights)
library(ggplot2)

# create graphs exploring differences in outcomes between nonprofit and for-profit ownership status

hospital_data <- read_rds(paste0(created_data_path, "all_hospital_data.rds")) %>%
  filter(year!=2016 & year!=2017)

# create "ever has MD" variable
hospital_data <- hospital_data %>%
  group_by(ID) %>%
  mutate(ever_has_md = ifelse(sum(has_any_md, na.rm=T)>0,1,0)) %>%
  ungroup() 

# create variable for "has leadership info"
hospital_data <- hospital_data %>%
  group_by(ID) %>%
  mutate(has_leader_info = ifelse(sum(num_execs,na.rm=T)>0,1,0)) %>%
  ungroup()

# compare weighted avg readmissions for: for profit, NFP w/ leadership info, NFP w/out leadership info
hospital_data <- hospital_data %>%
  mutate(profit_leadership_status = ifelse(profit_status=="forprofit","forprofit",NA)) %>%
  mutate(profit_leadership_status = ifelse(profit_status=="nonprofit" & has_leader_info==1,"nonprofit w/ leader info",profit_leadership_status)) %>%
  mutate(profit_leadership_status = ifelse(profit_status=="nonprofit" & has_leader_info==0,"nonprofit w/out leader info",profit_leadership_status)) 

sum <- hospital_data %>%
  group_by(year,profit_leadership_status) %>%
  summarise_at(c("weightedavg_read"), list(mean), na.rm=T) %>%
  filter(year!=2016 & year!=2017)

ggplot(sum, aes(x=year, y=weightedavg_read, color=profit_leadership_status)) + geom_line()

# compare weighted avg readmissions for: for profit, NFP w/ stable team, NFP w/out stable team
hospital_data <- hospital_data %>%
  group_by(MCRNUM) %>%
  fill(no_md_change_2010_2014, .direction="downup") %>%
  ungroup() %>%
  mutate(profit_stability_status = ifelse(profit_status=="forprofit","forprofit",NA)) %>%
  mutate(profit_stability_status = ifelse(profit_status=="nonprofit" & no_md_change_2010_2014==1,"nonprofit w/ stable team",profit_stability_status)) %>%
  mutate(profit_stability_status = ifelse(profit_status=="nonprofit" & no_md_change_2010_2014==0,"nonprofit w/out stable team",profit_stability_status)) 

observe <- hospital_data %>%
  filter(is.na(profit_md_status)) %>%
  filter(!(profit_status=="nonprofit" & has_leader_info==0)) 

sum <- hospital_data %>%
  filter(!(profit_status=="nonprofit" & has_leader_info==0)) %>%
  group_by(year,profit_stability_status) %>%
  summarise_at(c("weightedavg_read"), list(mean), na.rm=T) %>%
  filter(year!=2016 & year!=2017) %>%
  filter(!is.na(profit_stability_status))

ggplot(sum, aes(x=year, y=weightedavg_read, color=profit_stability_status)) + geom_line()

# compare weighted average readmissions for: for-profit, NFP w/ MD. NFP w/out MD
hospital_data <- hospital_data %>%
  mutate(profit_md_status = ifelse(profit_status=="forprofit","forprofit",NA)) %>%
  mutate(profit_md_status = ifelse(profit_status=="nonprofit" & ever_has_md==1,"nonprofit w/ MD",profit_md_status)) %>%
  mutate(profit_md_status = ifelse(profit_status=="nonprofit" & ever_has_md==0,"nonprofit w/out MD",profit_md_status)) 

sum <- hospital_data %>%
  filter(!(profit_status=="nonprofit" & has_leader_info==0)) %>%
  group_by(year,profit_md_status) %>%
  summarise_at(c("weightedavg_read"), list(mean), na.rm=T) %>%
  filter(year!=2016 & year!=2017) 

ggplot(sum, aes(x=year, y=weightedavg_read, color=profit_md_status)) + geom_line()
