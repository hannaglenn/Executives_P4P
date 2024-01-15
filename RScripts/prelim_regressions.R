library(readr)
library(dplyr)
library(fixest)
library(ggplot2)

# Read in hospital_data created in "create_hospital_data.R"
hospital_data <- readRDS(paste0(created_data_path, "penalized_hospital_data(temp).rds"))

# create min year penalized variable
minyr_penalized <- hospital_data %>%
  mutate(year_penalized = ifelse(penalized_HC==1, year, NA)) %>%
  filter(!is.na(year_penalized)) %>%
  group_by(ID) %>%
  mutate(minyr_pen = min(year)) %>%
  ungroup() %>%
  distinct(ID, minyr_pen)

hospital_data <- hospital_data %>%
  left_join(minyr_penalized, by="ID")

# limit to hospitals who do not change their leadership team structure in the time frame I care about
# create sample based on not changing MD leadership structure from 2010-2014 (my ideal sample I think)
regression_data <- hospital_data %>%
  filter(no_md_changes_2010_2014==1) %>%
  mutate(has_any_md = ifelse(total_docs>0, 1, 0)) %>%
  select(ID, year, heartattack_mortality, heartattack_readmission, pneum_mortality, pneum_readmission, heartfailure_mortality, heartfailure_readmission, has_any_md, minyr_pen, ever_pen_ha, ever_pen_hf, ever_pen_pnem) %>%
  mutate(post = ifelse(year>=`minyr_pen`, 1, 0))

# graph the diff in diff first
graph_did <- regression_data %>%
  group_by(has_any_md, year) %>%
  summarise_at(c("heartattack_mortality", "heartattack_readmission", "pneum_mortality", "pneum_readmission", "heartfailure_mortality", "heartfailure_readmission"), list(mean), na.rm=T) %>%
  filter(year!=2008) %>%
  mutate(has_any_md = as.factor(has_any_md))

graph_did_read <- pivot_longer(graph_did, c("heartattack_mortality", "heartattack_readmission", "pneum_mortality", "pneum_readmission", "heartfailure_mortality", "heartfailure_readmission"), names_to = "outcome", values_to = "rate") %>%
  filter(outcome %in% c("pneum_readmission", "heartfailure_readmission", "heartattack_readmission"))

graph_did_mort <- pivot_longer(graph_did, c("heartattack_mortality", "heartattack_readmission", "pneum_mortality", "pneum_readmission", "heartfailure_mortality", "heartfailure_readmission"), names_to = "outcome", values_to = "rate") %>%
  filter(outcome %in% c("pneum_mortality", "heartfailure_mortality", "heartattack_mortality"))

ggplot(graph_did_read, aes(x=year, y=rate, color=outcome, linetype=has_any_md)) + geom_vline(xintercept = 2012) + geom_line(size=.75) + theme_bw() + 
  ylim(15,30) + ggtitle("average readmission rates over time, all penalized hospitals")

ggplot(graph_did_mort, aes(x=year, y=rate, color=outcome, linetype=has_any_md)) + geom_vline(xintercept = 2012) + geom_line(size=.75) + theme_bw() + 
  ylim(0,20) + ggtitle("average mortality rates over time, all penalized hospitals")




# run diff in diff 
pneum_mort_reg <- fixest::feols(pneum_mortality ~ has_any_md + post + has_any_md*post, data=regression_data)
pneum_read_reg <- fixest::feols(pneum_readmission ~ has_any_md + post + has_any_md*post, data=regression_data)
hf_mort_reg <- fixest::feols(heartfailure_mortality ~ has_any_md + post + has_any_md*post, data=regression_data)
hf_read_reg <- fixest::feols(heartfailure_readmission ~ has_any_md + post + has_any_md*post, data=regression_data)
ha_mort_reg <- fixest::feols(heartattack_mortality ~ has_any_md + post + has_any_md*post, data=regression_data)
ha_read_reg <- fixest::feols(heartattack_readmission ~ has_any_md + post + has_any_md*post, data=regression_data)

summary(pneum_mort_reg)
summary(pneum_read_reg)
summary(hf_mort_reg)
summary(hf_read_reg)
summary(ha_mort_reg)
summary(ha_read_reg)

# add fixed effects
pneum_mort_reg <- fixest::feols(pneum_mortality ~ has_any_md + post + has_any_md*post | ID, data=regression_data)
pneum_read_reg <- fixest::feols(pneum_readmission ~ has_any_md + post + has_any_md*post | ID, data=regression_data)
hf_mort_reg <- fixest::feols(heartfailure_mortality ~ has_any_md + post + has_any_md*post | ID, data=regression_data)
hf_read_reg <- fixest::feols(heartfailure_readmission ~ has_any_md + post + has_any_md*post | ID, data=regression_data)
ha_mort_reg <- fixest::feols(heartattack_mortality ~ has_any_md + post + has_any_md*pos | ID, data=regression_data)
ha_read_reg <- fixest::feols(heartattack_readmission ~ has_any_md + post + has_any_md*post | ID, data=regression_data)

summary(pneum_mort_reg)
summary(pneum_read_reg)
summary(hf_mort_reg)
summary(hf_read_reg)
summary(ha_mort_reg)
summary(ha_read_reg)


