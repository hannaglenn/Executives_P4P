library(readr)
library(dplyr)
library(tidyr)
library(knitr)
library(kableExtra)
library(weights)
library(ggplot2)
library(maps)
library(mapdata)
library(cdlTools)
library(tibble)

# read in hospital data
hospital_data <- read_rds(paste0(created_data_path, "all_hospital_data.rds"))

# put uncompensated care in millions
hospital_data$uncomp_care <- abs(hospital_data$uncomp_care)
hospital_data <- hospital_data %>%
  mutate(uncomp_care=uncomp_care/1000000) 


# List of tables:
# Table 1: mean, SD, min, max for the whole sample
# Table 2: means for different sub-samples

### TABLE 1: mean, sd, min, max for the whole sample
stats <- hospital_data %>% 
  summarise_at(c("Number Beds"="beds",
                 "Penalized for AMI"="ever_pen_ha", "Penalized for HF"="ever_pen_hf",
                 "Penalized for Pneumonia"="ever_pen_pnem",
                 "Weighted Avg. Readmission Rate"="weightedavg_read",
                 "HF Readmission Rate" = "rate_heartfailure_readmission",
                 "AMI Readmission Rate" = "rate_heartattack_readmission",
                 "Pneum. Readmission Rate" = "rate_pneum_readmission",
                 "Weighted Avg. Mortality Rate"="weightedavg_mort",
                 "HF Mortality Rate" = "rate_heartfailure_mortality",
                 "AMI Mortality Rate" = "rate_heartattack_mortality",
                 "Pneum. Mortality Rate" = "rate_pneum_mortality",
                 "Num. Patients HF" = "patnum_heartfailure_readmission",
                 "Num. Patients AMI" = "patnum_heartattack_readmission",
                 "Num. Patients Pneumonia" = "patnum_pneum_readmission",
                 "Uncomp. Care (Millions)"="uncomp_care",
                 "Case Mix Index"="cmiv"), 
               list(m=mean,sd=sd, min=min, max=max), na.rm=TRUE) %>%
  mutate_if(is.numeric, ~ifelse(abs(.)==Inf,NA,.))  %>%
  gather(key=var,value=value) %>%
  extract(col="var",into=c("variable", "statistic"), regex=("(.*)_(.*)$")) %>%
  spread(key=statistic, value=value) %>%
  relocate(variable,m,sd,min,max) 

n <- hospital_data %>%
  distinct(ID) %>%
  nrow()

stats <- stats %>%
  add_row(variable="Num. Hospitals", m=n)

knitr::kable(stats[c(9,6,7,8,10,11,12,17,2,5,14,16,1,4,13,15,3,18),],
             row.names = FALSE,
             format="latex",
             table.envir="table",
             col.names=c("Variable","Mean","Std. Dev.", "Min","Max"),
             digits=2,
             caption="\\label{sumstats} Summary Statistics",
             booktabs=TRUE,
             escape=F,
             align=c("l","c","c","c","c","c"),
             position="h") %>%
  kable_styling(full_width=F) %>%
  pack_rows(index = c("Hospital Characteristics" = 4, "Penalty Variables" = 3, "Readmission Outcome Variables" = 4, "Mortality Outcome Variables" = 4, 
                      "Other Outcome Variables"=2, " " = 1))

### TABLE 2: table of means for each subsample of hospitals
n_FP <- hospital_data %>%
  filter(profit_status=="forprofit") %>%
  distinct(ID) %>%
  nrow()
FP_stats <- hospital_data %>%
  filter(profit_status=="forprofit") %>%
  summarise_at(c("Number Beds"="beds",
                 "Penalized for AMI"="ever_pen_ha", "Penalized for HF"="ever_pen_hf",
                 "Penalized for Pneumonia"="ever_pen_pnem",
                 "Weighted Avg. Readmission Rate"="weightedavg_read",
                 "HF Readmission Rate" = "rate_heartfailure_readmission",
                 "AMI Readmission Rate" = "rate_heartattack_readmission",
                 "Pneum. Readmission Rate" = "rate_pneum_readmission",
                 "Weighted Avg. Mortality Rate"="weightedavg_mort",
                 "HF Mortality Rate" = "rate_heartfailure_mortality",
                 "AMI Mortality Rate" = "rate_heartattack_mortality",
                 "Pneum. Mortality Rate" = "rate_pneum_mortality",
                 "Num. Patients HF" = "patnum_heartfailure_readmission",
                 "Num. Patients AMI" = "patnum_heartattack_readmission",
                 "Num. Patients Pneumonia" = "patnum_pneum_readmission",
                 "Uncomp. Care (Millions)"="uncomp_care",
                 "Case Mix Index"="cmiv"), 
               list(m=mean), na.rm=TRUE) %>%
  mutate_if(is.numeric, ~ifelse(abs(.)==Inf,NA,.))  %>%
  gather(key=var,value=value) %>%
  extract(col="var",into=c("variable", "statistic"), regex=("(.*)_(.*)$")) %>%
  spread(key=statistic, value=value) %>%
  relocate(variable,m) %>%
  add_row(variable="Num. Hospitals", m=n_FP)

n_NP <- hospital_data %>%
  filter(profit_status=="nonprofit") %>%
  distinct(ID) %>%
  nrow()
NP_stats <- hospital_data %>%
  filter(profit_status=="nonprofit") %>%
  summarise_at(c("Number Beds"="beds",
                 "Penalized for AMI"="ever_pen_ha", "Penalized for HF"="ever_pen_hf",
                 "Penalized for Pneumonia"="ever_pen_pnem",
                 "Weighted Avg. Readmission Rate"="weightedavg_read",
                 "HF Readmission Rate" = "rate_heartfailure_readmission",
                 "AMI Readmission Rate" = "rate_heartattack_readmission",
                 "Pneum. Readmission Rate" = "rate_pneum_readmission",
                 "Weighted Avg. Mortality Rate"="weightedavg_mort",
                 "HF Mortality Rate" = "rate_heartfailure_mortality",
                 "AMI Mortality Rate" = "rate_heartattack_mortality",
                 "Pneum. Mortality Rate" = "rate_pneum_mortality",
                 "Num. Patients HF" = "patnum_heartfailure_readmission",
                 "Num. Patients AMI" = "patnum_heartattack_readmission",
                 "Num. Patients Pneumonia" = "patnum_pneum_readmission",
                 "Uncomp. Care (Millions)"="uncomp_care",
                 "Case Mix Index"="cmiv"), 
               list(m=mean), na.rm=TRUE) %>%
  mutate_if(is.numeric, ~ifelse(abs(.)==Inf,NA,.))  %>%
  gather(key=var,value=value) %>%
  extract(col="var",into=c("variable", "statistic"), regex=("(.*)_(.*)$")) %>%
  spread(key=statistic, value=value) %>%
  relocate(variable,m) %>%
  add_row(variable="Num. Hospitals", m=n_NP)

n_NP_md <- hospital_data %>%
  filter(ever_has_md==1 & no_num_md_change_2010_2014==1) %>%
  distinct(ID) %>%
  nrow()
NP_md_stats <- hospital_data %>%
  filter(ever_has_md==1 & no_num_md_change_2010_2014==1) %>%
  summarise_at(c("Number Beds"="beds",
                 "Penalized for AMI"="ever_pen_ha", "Penalized for HF"="ever_pen_hf",
                 "Penalized for Pneumonia"="ever_pen_pnem",
                 "Weighted Avg. Readmission Rate"="weightedavg_read",
                 "HF Readmission Rate" = "rate_heartfailure_readmission",
                 "AMI Readmission Rate" = "rate_heartattack_readmission",
                 "Pneum. Readmission Rate" = "rate_pneum_readmission",
                 "Weighted Avg. Mortality Rate"="weightedavg_mort",
                 "HF Mortality Rate" = "rate_heartfailure_mortality",
                 "AMI Mortality Rate" = "rate_heartattack_mortality",
                 "Pneum. Mortality Rate" = "rate_pneum_mortality",
                 "Num. Patients HF" = "patnum_heartfailure_readmission",
                 "Num. Patients AMI" = "patnum_heartattack_readmission",
                 "Num. Patients Pneumonia" = "patnum_pneum_readmission",
                 "Uncomp. Care (Millions)"="uncomp_care",
                 "Case Mix Index"="cmiv"), 
               list(m=mean), na.rm=TRUE) %>%
  mutate_if(is.numeric, ~ifelse(abs(.)==Inf,NA,.))  %>%
  gather(key=var,value=value) %>%
  extract(col="var",into=c("variable", "statistic"), regex=("(.*)_(.*)$")) %>%
  spread(key=statistic, value=value) %>%
  relocate(variable,m) %>%
  add_row(variable="Num. Hospitals", m=n_NP_md)

n_NP_nomd <- hospital_data %>%
  filter(ever_has_md==0 & no_num_md_change_2010_2014==1) %>%
  distinct(ID) %>%
  nrow()
NP_nomd_stats <- hospital_data %>%
  filter(ever_has_md==0 & no_num_md_change_2010_2014==1) %>%
  summarise_at(c("Number Beds"="beds",
                 "Penalized for AMI"="ever_pen_ha", "Penalized for HF"="ever_pen_hf",
                 "Penalized for Pneumonia"="ever_pen_pnem",
                 "Weighted Avg. Readmission Rate"="weightedavg_read",
                 "HF Readmission Rate" = "rate_heartfailure_readmission",
                 "AMI Readmission Rate" = "rate_heartattack_readmission",
                 "Pneum. Readmission Rate" = "rate_pneum_readmission",
                 "Weighted Avg. Mortality Rate"="weightedavg_mort",
                 "HF Mortality Rate" = "rate_heartfailure_mortality",
                 "AMI Mortality Rate" = "rate_heartattack_mortality",
                 "Pneum. Mortality Rate" = "rate_pneum_mortality",
                 "Num. Patients HF" = "patnum_heartfailure_readmission",
                 "Num. Patients AMI" = "patnum_heartattack_readmission",
                 "Num. Patients Pneumonia" = "patnum_pneum_readmission",
                 "Uncomp. Care (Millions)"="uncomp_care",
                 "Case Mix Index"="cmiv"), 
               list(m=mean), na.rm=TRUE) %>%
  mutate_if(is.numeric, ~ifelse(abs(.)==Inf,NA,.))  %>%
  gather(key=var,value=value) %>%
  extract(col="var",into=c("variable", "statistic"), regex=("(.*)_(.*)$")) %>%
  spread(key=statistic, value=value) %>%
  relocate(variable,m) %>%
  add_row(variable="Num. Hospitals", m=n_NP_nomd)

stats <- FP_stats %>%
  left_join(NP_stats, by="variable") %>%
  left_join(NP_md_stats, by="variable") %>%
  left_join(NP_nomd_stats, by="variable")

knitr::kable(stats[c(9,6,7,8,10,11,12,17,2,5,14,16,1,4,13,15,3,18),],
             row.names = FALSE,
             format="latex",
             table.envir="table",
             col.names=c("Variable","For-Profit", "NonProfit", "NonProfit w/ MD", "NonProfit w/out MD"),
             digits=2,
             caption="\\label{tab:sumstats_samples} Summary Statistics by Sub-Sample",
             booktabs=TRUE,
             escape=F,
             align=c("l","c","c","c","c","c"),
             position="h") %>%
  kable_styling(full_width=F) %>%
  pack_rows(index = c("Hospital Characteristics" = 4, "Penalty Variables" = 3, "Readmission Outcome Variables" = 4, "Mortality Outcome Variables" = 4, 
                      "Other Outcome Variables"=2, " " = 1))





### TABLE 1 #########
noMDchg_doc <- hospital_data %>%
  filter(no_md_change_2010_2014==1 & has_any_md==1)
noMDchg_nodoc <- hospital_data %>%
  filter(no_md_change_2010_2014==1 & has_any_md==0)

n_doc <- noMDchg_doc %>%
  distinct(ID) %>%
  nrow()
n_nodoc <- noMDchg_nodoc %>%
  distinct(ID) %>%
  nrow()

p_beds <- data.frame(c("Number Beds"), c(round(t.test(noMDchg_doc$beds, noMDchg_nodoc$beds)$p.value,4))) %>%
  rename("variable" = 1, "p-value" = 2)
p_pen_AMI <- data.frame(c("Penalized for AMI"), c(round(t.test(noMDchg_doc$ever_pen_ha, noMDchg_nodoc$ever_pen_ha)$p.value,4))) %>%
  rename("variable" = 1, "p-value" = 2)
p_pen_hf <- data.frame(c("Penalized for HF"), c(round(t.test(noMDchg_doc$ever_pen_hf, noMDchg_nodoc$ever_pen_hf)$p.value,4)))%>%
  rename("variable" = 1, "p-value" = 2)
p_pen_pneum <- data.frame(c("Penalized for Pneumonia"), c(round(t.test(noMDchg_doc$ever_pen_pnem, noMDchg_nodoc$ever_pen_pnem)$p.value,4)))%>%
  rename("variable" = 1, "p-value" = 2)
p_wa_read <- data.frame(c("Weighted Avg. Readmission Rate"), c(round(t.test(noMDchg_doc$weightedavg_read, noMDchg_nodoc$weightedavg_read)$p.value,4)))%>%
  rename("variable" = 1, "p-value" = 2)
p_hf_read <- data.frame(c("HF Readmission Rate"), c(round(t.test(noMDchg_doc$rate_heartfailure_readmission, noMDchg_nodoc$rate_heartfailure_readmission)$p.value,4)))%>%
  rename("variable" = 1, "p-value" = 2)
p_ami_read <- data.frame(c("AMI Readmission Rate"), c(round(t.test(noMDchg_doc$rate_heartattack_readmission, noMDchg_nodoc$rate_heartattack_readmission)$p.value,4)))%>%
  rename("variable" = 1, "p-value" = 2)
p_pneum_read <- data.frame(c("Pneum. Readmission Rate"), c(round(t.test(noMDchg_doc$rate_pneum_readmission, noMDchg_nodoc$rate_pneum_readmission)$p.value,4)))%>%
  rename("variable" = 1, "p-value" = 2)
p_wa_mort <- data.frame(c("Weighted Avg. Mortality Rate"), c(round(t.test(noMDchg_doc$weightedavg_mort, noMDchg_nodoc$weightedavg_mort)$p.value,4)))%>%
  rename("variable" = 1, "p-value" = 2)
p_hf_mort <- data.frame(c("HF Mortality Rate"), c(round(t.test(noMDchg_doc$rate_heartfailure_mortality, noMDchg_nodoc$rate_heartfailure_mortality)$p.value,4)))%>%
  rename("variable" = 1, "p-value" = 2)
p_ami_mort <- data.frame(c("AMI Mortality Rate"), c(round(t.test(noMDchg_doc$rate_heartattack_mortality, noMDchg_nodoc$rate_heartattack_mortality)$p.value,4)))%>%
  rename("variable" = 1, "p-value" = 2)
p_pneum_mort <- data.frame(c("Pneum. Mortality Rate"), c(round(t.test(noMDchg_doc$rate_pneum_mortality, noMDchg_nodoc$rate_pneum_mortality)$p.value,4)))%>%
  rename("variable" = 1, "p-value" = 2)
p_num_hf <- data.frame(c("Num. Patients HF"), c(round(t.test(noMDchg_doc$patnum_heartfailure_readmission, noMDchg_nodoc$patnum_heartfailure_readmission)$p.value,4)))%>%
  rename("variable" = 1, "p-value" = 2)
p_num_ami <- data.frame(c("Num. Patients AMI"), c(round(t.test(noMDchg_doc$patnum_heartattack_readmission, noMDchg_nodoc$patnum_heartattack_readmission)$p.value,4)))%>%
  rename("variable" = 1, "p-value" = 2)
p_num_pneum <- data.frame(c("Num. Patients Pneumonia"), c(round(t.test(noMDchg_doc$patnum_pneum_readmission, noMDchg_nodoc$patnum_pneum_readmission)$p.value,4)))%>%
  rename("variable" = 1, "p-value" = 2)

pvalues <- rbind(p_beds, p_pen_AMI, p_pen_hf, p_pen_pneum, p_wa_read, p_hf_read, p_ami_read, p_pneum_read, p_wa_mort, p_hf_mort, p_ami_mort, p_pneum_mort, 
      p_num_hf, p_num_ami, p_num_pneum)

noMDchg_doc_stats <- hospital_data %>% 
  filter(no_md_change_2010_2014==1) %>%
  filter(has_any_md==1) %>%
  summarise_at(c("Number Beds"="beds",
                 "Penalized for AMI"="ever_pen_ha", "Penalized for HF"="ever_pen_hf",
                 "Penalized for Pneumonia"="ever_pen_pnem",
                 "Weighted Avg. Readmission Rate"="weightedavg_read",
                 "HF Readmission Rate" = "rate_heartfailure_readmission",
                 "AMI Readmission Rate" = "rate_heartattack_readmission",
                 "Pneum. Readmission Rate" = "rate_pneum_readmission",
                 "Weighted Avg. Mortality Rate"="weightedavg_mort",
                 "HF Mortality Rate" = "rate_heartfailure_mortality",
                 "AMI Mortality Rate" = "rate_heartattack_mortality",
                 "Pneum. Mortality Rate" = "rate_pneum_mortality",
                 "Num. Patients HF" = "patnum_heartfailure_readmission",
                 "Num. Patients AMI" = "patnum_heartattack_readmission",
                 "Num. Patients Pneumonia" = "patnum_pneum_readmission"), 
               list(m=mean,sd=sd), na.rm=TRUE) %>%
  mutate_if(is.numeric, ~ifelse(abs(.)==Inf,NA,.))  %>%
  gather(key=var,value=value) %>%
  extract(col="var",into=c("variable", "statistic"), regex=("(.*)_(.*)$")) %>%
  spread(key=statistic, value=value) %>%
  relocate(variable,m,sd) %>%
  rename(m_doc=m, sd_doc=sd)

noMDchg_nodoc_stats <- hospital_data %>% 
  filter(no_md_change_2010_2014==1) %>%
  filter(has_any_md==0) %>%
  summarise_at(c("Number Beds"="beds",
                 "Penalized for AMI"="ever_pen_ha", "Penalized for HF"="ever_pen_hf",
                 "Penalized for Pneumonia"="ever_pen_pnem",
                 "Weighted Avg. Readmission Rate"="weightedavg_read",
                 "HF Readmission Rate" = "rate_heartfailure_readmission",
                 "AMI Readmission Rate" = "rate_heartattack_readmission",
                 "Pneum. Readmission Rate" = "rate_pneum_readmission",
                 "Weighted Avg. Mortality Rate"="weightedavg_mort",
                 "HF Mortality Rate" = "rate_heartfailure_mortality",
                 "AMI Mortality Rate" = "rate_heartattack_mortality",
                 "Pneum. Mortality Rate" = "rate_pneum_mortality",
                 "Num. Patients HF" = "patnum_heartfailure_readmission",
                 "Num. Patients AMI" = "patnum_heartattack_readmission",
                 "Num. Patients Pneumonia" = "patnum_pneum_readmission"), 
               list(m=mean,sd=sd), na.rm=TRUE) %>%
  mutate_if(is.numeric, ~ifelse(abs(.)==Inf,NA,.))  %>%
  gather(key=var,value=value) %>%
  extract(col="var",into=c("variable", "statistic"), regex=("(.*)_(.*)$")) %>%
  spread(key=statistic, value=value) %>%
  relocate(variable,m,sd) 

stats <- merge(noMDchg_doc_stats, noMDchg_nodoc_stats)
stats <- merge(stats, pvalues) %>%
  add_row(variable = "Num. Hospitals", sd_doc = n_doc, sd = n_nodoc)

row.names(stats) <- NULL

knitr::kable(stats[c(8,5,6,7,9,10,11,15,2,4,13,14,1,3,12,16),],
             row.names = FALSE,
             format="latex",
             table.envir="table",
             col.names=c("Variable","Mean","Std. Dev.", "Mean","Std. Dev.", "p-value"),
             digits=2,
             caption="Summary Statistics",
             booktabs=TRUE,
             escape=F,
             align=c("l","c","c","c","c","c"),
             position="h") %>%
  kable_styling(full_width=F) %>%
  pack_rows(index = c("Hospital Characteristics" = 4, "Penalty Variables" = 3, "Readmission Outcome Variables" = 4, "Mortality Outcome Variables" = 4, " " = 1)) %>%
  add_header_above(c(" ", "with clinical experience" = 2, "without clinical experience" = 2, " "))


### TABLE 2 #########
noMDchg_doc <- hospital_data %>%
  filter(no_md_changes_2010_2014==1 & has_any_md==1 & matched==1)
noMDchg_nodoc <- hospital_data %>%
  filter(no_md_changes_2010_2014==1 & has_any_md==0 & matched==1)

n_doc <- noMDchg_doc %>%
  distinct(MCRNUM) %>%
  nrow()
n_nodoc <- noMDchg_nodoc %>%
  distinct(MCRNUM) %>%
  nrow()

p_beds <- data.frame(c("Number Beds"), round(c(wtd.t.test(x=noMDchg_doc$beds, y=noMDchg_nodoc$beds, weight = noMDchg_doc$weights, weighty = noMDchg_nodoc$weights)$coefficients[[3]]),4)) %>% 
  rename("variable" = 1, "p-value" = 2)
p_pen_AMI <- data.frame(c("Penalized for AMI"), round(c(wtd.t.test(x=noMDchg_doc$ever_pen_ha, y=noMDchg_nodoc$ever_pen_ha, weight = noMDchg_doc$weights, weighty = noMDchg_nodoc$weights)$coefficients[[3]]),4)) %>% 
  rename("variable" = 1, "p-value" = 2)
p_pen_hf <- data.frame(c("Penalized for HF"), round(c(wtd.t.test(x=noMDchg_doc$ever_pen_hf, y=noMDchg_nodoc$ever_pen_hf, weight = noMDchg_doc$weights, weighty = noMDchg_nodoc$weights)$coefficients[[3]]),4)) %>% 
  rename("variable" = 1, "p-value" = 2)
p_pen_pneum <- data.frame(c("Penalized for Pneumonia"), round(c(wtd.t.test(x=noMDchg_doc$ever_pen_pnem, y=noMDchg_nodoc$ever_pen_pnem, weight = noMDchg_doc$weights, weighty = noMDchg_nodoc$weights)$coefficients[[3]]),4)) %>% 
  rename("variable" = 1, "p-value" = 2)
p_wa_read <- data.frame(c("Weighted Avg. Readmission Rate"), round(c(wtd.t.test(x=noMDchg_doc$weightedavg_read, y=noMDchg_nodoc$weightedavg_read, weight = noMDchg_doc$weights, weighty = noMDchg_nodoc$weights)$coefficients[[3]]),4)) %>% 
  rename("variable" = 1, "p-value" = 2)
p_hf_read <- data.frame(c("HF Readmission Rate"), round(c(wtd.t.test(x=noMDchg_doc$rate_heartfailure_readmission, y=noMDchg_nodoc$rate_heartfailure_readmission, weight = noMDchg_doc$weights, weighty = noMDchg_nodoc$weights)$coefficients[[3]]),4)) %>% 
  rename("variable" = 1, "p-value" = 2)
p_ami_read <- data.frame(c("AMI Readmission Rate"), round(c(wtd.t.test(x=noMDchg_doc$rate_heartattack_readmission, y=noMDchg_nodoc$rate_heartattack_readmission, weight = noMDchg_doc$weights, weighty = noMDchg_nodoc$weights)$coefficients[[3]]),4)) %>% 
  rename("variable" = 1, "p-value" = 2)
p_pneum_read <- data.frame(c("Pneum. Readmission Rate"), round(c(wtd.t.test(x=noMDchg_doc$rate_pneum_readmission, y=noMDchg_nodoc$rate_pneum_readmission, weight = noMDchg_doc$weights, weighty = noMDchg_nodoc$weights)$coefficients[[3]]),4)) %>% 
  rename("variable" = 1, "p-value" = 2)
p_wa_mort <- data.frame(c("Weighted Avg. Mortality Rate"), round(c(wtd.t.test(x=noMDchg_doc$weightedavg_mort, y=noMDchg_nodoc$weightedavg_mort, weight = noMDchg_doc$weights, weighty = noMDchg_nodoc$weights)$coefficients[[3]]),4)) %>% 
  rename("variable" = 1, "p-value" = 2)
p_hf_mort <- data.frame(c("HF Mortality Rate"), round(c(wtd.t.test(x=noMDchg_doc$rate_heartfailure_mortality, y=noMDchg_nodoc$rate_heartfailure_mortality, weight = noMDchg_doc$weights, weighty = noMDchg_nodoc$weights)$coefficients[[3]]),4)) %>% 
  rename("variable" = 1, "p-value" = 2)
p_ami_mort <- data.frame(c("AMI Mortality Rate"), round(c(wtd.t.test(x=noMDchg_doc$rate_heartattack_mortality, y=noMDchg_nodoc$rate_heartattack_mortality, weight = noMDchg_doc$weights, weighty = noMDchg_nodoc$weights)$coefficients[[3]]),4)) %>% 
  rename("variable" = 1, "p-value" = 2)
p_pneum_mort <- data.frame(c("Pneum. Mortality Rate"), round(c(wtd.t.test(x=noMDchg_doc$rate_pneum_mortality, y=noMDchg_nodoc$rate_pneum_mortality, weight = noMDchg_doc$weights, weighty = noMDchg_nodoc$weights)$coefficients[[3]]),4)) %>% 
  rename("variable" = 1, "p-value" = 2)
p_num_hf <- data.frame(c("Num. Patients HF"), round(c(wtd.t.test(x=noMDchg_doc$patnum_heartfailure_readmission, y=noMDchg_nodoc$patnum_heartfailure_readmission, weight = noMDchg_doc$weights, weighty = noMDchg_nodoc$weights)$coefficients[[3]]),4)) %>% 
  rename("variable" = 1, "p-value" = 2)
p_num_ami <- data.frame(c("Num. Patients AMI"), round(c(wtd.t.test(x=noMDchg_doc$patnum_heartattack_readmission, y=noMDchg_nodoc$patnum_heartattack_readmission, weight = noMDchg_doc$weights, weighty = noMDchg_nodoc$weights)$coefficients[[3]]),4)) %>% 
  rename("variable" = 1, "p-value" = 2)
p_num_pneum <- data.frame(c("Num. Patients Pneumonia"), round(c(wtd.t.test(x=noMDchg_doc$patnum_pneum_readmission, y=noMDchg_nodoc$patnum_pneum_readmission, weight = noMDchg_doc$weights, weighty = noMDchg_nodoc$weights)$coefficients[[3]]),4)) %>% 
  rename("variable" = 1, "p-value" = 2)

pvalues <- rbind(p_beds, p_pen_AMI, p_pen_hf, p_pen_pneum, p_wa_read, p_hf_read, p_ami_read, p_pneum_read, p_wa_mort, p_hf_mort, p_ami_mort, p_pneum_mort, 
                 p_num_hf, p_num_ami, p_num_pneum)

noMDchg_doc_stats <- hospital_data %>% 
  filter(no_md_changes_2010_2014==1 & year==2010) %>%
  filter(has_any_md==1 & matched==1) %>%
  summarise_at(c("Number Beds"="beds",
                 "Penalized for AMI"="ever_pen_ha", "Penalized for HF"="ever_pen_hf",
                 "Penalized for Pneumonia"="ever_pen_pnem",
                 "Weighted Avg. Readmission Rate"="weightedavg_read",
                 "HF Readmission Rate" = "rate_heartfailure_readmission",
                 "AMI Readmission Rate" = "rate_heartattack_readmission",
                 "Pneum. Readmission Rate" = "rate_pneum_readmission",
                 "Weighted Avg. Mortality Rate"="weightedavg_mort",
                 "HF Mortality Rate" = "rate_heartfailure_mortality",
                 "AMI Mortality Rate" = "rate_heartattack_mortality",
                 "Pneum. Mortality Rate" = "rate_pneum_mortality",
                 "Num. Patients HF" = "patnum_heartfailure_readmission",
                 "Num. Patients AMI" = "patnum_heartattack_readmission",
                 "Num. Patients Pneumonia" = "patnum_pneum_readmission"), 
               list(m=mean,sd=sd), na.rm=TRUE) %>%
  mutate_if(is.numeric, ~ifelse(abs(.)==Inf,NA,.))  %>%
  gather(key=var,value=value) %>%
  extract(col="var",into=c("variable", "statistic"), regex=("(.*)_(.*)$")) %>%
  spread(key=statistic, value=value) %>%
  relocate(variable,m,sd) %>%
  rename(m_doc=m, sd_doc=sd)

noMDchg_nodoc_stats <- hospital_data %>% 
  filter(no_md_changes_2010_2014==1) %>%
  filter(has_any_md==0 & matched==1 & year==2010) %>%
  summarise_at(c("Number Beds"="beds",
                 "Penalized for AMI"="ever_pen_ha", "Penalized for HF"="ever_pen_hf",
                 "Penalized for Pneumonia"="ever_pen_pnem",
                 "Weighted Avg. Readmission Rate"="weightedavg_read",
                 "HF Readmission Rate" = "rate_heartfailure_readmission",
                 "AMI Readmission Rate" = "rate_heartattack_readmission",
                 "Pneum. Readmission Rate" = "rate_pneum_readmission",
                 "Weighted Avg. Mortality Rate"="weightedavg_mort",
                 "HF Mortality Rate" = "rate_heartfailure_mortality",
                 "AMI Mortality Rate" = "rate_heartattack_mortality",
                 "Pneum. Mortality Rate" = "rate_pneum_mortality",
                 "Num. Patients HF" = "patnum_heartfailure_readmission",
                 "Num. Patients AMI" = "patnum_heartattack_readmission",
                 "Num. Patients Pneumonia" = "patnum_pneum_readmission"), 
               list(m=mean,sd=sd), na.rm=TRUE) %>%
  mutate_if(is.numeric, ~ifelse(abs(.)==Inf,NA,.))  %>%
  gather(key=var,value=value) %>%
  extract(col="var",into=c("variable", "statistic"), regex=("(.*)_(.*)$")) %>%
  spread(key=statistic, value=value) %>%
  relocate(variable,m,sd) 

stats <- merge(noMDchg_doc_stats, noMDchg_nodoc_stats)
stats <- merge(stats, pvalues) %>%
  add_row(variable = "Num. Hospitals", sd_doc = n_doc, sd = n_nodoc)

knitr::kable(stats[c(8,5,6,7,9,10,11,15,2,4,13,14,1,3,12,16),],
             row.names = FALSE,
             format="latex",
             table.envir="table",
             col.names=c("Variable","Mean","Std. Dev.", "Mean","Std. Dev.", "p-value"),
             digits=2,
             caption="Summary Statistics",
             booktabs=TRUE,
             escape=F,
             align=c("l","c","c","c","c","c"),
             position="h") %>%
  kable_styling(full_width=F) %>%
  pack_rows(index = c("Hospital Characteristics" = 4, "Penalty Variables" = 3, "Readmission Outcome Variables" = 4, "Mortality Outcome Variables" = 4, " " = 1)) %>%
  add_header_above(c(" ", "with clinical experience" = 2, "without clinical experience" = 2, " "))

### TABLE 3 #########
noMDchg_doc <- hospital_data %>%
  filter(no_md_changes_2010_2014==1 & has_any_md==1 & ever_penalized==1)
noMDchg_nodoc <- hospital_data %>%
  filter(no_md_changes_2010_2014==1 & has_any_md==0 & ever_penalized==1)

n_doc <- noMDchg_doc %>%
  distinct(MCRNUM) %>%
  nrow()
n_nodoc <- noMDchg_nodoc %>%
  distinct(MCRNUM) %>%
  nrow()

p_beds <- data.frame(c("Number Beds"), c(round(t.test(noMDchg_doc$beds, noMDchg_nodoc$beds)$p.value,4))) %>%
  rename("variable" = 1, "p-value" = 2)
p_pen_AMI <- data.frame(c("Penalized for AMI"), c(round(t.test(noMDchg_doc$ever_pen_ha, noMDchg_nodoc$ever_pen_ha)$p.value,4))) %>%
  rename("variable" = 1, "p-value" = 2)
p_pen_hf <- data.frame(c("Penalized for HF"), c(round(t.test(noMDchg_doc$ever_pen_hf, noMDchg_nodoc$ever_pen_hf)$p.value,4)))%>%
  rename("variable" = 1, "p-value" = 2)
p_pen_pneum <- data.frame(c("Penalized for Pneumonia"), c(round(t.test(noMDchg_doc$ever_pen_pnem, noMDchg_nodoc$ever_pen_pnem)$p.value,4)))%>%
  rename("variable" = 1, "p-value" = 2)
p_wa_read <- data.frame(c("Weighted Avg. Readmission Rate"), c(round(t.test(noMDchg_doc$weightedavg_read, noMDchg_nodoc$weightedavg_read)$p.value,4)))%>%
  rename("variable" = 1, "p-value" = 2)
p_hf_read <- data.frame(c("HF Readmission Rate"), c(round(t.test(noMDchg_doc$rate_heartfailure_readmission, noMDchg_nodoc$rate_heartfailure_readmission)$p.value,4)))%>%
  rename("variable" = 1, "p-value" = 2)
p_ami_read <- data.frame(c("AMI Readmission Rate"), c(round(t.test(noMDchg_doc$rate_heartattack_readmission, noMDchg_nodoc$rate_heartattack_readmission)$p.value,4)))%>%
  rename("variable" = 1, "p-value" = 2)
p_pneum_read <- data.frame(c("Pneum. Readmission Rate"), c(round(t.test(noMDchg_doc$rate_pneum_readmission, noMDchg_nodoc$rate_pneum_readmission)$p.value,4)))%>%
  rename("variable" = 1, "p-value" = 2)
p_wa_mort <- data.frame(c("Weighted Avg. Mortality Rate"), c(round(t.test(noMDchg_doc$weightedavg_mort, noMDchg_nodoc$weightedavg_mort)$p.value,4)))%>%
  rename("variable" = 1, "p-value" = 2)
p_hf_mort <- data.frame(c("HF Mortality Rate"), c(round(t.test(noMDchg_doc$rate_heartfailure_mortality, noMDchg_nodoc$rate_heartfailure_mortality)$p.value,4)))%>%
  rename("variable" = 1, "p-value" = 2)
p_ami_mort <- data.frame(c("AMI Mortality Rate"), c(round(t.test(noMDchg_doc$rate_heartattack_mortality, noMDchg_nodoc$rate_heartattack_mortality)$p.value,4)))%>%
  rename("variable" = 1, "p-value" = 2)
p_pneum_mort <- data.frame(c("Pneum. Mortality Rate"), c(round(t.test(noMDchg_doc$rate_pneum_mortality, noMDchg_nodoc$rate_pneum_mortality)$p.value,4)))%>%
  rename("variable" = 1, "p-value" = 2)
p_num_hf <- data.frame(c("Num. Patients HF"), c(round(t.test(noMDchg_doc$patnum_heartfailure_readmission, noMDchg_nodoc$patnum_heartfailure_readmission)$p.value,4)))%>%
  rename("variable" = 1, "p-value" = 2)
p_num_ami <- data.frame(c("Num. Patients AMI"), c(round(t.test(noMDchg_doc$patnum_heartattack_readmission, noMDchg_nodoc$patnum_heartattack_readmission)$p.value,4)))%>%
  rename("variable" = 1, "p-value" = 2)
p_num_pneum <- data.frame(c("Num. Patients Pneumonia"), c(round(t.test(noMDchg_doc$patnum_pneum_readmission, noMDchg_nodoc$patnum_pneum_readmission)$p.value,4)))%>%
  rename("variable" = 1, "p-value" = 2)

pvalues <- rbind(p_beds, p_pen_AMI, p_pen_hf, p_pen_pneum, p_wa_read, p_hf_read, p_ami_read, p_pneum_read, p_wa_mort, p_hf_mort, p_ami_mort, p_pneum_mort, 
                 p_num_hf, p_num_ami, p_num_pneum)

noMDchg_doc_stats <- hospital_data %>% 
  filter(no_md_changes_2010_2014==1) %>%
  filter(has_any_md==1 & ever_penalized==1 & year==2010) %>%
  summarise_at(c("Number Beds"="beds",
                 "Penalized for AMI"="ever_pen_ha", "Penalized for HF"="ever_pen_hf",
                 "Penalized for Pneumonia"="ever_pen_pnem",
                 "Weighted Avg. Readmission Rate"="weightedavg_read",
                 "HF Readmission Rate" = "rate_heartfailure_readmission",
                 "AMI Readmission Rate" = "rate_heartattack_readmission",
                 "Pneum. Readmission Rate" = "rate_pneum_readmission",
                 "Weighted Avg. Mortality Rate"="weightedavg_mort",
                 "HF Mortality Rate" = "rate_heartfailure_mortality",
                 "AMI Mortality Rate" = "rate_heartattack_mortality",
                 "Pneum. Mortality Rate" = "rate_pneum_mortality",
                 "Num. Patients HF" = "patnum_heartfailure_readmission",
                 "Num. Patients AMI" = "patnum_heartattack_readmission",
                 "Num. Patients Pneumonia" = "patnum_pneum_readmission"), 
               list(m=mean,sd=sd), na.rm=TRUE) %>%
  mutate_if(is.numeric, ~ifelse(abs(.)==Inf,NA,.))  %>%
  gather(key=var,value=value) %>%
  extract(col="var",into=c("variable", "statistic"), regex=("(.*)_(.*)$")) %>%
  spread(key=statistic, value=value) %>%
  relocate(variable,m,sd) %>%
  rename(m_doc=m, sd_doc=sd)

noMDchg_nodoc_stats <- hospital_data %>% 
  filter(no_md_changes_2010_2014==1) %>%
  filter(has_any_md==0 & ever_penalized==1 & year==2010) %>%
  summarise_at(c("Number Beds"="beds",
                 "Penalized for AMI"="ever_pen_ha", "Penalized for HF"="ever_pen_hf",
                 "Penalized for Pneumonia"="ever_pen_pnem",
                 "Weighted Avg. Readmission Rate"="weightedavg_read",
                 "HF Readmission Rate" = "rate_heartfailure_readmission",
                 "AMI Readmission Rate" = "rate_heartattack_readmission",
                 "Pneum. Readmission Rate" = "rate_pneum_readmission",
                 "Weighted Avg. Mortality Rate"="weightedavg_mort",
                 "HF Mortality Rate" = "rate_heartfailure_mortality",
                 "AMI Mortality Rate" = "rate_heartattack_mortality",
                 "Pneum. Mortality Rate" = "rate_pneum_mortality",
                 "Num. Patients HF" = "patnum_heartfailure_readmission",
                 "Num. Patients AMI" = "patnum_heartattack_readmission",
                 "Num. Patients Pneumonia" = "patnum_pneum_readmission"), 
               list(m=mean,sd=sd), na.rm=TRUE) %>%
  mutate_if(is.numeric, ~ifelse(abs(.)==Inf,NA,.))  %>%
  gather(key=var,value=value) %>%
  extract(col="var",into=c("variable", "statistic"), regex=("(.*)_(.*)$")) %>%
  spread(key=statistic, value=value) %>%
  relocate(variable,m,sd) 

stats <- merge(noMDchg_doc_stats, noMDchg_nodoc_stats)
stats <- merge(stats, pvalues) %>%
  add_row(variable = "Num. Hospitals", sd_doc = n_doc, sd = n_nodoc)

knitr::kable(stats[c(8,5,6,7,9,10,11,15,2,4,13,14,1,3,12,16),],
             row.names = FALSE,
             format="latex",
             table.envir="table",
             col.names=c("Variable","Mean","Std. Dev.", "Mean","Std. Dev.", "p-value"),
             digits=2,
             caption="Summary Statistics",
             booktabs=TRUE,
             escape=F,
             align=c("l","c","c","c","c","c"),
             position="h") %>%
  kable_styling(full_width=F) %>%
  pack_rows(index = c("Hospital Characteristics" = 4, "Penalty Variables" = 3, "Readmission Outcome Variables" = 4, "Mortality Outcome Variables" = 4, " " = 1)) %>%
  add_header_above(c(" ", "with clinical experience" = 2, "without clinical experience" = 2, " "))



### Graph leadership changes over time
observe <- hospital_data %>%
  filter(year==2010 & is.na(any_change))


change_avgs <- hospital_data %>%
  filter(!(year==2010 & is.na(any_change))) %>%
  group_by(year) %>%
  summarise_at(c("any_change", "md_change", "num_doc_change", "ceo_change", "num_exec_change"), list(mean), na.rm=T) %>%
  ungroup() %>%
  filter(year!=2008 & year!=2009)

change_avgs <- pivot_longer(change_avgs, c("any_change", "md_change", "num_doc_change", "ceo_change", "num_exec_change"), names_to = "change") %>%
  mutate(change=ifelse(change=="any_change", "any", change),
         change=ifelse(change=="ceo_change", "CEO", change),
         change=ifelse(change=="md_change", "hires any MD", change),
         change=ifelse(change=="num_doc_change", "number of MDs", change),
         change=ifelse(change=="num_exec_change", "number executives", change))

ggplot(change_avgs, aes(x=year, y=value, group=change, color=change)) + geom_line(linewidth=.75) + theme_minimal() + theme_bw() + ylab("Proportion of Hospitals\n") +
  xlab("\nyear") + scale_color_brewer(palette="Set2") + theme(text = element_text(size=18))

## GRAPH OF VARIATION BY STATE ######
hospital_data <- hospital_data %>%
  group_by(ID) %>%
  mutate(ever_has_md = ifelse(sum(has_any_md, na.rm=T)>0,1,0)) %>%
  ungroup() %>%
  mutate(ever_has_md = ifelse(profit_status=="forprofit",NA,ever_has_md))

state_data <- hospital_data %>%
  filter(!is.na(no_md_change_2010_2014)) %>%
  distinct(ID, MSTATE, ever_has_md) %>%
  group_by(MSTATE) %>%
  mutate(avg = mean(ever_has_md)) %>%
  ungroup() %>%
  distinct(MSTATE, avg)

state <- map_data("state")

state <- state %>%
  mutate(fips = fips(region, to="FIPS"))
state_data <- state_data %>%
  mutate(fips=fips(MSTATE, to="FIPS"))

state_data <- state_data %>%
  left_join(state, by="fips")

ggplot(data=state_data, aes(x=long, y=lat, fill=avg, group=group)) + 
  geom_polygon(color = "white") + 
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + 
  coord_fixed(1.3) +
  theme(text=element_text(size=15)) + theme_bw() + scale_fill_gradient2(midpoint = .5, low="#66C2A5", high="#8DA0CB",
                                                                         mid="white") +
  labs(fill='Perc. Hosp.\nw/ MD\n') 


ggsave(filename = "Objects/has_doc_avg_map.pdf", width=8, height=5, units="in")


### table of correlations between MD and other characteristics #######
hospital_data <- hospital_data %>%
  group_by(ID) %>%
  mutate(ever_has_md = ifelse(sum(has_any_md, na.rm=T)>0,1,0)) %>%
  ungroup() %>%
  mutate(ever_has_md = ifelse(profit_status=="forprofit",NA,ever_has_md))

hospital_data <- hospital_data %>%
  mutate(academic = ifelse(MAPP5==1,1,0),
         part_of_system = ifelse(is.na(SYSID),0,1),
         EHR = ifelse(EHLTH==2,1,0)) %>%
  group_by(ID) %>%
  mutate(minyr = min(year),
         maxyr = max(year)) %>% 
  arrange(year) %>%
  mutate(lag_sys = dplyr::lag(SYSID)) %>%
  mutate(change_sys = ifelse(is.na(SYSID) & !is.na(lag_sys),1,NA)) %>%
  mutate(change_sys = ifelse(!is.na(SYSID) & is.na(lag_sys),1,change_sys)) %>%
  mutate(change_sys = ifelse(!is.na(SYSID) & !is.na(lag_sys) & lag_sys!=SYSID, 1, change_sys)) %>%
  mutate(change_sys = ifelse(year==minyr, NA, change_sys)) %>%
  fill(change_sys, .direction="downup") %>%
  mutate(change_sys = ifelse(is.na(change_sys),0,change_sys)) %>%
  ungroup()

corr <- as.data.frame(cor(hospital_data[, c("ever_has_md", "academic", "beds", "PHYGP", "SUBS", "part_of_system",
                      "ever_cmo")], use="complete.obs")) %>%
  select(ever_has_md) %>%
  rownames_to_column() %>%
  filter(rowname!="ever_has_md") %>%
  mutate(rowname = ifelse(rowname=="FTMT", "No. Physicians", rowname),
         rowname = ifelse(rowname=="FTRNTF", "No. Nurses", rowname),
         rowname = ifelse(rowname=="PHYGP", "Owned by Phys. Group", rowname),
         rowname = ifelse(rowname=="academic", "Academic", rowname),
         rowname = ifelse(rowname=="beds", "No. Beds", rowname),
         rowname = ifelse(rowname=="operating_expenses", "Operating Expenses", rowname),
         rowname = ifelse(rowname=="SUBS", "Owns Subsidiary", rowname),
         rowname = ifelse(rowname=="part_of_system", "Part of System", rowname),
         rowname = ifelse(rowname=="change_sys", "Acquired", rowname),
         rowname = ifelse(rowname=="MNGT", "Contract Managed", rowname),
         rowname = ifelse(rowname=="ever_cmo", "Has CMO", rowname))

knitr::kable(corr,
             row.names = FALSE,
             format="latex",
             table.envir="table",
             col.names=c("Variable","Correlation w/ MD Executive"),
             digits=3,
             caption="Correlation between having MD executive and other hospital characteristics",
             booktabs=TRUE,
             escape=F,
             align=c("l","c"),
             position="h") %>%
  kable_styling(full_width=F) 










