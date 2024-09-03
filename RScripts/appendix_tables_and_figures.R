library(dplyr)
library(readr)
library(knitr)
library(kableExtra)
library(ggplot2)
library(ggpubr)
library(RColorBrewer)
library(synthdid)
library(abind)
library(reshape2)
library(data.table)
library(tidyr)
library(maps)
library(cdlTools)
library(MatchIt)
library(fixest)

options(knitr.kable.NA=" ")

# read in data
hospital_data <- read_rds(paste0(created_data_path, "all_hospital_data3.rds"))
doc_exec_data <- read_rds(paste0(created_data_path, "doc_exec_data.rds"))

hospital_data <- hospital_data %>%
  mutate(ever_pen_ha_only=ifelse(ever_pen_ha>=1 & ever_pen_hf==0 & ever_pen_pnem==0,1,0), 
         ever_pen_hf_only=ifelse(ever_pen_hf>=1 & ever_pen_ha==0 & ever_pen_pnem==0,1,0), 
         ever_pen_pnem_only=ifelse(ever_pen_pnem>=1 & ever_pen_hf==0 & ever_pen_ha==0,1,0), 
         ever_pen_ami_hf = ifelse(ever_pen_hf>=1 & ever_pen_ha>=1 & ever_pen_pnem==0,1,0),
         ever_pen_ami_pneum = ifelse(ever_pen_hf==0 & ever_pen_pnem>=1 & ever_pen_ha==1,1,0),
         ever_pen_hf_pneum = ifelse(ever_pen_hf>=1 & ever_pen_pnem>=1 & ever_pen_ha>=0,1,0),
         ever_pen_all = ifelse(ever_pen_hf>=1 & ever_pen_ha>=1 & ever_pen_pnem>=1,1,0)) %>%
  mutate(frac_doc_exec = num_doctors/num_execs) %>%
  mutate(rate_exec=num_doctors/num_execs) %>%
  mutate(rate_int_exec=num_int_med_doctors/num_execs) %>%
  mutate(ever_has_md = ifelse(is.na(ever_has_md),0,ever_has_md)) 

# TABLE 4: compare in-sample and out-of-sample nonprofit hospitals ####
in_sample_NFP_data <- hospital_data %>%
  filter(year!=2009 & year<=2014) %>%
  filter(!is.na(no_num_md_change_2010_2014)) %>%
  summarise_at(c("Number Beds"="beds",
                 "Penalized for AMI"="ever_pen_ha_only", "Penalized for HF"="ever_pen_hf_only",
                 "Penalized for Pneumonia"="ever_pen_pnem_only",
                 "Penalized for AMI + HF"="ever_pen_ami_hf",
                 "Penalized for AMI + Pneumonia"="ever_pen_ami_pneum",
                 "Penalized for HF + Pneumonia"="ever_pen_hf_pneum",
                 "Penalized for All Conditions"="ever_pen_all",
                 "Ever Received HVBP Incentive" = "ever_hvbp",
                 "Academic Med. Center"="academic", 
                 "Physician Owned"="PHYGP", 
                 "Owns Subsidiary"="ever_subs", 
                 "System Affiliated"="ever_part_of_system",
                 "Has a CMO"="ever_cmo",
                 "Number Executives"="num_execs",
                 "Fraction Clinical Execs"="rate_exec",
                 "Fraction Int. Medicine Execs"="rate_int_exec",
                 "Has Clinical CEO"="ever_ceo_md"
  ), 
               list(m=mean,sd=sd), na.rm=TRUE) %>%
  mutate_if(is.numeric, ~ifelse(abs(.)==Inf,NA,.))  %>%
  gather(key=var,value=value) %>%
  extract(col="var",into=c("variable", "statistic"), regex=("(.*)_(.*)$")) %>%
  spread(key=statistic, value=value) %>%
  relocate(variable,m,sd) 
out_sample_NFP_data <- hospital_data %>%
  filter(year!=2009 & year<=2014) %>%
  filter(is.na(no_num_md_change_2010_2014) & profit_status=="nonprofit") %>%
  summarise_at(c("Number Beds"="beds",
                 "Penalized for AMI"="ever_pen_ha_only", "Penalized for HF"="ever_pen_hf_only",
                 "Penalized for Pneumonia"="ever_pen_pnem_only",
                 "Penalized for AMI + HF"="ever_pen_ami_hf",
                 "Penalized for AMI + Pneumonia"="ever_pen_ami_pneum",
                 "Penalized for HF + Pneumonia"="ever_pen_hf_pneum",
                 "Penalized for All Conditions"="ever_pen_all",
                 "Ever Received HVBP Incentive" = "ever_hvbp",
                 "Academic Med. Center"="academic", 
                 "Physician Owned"="PHYGP", 
                 "Owns Subsidiary"="ever_subs", 
                 "System Affiliated"="ever_part_of_system",
                 "Has a CMO"="ever_cmo",
                 "Number Executives"="num_execs",
                 "Fraction Clinical Execs"="rate_exec",
                 "Fraction Int. Medicine Execs"="rate_int_exec",
                 "Has Clinical CEO"="ever_ceo_md"), 
               list(m=mean,sd=sd), na.rm=TRUE) %>%
  mutate_if(is.numeric, ~ifelse(abs(.)==Inf,NA,.))  %>%
  gather(key=var,value=value) %>%
  extract(col="var",into=c("variable", "statistic"), regex=("(.*)_(.*)$")) %>%
  spread(key=statistic, value=value) %>%
  relocate(variable,m,sd) %>%
  rename(m_out=m, sd_out=sd)

n_in <- hospital_data %>%
  filter(year!=2009 & year<=2014) %>%
  filter(!is.na(no_num_md_change_2010_2014)) %>%
  distinct(ID) %>%
  nrow()
n_out <- hospital_data %>%
  filter(year!=2009 & year<=2014) %>%
  filter(is.na(no_num_md_change_2010_2014) & profit_status=="nonprofit") %>%
  distinct(ID) %>%
  nrow()

NFP_sample_data <- merge(in_sample_NFP_data, out_sample_NFP_data, by="variable") %>%
  add_row(variable="Num. Hospitals", m=n_in, m_out=n_out)

NFP_in_out_sample_tab <- knitr::kable(NFP_sample_data[c(1,7,17,18,8,3,4,5,6,2,11,14,16,12,13,15,10,19),],
                                      row.names = FALSE,
                                      format="latex",
                                      table.envir="table",
                                      col.names=c("Variable","Mean","Std. Dev.", "Mean","Std. Dev."),
                                      digits=2,
                                      caption="\\label{nfp_sample_compare}NFP Sample Comparison",
                                      booktabs=TRUE,
                                      escape=F,
                                      align=c("l","c","c","c","c","c"),
                                      position="ht!") %>%
  kable_styling(full_width=F) %>%
  add_header_above(c(" "=1, "In-sample NFP"=2, "Out-of-sample NFP"=2)) %>%
  pack_rows(index = c("Hospital Characteristics" = 4, "Executive Team Characteristics" = 5, "Penalty/Payment Variables" = 8,
                      " " = 1))
write(NFP_in_out_sample_tab, file="Tables/NFP_sample_comparison.tex")

# FIGURE 11: outcomes for in-sample and out-of-sample ####
in_sample_outcomes <- hospital_data %>%
  filter(!is.na(num_execs)) %>%
  group_by(year) %>%
  summarise_at(c("Weighted Avg. Readmission Rate"="weightedavg_read",
                 "Weighted Avg. Mortality Rate"="weightedavg_mort",
                 "Case Mix Index"="cmiv"
  ), 
  list(m=mean), na.rm=TRUE) %>%
  mutate_if(is.numeric, ~ifelse(abs(.)==Inf,NA,.))  %>%
  mutate(sample = "In-Sample") %>%
  filter(year<2016)

out_sample_outcomes <- hospital_data %>%
  filter(profit_status=="nonprofit" & is.na(num_execs)) %>%
  group_by(year) %>%
  summarise_at(c("Weighted Avg. Readmission Rate"="weightedavg_read",
                 "Weighted Avg. Mortality Rate"="weightedavg_mort",
                 "Case Mix Index"="cmiv"
  ), 
  list(m=mean), na.rm=TRUE) %>%
  mutate_if(is.numeric, ~ifelse(abs(.)==Inf,NA,.))  %>%
  mutate(sample = "Out-of-Sample") %>%
  filter(year<2016)

outcomes <- rbind(in_sample_outcomes, out_sample_outcomes)

read <- ggplot(outcomes, aes(x=year, y=`Weighted Avg. Readmission Rate_m`, color=sample, linetype=sample, shape=sample)) + geom_point(size=3) + geom_line() +
  geom_vline(xintercept = 2012, linetype = "dotted") + xlim(2010,2014) +
  geom_line(linewidth=1.2) + theme_bw() + xlab("") + ylab("Readmission\n") + theme(legend.title = element_blank()) +
  theme(text=element_text(size=18)) + scale_color_manual(values = c("In-Sample" = "#D65828", "Out-of-Sample" = "#AED1EC", "Ever Clinical Exec" ="#6BAED6", "Always Clinical Exec" = "#2C6B8E")) 
mort <- ggplot(outcomes, aes(x=year, y=`Weighted Avg. Mortality Rate_m`, color=sample, linetype=sample, shape=sample)) + geom_point(size=3) + geom_line() +
  geom_vline(xintercept = 2012, linetype = "dotted") + xlim(2010,2014) +
  geom_line(linewidth=1.2) + theme_bw() + xlab("") + ylab("Mortality\n") + theme(legend.title = element_blank()) +
  theme(text=element_text(size=18)) + scale_color_manual(values = c("In-Sample" = "#D65828", "Out-of-Sample" = "#AED1EC", "Ever Clinical Exec" ="#6BAED6", "Always Clinical Exec" = "#2C6B8E"))   + ylim(11.75,12.75)
cmi <- ggplot(outcomes, aes(x=year, y=`Case Mix Index_m`, color=sample, linetype=sample, shape=sample)) + geom_point(size=3) + geom_line() +
  geom_vline(xintercept = 2012, linetype = "dotted") + xlim(2010,2014) +
  geom_line(linewidth=1.2) + theme_bw() + xlab("") + ylab("Case Mix Index\n") + theme(legend.title = element_blank()) +
  theme(text=element_text(size=18)) + scale_color_manual(values = c("In-Sample" = "#D65828", "Out-of-Sample" = "#AED1EC", "Ever Clinical Exec" ="#6BAED6", "Always Clinical Exec" = "#2C6B8E"))   + ylim(1,2)

ggarrange(read, mort, cmi,
          nrow = 3,
          ncol = 1,
          common.legend = T,
          legend = "right")

ggsave(paste0(objects_path, "nonprofit_sampleoutcomes_graph.pdf"), height=9, width=7, units="in")


# TABLE 5: Timing of clinical executive changes ####
change_data_2012 <- hospital_data %>%
  filter(!is.na(no_num_md_change_2010_2014)) %>%
  mutate(MD_pre_2012 = ifelse(year %in% c(2010, 2011) & num_doctors>0,1,NA),
         MD_post_2012 = ifelse(year %in% c(2013, 2014) & num_doctors>0,1,NA),
         has_md_2012 = ifelse(year==2012 & num_doctors>0,1,NA)) %>%
  group_by(ID) %>%
  fill(MD_pre_2012, MD_post_2012, has_md_2012, .direction="downup") %>%
  ungroup() %>%
  mutate(MD_pre_2012=ifelse(is.na(MD_pre_2012),0,MD_pre_2012)) %>%
  mutate(MD_post_2012 = ifelse(is.na(MD_post_2012),0,MD_post_2012)) %>%
  mutate(has_md_2012 = ifelse(is.na(has_md_2012),0,has_md_2012))
nopre_2012_post <- change_data_2012 %>%
  filter(MD_pre_2012==0 & has_md_2012==1 & MD_post_2012==1) %>%
  distinct(ID) %>%
  nrow()
pre_2012_nopost <- change_data_2012 %>%
  filter(MD_pre_2012==1 & has_md_2012==1 & MD_post_2012==0) %>%
  distinct(ID) %>%
  nrow()
nopre_2012_nopost <- change_data_2012 %>%
  filter(MD_pre_2012==0 & has_md_2012==1 & MD_post_2012==0) %>%
  distinct(ID) %>%
  nrow()
pre_2012_post <- change_data_2012 %>%
  filter(MD_pre_2012==1 & has_md_2012==1 & MD_post_2012==1) %>%
  distinct(ID) %>%
  nrow()
nopre_no2012_post <- change_data_2012 %>%
  filter(MD_pre_2012==0 & has_md_2012==0 & MD_post_2012==1) %>%
  distinct(ID) %>%
  nrow()
pre_no2012_nopost <- change_data_2012 %>%
  filter(MD_pre_2012==1 & has_md_2012==0 & MD_post_2012==0) %>%
  distinct(ID) %>%
  nrow()
nopre_no2012_nopost <- change_data_2012 %>%
  filter(MD_pre_2012==0 & has_md_2012==0 & MD_post_2012==0) %>%
  distinct(ID) %>%
  nrow()
pre_no2012_post <- change_data_2012 %>%
  filter(MD_pre_2012==1 & has_md_2012==0 & MD_post_2012==1) %>%
  distinct(ID) %>%
  nrow()

Pre <- c(0,1,0,1,0,1,0,1)
In_2012 <- c(1,1,1,1,0,0,0,0)
Post <- c(1,0,0,1,1,0,0,1)
Num <- c(nopre_2012_post, pre_2012_nopost, nopre_2012_nopost, pre_2012_post,
         nopre_no2012_post, pre_no2012_nopost, nopre_no2012_nopost, pre_no2012_post)
change_num_table <- data.frame(Pre, In_2012, Post, Num)

change_num_table_2012 <- knitr::kable(change_num_table,
                                      row.names = FALSE,
                                      format="latex",
                                      table.envir="table",
                                      col.names=c("Has MD Pre-2012","Has MD 2012","Has MD Post 2012", "Num. Hospitals"),
                                      digits=0,
                                      caption="\\label{change_timing}Timing of MD Exec Changes",
                                      booktabs=TRUE,
                                      escape=F,
                                      align=c("l","c","c","c","c","c"),
                                      position="ht!") %>%
  kable_styling(full_width=F) 
write(change_num_table_2012, file="Tables/change_num_table_2012.tex")

# FIGURE 12: Percent of hospitals w/ clinical execs by state ####
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
  theme(text=element_text(size=15)) + theme_bw() + scale_fill_gradient2(low="#C2DEF2", high="#2C6B8E") +
  labs(fill=' ') 
ggsave(filename = "Objects/has_doc_avg_map.pdf", width=8, height=5, units="in")

# TABLE 6: summary statistics including for-profit ####
n_ever_md <- hospital_data %>%
  filter(ever_has_md==1) %>%
  distinct(ID) %>%
  nrow()
ever_md_stats <- hospital_data %>%
  filter(ever_has_md==1) %>%
  mutate(rate_exec=num_doctors/num_execs) %>%
  mutate(rate_int_exec=num_int_med_doctors/num_execs) %>%
  mutate(ever_has_md = ifelse(is.na(ever_has_md),0,ever_has_md)) %>%
  summarise_at(c("Number Beds"="beds",
                 "Penalized for AMI"="ever_pen_ha_only", "Penalized for HF"="ever_pen_hf_only",
                 "Penalized for Pneumonia"="ever_pen_pnem_only",
                 "Penalized for AMI + HF"="ever_pen_ami_hf",
                 "Penalized for AMI + Pneumonia"="ever_pen_ami_pneum",
                 "Penalized for HF + Pneumonia"="ever_pen_hf_pneum",
                 "Penalized for All Conditions"="ever_pen_all",
                 "Ever Received HVBP Incentive" = "ever_hvbp",
                 "Academic Med. Center"="academic", 
                 "Physician Owned"="PHYGP", 
                 "Owns Subsidiary"="ever_subs", 
                 "System Affiliated"="ever_part_of_system",
                 "Has a CMO"="ever_cmo",
                 "Number Executives"="num_execs",
                 "Fraction Clinical Execs"="rate_exec",
                 "Fraction Int. Medicine Execs"="rate_int_exec",
                 "Has Clinical CEO"="ever_ceo_md"
  ), 
  list(m=mean), na.rm=TRUE) %>%
  mutate_if(is.numeric, ~ifelse(abs(.)==Inf,NA,.))  %>%
  gather(key=var,value=value) %>%
  extract(col="var",into=c("variable", "statistic"), regex=("(.*)_(.*)$")) %>%
  spread(key=statistic, value=value) %>%
  relocate(variable,m) %>%
  add_row(variable="Num. Hospitals", m=n_ever_md) %>%
  rename(ever=m)

n_always_md <- hospital_data %>%
  filter(ever_has_md==1 & no_num_md_change_2010_2014==1) %>%
  distinct(ID) %>%
  nrow()
always_md_stats <- hospital_data %>%
  filter(ever_has_md==1 & no_num_md_change_2010_2014==1) %>%
  mutate(rate_exec=num_doctors/num_execs) %>%
  mutate(rate_int_exec=num_int_med_doctors/num_execs) %>%
  mutate(ever_has_md = ifelse(is.na(ever_has_md),0,ever_has_md)) %>%
  summarise_at(c("Number Beds"="beds",
                 "Penalized for AMI"="ever_pen_ha_only", "Penalized for HF"="ever_pen_hf_only",
                 "Penalized for Pneumonia"="ever_pen_pnem_only",
                 "Penalized for AMI + HF"="ever_pen_ami_hf",
                 "Penalized for AMI + Pneumonia"="ever_pen_ami_pneum",
                 "Penalized for HF + Pneumonia"="ever_pen_hf_pneum",
                 "Penalized for All Conditions"="ever_pen_all",
                 "Ever Received HVBP Incentive" = "ever_hvbp",
                 "Academic Med. Center"="academic", 
                 "Physician Owned"="PHYGP", 
                 "Owns Subsidiary"="ever_subs", 
                 "System Affiliated"="ever_part_of_system",
                 "Has a CMO"="ever_cmo",
                 "Number Executives"="num_execs",
                 "Fraction Clinical Execs"="rate_exec",
                 "Fraction Int. Medicine Execs"="rate_int_exec",
                 "Has Clinical CEO"="ever_ceo_md"
  ), 
  list(m=mean), na.rm=TRUE) %>%
  mutate_if(is.numeric, ~ifelse(abs(.)==Inf,NA,.))  %>%
  gather(key=var,value=value) %>%
  extract(col="var",into=c("variable", "statistic"), regex=("(.*)_(.*)$")) %>%
  spread(key=statistic, value=value) %>%
  relocate(variable,m) %>%
  add_row(variable="Num. Hospitals", m=n_always_md) %>%
  rename(always=m)

n_never_md <- hospital_data %>%
  filter(ever_has_md==0 & no_num_md_change_2010_2014==1) %>%
  distinct(ID) %>%
  nrow()
never_md_stats <- hospital_data %>%
  filter(ever_has_md==0 & no_num_md_change_2010_2014==1) %>%
  mutate(rate_exec=num_doctors/num_execs) %>%
  mutate(rate_int_exec=num_int_med_doctors/num_execs) %>%
  mutate(ever_has_md = ifelse(is.na(ever_has_md),0,ever_has_md)) %>%
  summarise_at(c("Number Beds"="beds",
                 "Penalized for AMI"="ever_pen_ha_only", "Penalized for HF"="ever_pen_hf_only",
                 "Penalized for Pneumonia"="ever_pen_pnem_only",
                 "Penalized for AMI + HF"="ever_pen_ami_hf",
                 "Penalized for AMI + Pneumonia"="ever_pen_ami_pneum",
                 "Penalized for HF + Pneumonia"="ever_pen_hf_pneum",
                 "Penalized for All Conditions"="ever_pen_all",
                 "Ever Received HVBP Incentive" = "ever_hvbp",
                 "Academic Med. Center"="academic", 
                 "Physician Owned"="PHYGP", 
                 "Owns Subsidiary"="ever_subs", 
                 "System Affiliated"="ever_part_of_system",
                 "Has a CMO"="ever_cmo",
                 "Number Executives"="num_execs",
                 "Fraction Clinical Execs"="rate_exec",
                 "Fraction Int. Medicine Execs"="rate_int_exec",
                 "Has Clinical CEO"="ever_ceo_md"
  ), 
  list(m=mean), na.rm=TRUE) %>%
  mutate_if(is.numeric, ~ifelse(abs(.)==Inf,NA,.))  %>%
  gather(key=var,value=value) %>%
  extract(col="var",into=c("variable", "statistic"), regex=("(.*)_(.*)$")) %>%
  spread(key=statistic, value=value) %>%
  relocate(variable,m) %>%
  add_row(variable="Num. Hospitals", m=n_never_md) %>%
  rename(never=m)

n_fp <- hospital_data %>%
  filter(profit_status=="forprofit") %>%
  distinct(ID) %>%
  nrow()
fp_stats <- hospital_data %>%
  filter(profit_status=="forprofit") %>%
  summarise_at(c("Number Beds"="beds",
                 "Penalized for AMI"="ever_pen_ha_only", "Penalized for HF"="ever_pen_hf_only",
                 "Penalized for Pneumonia"="ever_pen_pnem_only",
                 "Penalized for AMI + HF"="ever_pen_ami_hf",
                 "Penalized for AMI + Pneumonia"="ever_pen_ami_pneum",
                 "Penalized for HF + Pneumonia"="ever_pen_hf_pneum",
                 "Penalized for All Conditions"="ever_pen_all",
                 "Ever Received HVBP Incentive" = "ever_hvbp",
                 "Academic Med. Center"="academic", 
                 "Physician Owned"="PHYGP", 
                 "Owns Subsidiary"="ever_subs", 
                 "System Affiliated"="ever_part_of_system"
  ), 
  list(m=mean), na.rm=TRUE) %>%
  mutate_if(is.numeric, ~ifelse(abs(.)==Inf,NA,.))  %>%
  gather(key=var,value=value) %>%
  extract(col="var",into=c("variable", "statistic"), regex=("(.*)_(.*)$")) %>%
  spread(key=statistic, value=value) %>%
  relocate(variable,m) %>%
  add_row(variable="Num. Hospitals", m=n_never_md) %>%
  rename(never=m)

fp_tab_stats <- ever_md_stats %>%
  left_join(always_md_stats, by="variable") %>%
  left_join(never_md_stats, by="variable") %>%
  left_join(fp_stats, by="variable")

fp_stats_tab <- knitr::kable(fp_tab_stats[c(1,7,17,18,8,3,4,5,6,2,11,14,16,12,13,15,10,19),],
                             row.names = FALSE,
                             format="latex",
                             table.envir="table",
                             col.names=c("Variable","Ever Clinical Exec", "Always Clinical Exec", "Never Clinical Exec", "For-Profit"),
                             digits=2,
                             caption="\\label{tab:fp_samples_stable}Summary Statistics by Hospital Type",
                             booktabs=TRUE,
                             escape=F,
                             align=c("l","c","c","c","c","c"),
                             position="ht!") %>%
  kable_styling(full_width=F) %>%
  pack_rows(index = c("Hospital Characteristics" = 4, "Executive Team Characteristics" = 5, "Penalty/Payment Variables" = 8,
                      " " = 1))
write(fp_stats_tab, file="Tables/forprofit_sample_sumstats.tex")

# FIGURE 13: graph outcomes for for-profits ####
ever_md_outcome_stats <- hospital_data %>%
  filter(ever_has_md==1) %>%
  group_by(year) %>%
  summarise_at(c("Weighted Avg. Readmission Rate"="weightedavg_read",
                 "Weighted Avg. Mortality Rate"="weightedavg_mort",
                 "Case Mix Index"="cmiv"
  ), 
  list(m=mean), na.rm=TRUE) %>%
  mutate_if(is.numeric, ~ifelse(abs(.)==Inf,NA,.))  %>%
  mutate(sample = "Ever Clinical Exec") %>%
  filter(year<2016)

always_md_outcome_stats <- hospital_data %>%
  filter(ever_has_md==1 & no_num_md_change_2010_2014==1) %>%
  group_by(year) %>%
  summarise_at(c("Weighted Avg. Readmission Rate"="weightedavg_read",
                 "Weighted Avg. Mortality Rate"="weightedavg_mort",
                 "Case Mix Index"="cmiv"
  ), 
  list(m=mean), na.rm=TRUE) %>%
  mutate_if(is.numeric, ~ifelse(abs(.)==Inf,NA,.))  %>%
  mutate(sample = "Always Clinical Exec") %>%
  filter(year<2016)

never_md_outcome_stats <- hospital_data %>%
  filter(ever_has_md==0 & no_num_md_change_2010_2014==1) %>%
  group_by(year) %>%
  summarise_at(c("Weighted Avg. Readmission Rate"="weightedavg_read",
                 "Weighted Avg. Mortality Rate"="weightedavg_mort",
                 "Case Mix Index"="cmiv"
  ), 
  list(m=mean), na.rm=TRUE) %>%
  mutate_if(is.numeric, ~ifelse(abs(.)==Inf,NA,.))  %>%
  mutate(sample = "Never Clinical Exec") %>%
  filter(year<2016)

forprofit_outcome_stats <- hospital_data %>%
  filter(profit_status=="forprofit") %>%
  group_by(year) %>%
  summarise_at(c("Weighted Avg. Readmission Rate"="weightedavg_read",
                 "Weighted Avg. Mortality Rate"="weightedavg_mort",
                 "Case Mix Index"="cmiv"
  ), 
  list(m=mean), na.rm=TRUE) %>%
  mutate_if(is.numeric, ~ifelse(abs(.)==Inf,NA,.))  %>%
  mutate(sample = "For-Profit") %>%
  filter(year<2016)

outcomes <- rbind(ever_md_outcome_stats, always_md_outcome_stats, never_md_outcome_stats, forprofit_outcome_stats)

read <- ggplot(outcomes, aes(x=year, y=`Weighted Avg. Readmission Rate_m`, color=sample, linetype=sample, shape=sample)) + geom_point(size=3) + geom_line() +
  geom_vline(xintercept = 2012, linetype = "dotted") + xlim(2010,2014) +
  geom_line(linewidth=1.2) + theme_bw() + xlab("") + ylab("Readmission\n") + theme(legend.title = element_blank()) +
  theme(text=element_text(size=18)) + scale_color_manual(values = c("For-Profit" = "#D65828", "Never Clinical Exec" = "#AED1EC", "Ever Clinical Exec" ="#6BAED6", "Always Clinical Exec" = "#2C6B8E")) 

mort <- ggplot(outcomes, aes(x=year, y=`Weighted Avg. Mortality Rate_m`, color=sample, linetype=sample, shape=sample)) + geom_point(size=3) + geom_line() +
  geom_vline(xintercept = 2012, linetype = "dotted") + xlim(2010,2014) +
  geom_line(linewidth=1.2) + theme_bw() + xlab("") + ylab("Mortality\n") + theme(legend.title = element_blank()) +
  theme(text=element_text(size=18)) + scale_color_manual(values = c("For-Profit" = "#D65828", "Never Clinical Exec" = "#AED1EC", "Ever Clinical Exec" ="#6BAED6", "Always Clinical Exec" = "#2C6B8E"))  + ylim(11.75,12.75)
cmi <- ggplot(outcomes, aes(x=year, y=`Case Mix Index_m`, color=sample, linetype=sample, shape=sample)) + geom_point(size=3) + geom_line() +
  geom_vline(xintercept = 2012, linetype = "dotted") + xlim(2010,2014) +
  geom_line(linewidth=1.2) + theme_bw() + xlab("") + ylab("Case Mix Index\n") + theme(legend.title = element_blank()) +
  theme(text=element_text(size=18)) + scale_color_manual(values = c("For-Profit" = "#D65828", "Never Clinical Exec" = "#AED1EC", "Ever Clinical Exec" ="#6BAED6", "Always Clinical Exec" = "#2C6B8E"))  + ylim(1,2)

ggarrange(read, mort, cmi,
          nrow = 3,
          ncol = 1,
          common.legend = T,
          legend = "right")

ggsave(paste0(objects_path, "outcomes_withforprofit_graph.pdf"), height=9, width=7, units="in")

# TABLE 7: Summary statistics by specialty ####
# get rid of changes in num docs and changes in int med docs
specialty_data <- hospital_data %>%
  filter(year>=2010 & year<=2014) %>%
  group_by(ID) %>%
  arrange(year) %>%
  mutate(lag_num_int_med = dplyr::lag(num_int_med_doctors)) %>%
  ungroup() %>%
  mutate(change_num_int_med = ifelse(num_int_med_doctors!=lag_num_int_med,1,0)) %>%
  group_by(ID) %>%
  mutate(no_num_int_med_changes = ifelse(sum(change_num_int_med, na.rm=T)==0,1,0)) %>%
  ungroup() 

# create variables never int med, always int med, and always only int med
specialty_data <- specialty_data %>%
  mutate(num_other_doctors = num_doctors-num_int_med_doctors) %>%
  mutate(never_int_med = ifelse(num_int_med_doctors==0,1,0)) %>%
  mutate(always_int_med = ifelse(num_int_med_doctors>0,1,0)) %>%
  mutate(always_only_int_med = ifelse(num_other_doctors==0 & num_int_med_doctors>0,1,0)) %>%
  mutate(post_2012 = ifelse(year>2012,1,0)) %>%
  filter((always_int_med==1 & no_num_int_med_changes==1) | (never_int_med==1 & no_num_md_change_2010_2014)) %>%
  filter(num_doctors>0) 

n_int_med <- specialty_data %>%
  filter(always_int_med==1) %>%
  distinct(ID) %>%
  nrow()
int_med_stats <- specialty_data %>%
  filter(always_int_med==1) %>%
  mutate(rate_exec=num_doctors/num_execs) %>%
  mutate(rate_int_exec=num_int_med_doctors/num_execs) %>%
  mutate(ever_has_md = ifelse(is.na(ever_has_md),0,ever_has_md)) %>%
  summarise_at(c("Number Beds"="beds",
                 "Penalized for AMI"="ever_pen_ha_only", "Penalized for HF"="ever_pen_hf_only",
                 "Penalized for Pneumonia"="ever_pen_pnem_only",
                 "Penalized for AMI + HF"="ever_pen_ami_hf",
                 "Penalized for AMI + Pneumonia"="ever_pen_ami_pneum",
                 "Penalized for HF + Pneumonia"="ever_pen_hf_pneum",
                 "Penalized for All Conditions"="ever_pen_all",
                 "Ever Received HVBP Incentive" = "ever_hvbp",
                 "Academic Med. Center"="academic", 
                 "Physician Owned"="PHYGP", 
                 "Owns Subsidiary"="ever_subs", 
                 "System Affiliated"="ever_part_of_system",
                 "Has a CMO"="ever_cmo",
                 "Number Executives"="num_execs",
                 "Fraction Clinical Execs"="rate_exec",
                 "Fraction Int. Medicine Execs"="rate_int_exec",
                 "Has Clinical CEO"="ever_ceo_md"
  ), 
  list(m=mean), na.rm=TRUE) %>%
  mutate_if(is.numeric, ~ifelse(abs(.)==Inf,NA,.))  %>%
  gather(key=var,value=value) %>%
  extract(col="var",into=c("variable", "statistic"), regex=("(.*)_(.*)$")) %>%
  spread(key=statistic, value=value) %>%
  relocate(variable,m) %>%
  add_row(variable="Num. Hospitals", m=n_int_med) %>%
  rename(int_med=m)

n_other <- specialty_data %>%
  filter(never_int_med==1) %>%
  distinct(ID) %>%
  nrow()
other_stats <- specialty_data %>%
  filter(never_int_med==1) %>%
  mutate(rate_exec=num_doctors/num_execs) %>%
  mutate(rate_int_exec=num_int_med_doctors/num_execs) %>%
  mutate(ever_has_md = ifelse(is.na(ever_has_md),0,ever_has_md)) %>%
  summarise_at(c("Number Beds"="beds",
                 "Penalized for AMI"="ever_pen_ha_only", "Penalized for HF"="ever_pen_hf_only",
                 "Penalized for Pneumonia"="ever_pen_pnem_only",
                 "Penalized for AMI + HF"="ever_pen_ami_hf",
                 "Penalized for AMI + Pneumonia"="ever_pen_ami_pneum",
                 "Penalized for HF + Pneumonia"="ever_pen_hf_pneum",
                 "Penalized for All Conditions"="ever_pen_all",
                 "Ever Received HVBP Incentive" = "ever_hvbp",
                 "Academic Med. Center"="academic", 
                 "Physician Owned"="PHYGP", 
                 "Owns Subsidiary"="ever_subs", 
                 "System Affiliated"="ever_part_of_system",
                 "Has a CMO"="ever_cmo",
                 "Number Executives"="num_execs",
                 "Fraction Clinical Execs"="rate_exec",
                 "Fraction Int. Medicine Execs"="rate_int_exec",
                 "Has Clinical CEO"="ever_ceo_md"
  ), 
  list(m=mean), na.rm=TRUE) %>%
  mutate_if(is.numeric, ~ifelse(abs(.)==Inf,NA,.))  %>%
  gather(key=var,value=value) %>%
  extract(col="var",into=c("variable", "statistic"), regex=("(.*)_(.*)$")) %>%
  spread(key=statistic, value=value) %>%
  relocate(variable,m) %>%
  add_row(variable="Num. Hospitals", m=n_other) %>%
  rename(other=m)

specialty_stats <- int_med_stats %>%
  left_join(other_stats, by="variable") 

specialty_stats_tab <- knitr::kable(specialty_stats[c(1,7,17,18,8,3,4,5,6,2,11,14,16,12,13,15,10,19),],
                                    row.names = FALSE,
                                    format="latex",
                                    table.envir="table",
                                    col.names=c("Variable","Always Int Med Exec", "Always Other Exec"),
                                    digits=2,
                                    caption="\\label{tab:specialty}Summary Statistics by Specialty",
                                    booktabs=TRUE,
                                    escape=F,
                                    align=c("l","c","c","c","c","c"),
                                    position="ht!") %>%
  kable_styling(full_width=F) %>%
  pack_rows(index = c("Hospital Characteristics" = 4, "Executive Team Characteristics" = 5, "Penalty/Payment Variables" = 8,
                      " " = 1))
write(specialty_stats_tab, file="Tables/specialty_sumstats.tex")

# FIGURE 14/15: binned treatment, limiting sample ####
# continuous treatment, limiting to similar size executive teams 
cont_main_analysis_data_size <- cont_main_analysis_data %>%
  filter(num_execs>=1 & num_execs<=7)

# below median 
cont_belowmed_read_data_size <- cont_main_analysis_data_size %>%
  filter(year!=2015 & (below_med==1 | frac_doc_exec==0)) %>%
  mutate(did = post_2012 * below_med==1) %>%
  select(ID, year, weightedavg_read, did, cmiv) %>%
  na.omit() %>%
  mutate(count=1) %>%
  group_by(ID) %>%
  mutate(min=min(year[did==1])) %>%
  mutate(sum=sum(count)) %>%
  ungroup() %>%
  filter(sum==5) %>%
  filter(min!=2014) %>%
  select(-sum, -count, -min) %>%
  mutate(ID=as.factor(ID))
cont_belowmed_read_panel_size <- panel.matrices(as.data.frame(cont_belowmed_read_data_size))
X_mat_cont_belowmed_read_size <- cont_belowmed_read_data_size %>%
  select(ID, year, cmiv) %>%
  melt(id.var=c("ID", "year")) %>%
  nest_by(variable) %>%
  mutate(X=list(dcast(data.table(cont_belowmed_read_data_size), ID~year) %>%
                  .[data.table(ID=rownames(cont_belowmed_read_panel_size$Y)), on="ID"] %>%
                  .[, ID := NULL] %>%
                  as.matrix()
  )) %>%
  .$X %>%
  abind(along=2)

cont_belowmed_read_did_size <- synthdid_estimate(cont_belowmed_read_panel_size$Y, cont_belowmed_read_panel_size$N0, cont_belowmed_read_panel_size$T0, X=X_mat_cont_belowmed_read_size)

sprintf('point estimate: %1.2f', cont_belowmed_read_did_size)
cont_belowmed_read_se_size = sqrt(vcov(cont_belowmed_read_did_size, method='placebo'))

cont_belowmed_read_plot_size <- synthdid_plot(cont_belowmed_read_did_size, facet.vertical=FALSE,
                                              control.name='Never Clinical Exec', treated.name='< Median Clinical Execs',
                                              lambda.comparable=TRUE, se.method = 'none',
                                              lambda.plot.scale = 0,
                                              trajectory.linetype = 1, line.width=.8, effect.curvature=-.4,
                                              trajectory.alpha=1, effect.alpha=0,
                                              diagram.alpha=0, onset.alpha=1) +
  theme(legend.position=c(.5,-.095), legend.direction='horizontal',
        legend.key=element_blank(), legend.background=element_blank(),
        strip.background=element_blank(), strip.text.x = element_blank(),
        text = element_text(size=18)) + ylim(18.5,22.5) +
  scale_color_manual(values = c("For-profit" = "#D65828", "Never Clinical Exec" = "#AED1EC", "< Median Clinical Execs" ="#6BAED6", "> Median Clinical Execs" = "#2C6B8E"))  +
  annotate(geom="label", x=2013.1, y=22.5, 
           label=paste0("ATT (s.e.) = ", round(cont_belowmed_read_did_size,2)," (",round(cont_belowmed_read_se_size,2),")"), 
           size=4,
           fill="gray90") 
ggsave(cont_belowmed_read_plot_size, filename=paste0(objects_path,"cont_belowmedread_md_nomd_size_synth_graph.pdf"), width=5.5, height=6, units="in")


# above median 
cont_abovemed_read_data_size <- cont_main_analysis_data_size %>%
  filter(year!=2015 & (above_med==1 | frac_doc_exec==0)) %>%
  mutate(did = post_2012 * above_med==1) %>%
  select(ID, year, weightedavg_read, did, cmiv) %>%
  na.omit() %>%
  mutate(count=1) %>%
  group_by(ID) %>%
  mutate(min=min(year[did==1])) %>%
  mutate(sum=sum(count)) %>%
  ungroup() %>%
  filter(sum==5) %>%
  filter(min!=2014) %>%
  select(-sum, -count, -min) %>%
  mutate(ID=as.factor(ID))
cont_abovemed_read_panel_size <- panel.matrices(as.data.frame(cont_abovemed_read_data_size))
X_mat_cont_abovemed_read_size <- cont_abovemed_read_data_size %>%
  select(ID, year, cmiv) %>%
  melt(id.var=c("ID", "year")) %>%
  nest_by(variable) %>%
  mutate(X=list(dcast(data.table(cont_abovemed_read_data_size), ID~year) %>%
                  .[data.table(ID=rownames(cont_abovemed_read_panel_size$Y)), on="ID"] %>%
                  .[, ID := NULL] %>%
                  as.matrix()
  )) %>%
  .$X %>%
  abind(along=2)

cont_abovemed_read_did_size <- synthdid_estimate(cont_abovemed_read_panel_size$Y, cont_abovemed_read_panel_size$N0, cont_abovemed_read_panel_size$T0, X=X_mat_cont_abovemed_read_size)

sprintf('point estimate: %1.2f', cont_abovemed_read_did_size)
cont_abovemed_read_se_size = sqrt(vcov(cont_abovemed_read_did_size))

cont_abovemed_read_plot_size <- synthdid_plot(cont_abovemed_read_did_size, facet.vertical=FALSE,
                                              control.name='Never Clinical Exec', treated.name='> Median Clinical Execs',
                                              lambda.comparable=TRUE, se.method = 'none',
                                              lambda.plot.scale = 0,
                                              trajectory.linetype = 1, line.width=.8, effect.curvature=-.4,
                                              trajectory.alpha=1, effect.alpha=0,
                                              diagram.alpha=0, onset.alpha=1) +
  theme(legend.position=c(.5,-.095), legend.direction='horizontal',
        legend.key=element_blank(), legend.background=element_blank(),
        strip.background=element_blank(), strip.text.x = element_blank(),
        text = element_text(size=18)) + ylim(19,22.5) +
  scale_color_manual(values = c("For-profit" = "#D65828", "Never Clinical Exec" = "#AED1EC", "< Median Clinical Execs" ="#6BAED6", "> Median Clinical Execs" = "#2C6B8E"))  +
  annotate(geom="label", x=2013.1, y=22.5, 
           label=paste0("ATT (s.e.) = ", round(cont_abovemed_read_did_size,2)," (",round(cont_abovemed_read_se_size,2),")"), 
           size=4,
           fill="gray90") 
ggsave(cont_abovemed_read_plot_size, filename=paste0(objects_path,"cont_abovemedread_md_nomd_size_synth_graph.pdf"), width=5.5, height=6, units="in")

# Mortality
# below median 
cont_belowmed_mort_data_size <- cont_main_analysis_data_size %>%
  filter(year!=2015 & (below_med==1 | frac_doc_exec==0)) %>%
  mutate(did = post_2012 * below_med==1) %>%
  select(ID, year, weightedavg_mort, did, cmiv) %>%
  na.omit() %>%
  mutate(count=1) %>%
  group_by(ID) %>%
  mutate(min=min(year[did==1])) %>%
  mutate(sum=sum(count)) %>%
  ungroup() %>%
  filter(sum==5) %>%
  filter(min!=2014) %>%
  select(-sum, -count, -min) %>%
  mutate(ID=as.factor(ID))
cont_belowmed_mort_panel_size <- panel.matrices(as.data.frame(cont_belowmed_mort_data_size))
X_mat_cont_belowmed_mort_size <- cont_belowmed_mort_data_size %>%
  select(ID, year, cmiv) %>%
  melt(id.var=c("ID", "year")) %>%
  nest_by(variable) %>%
  mutate(X=list(dcast(data.table(cont_belowmed_mort_data_size), ID~year) %>%
                  .[data.table(ID=rownames(cont_belowmed_mort_panel_size$Y)), on="ID"] %>%
                  .[, ID := NULL] %>%
                  as.matrix()
  )) %>%
  .$X %>%
  abind(along=2)

cont_belowmed_mort_did_size <- synthdid_estimate(cont_belowmed_mort_panel_size$Y, cont_belowmed_mort_panel_size$N0, cont_belowmed_mort_panel_size$T0, X=X_mat_cont_belowmed_mort_size)

sprintf('point estimate: %1.2f', cont_belowmed_mort_did_size)
cont_belowmed_mort_se_size = sqrt(vcov(cont_belowmed_mort_did_size, method='placebo'))

cont_belowmed_mort_plot_size <- synthdid_plot(cont_belowmed_mort_did_size, facet.vertical=FALSE,
                                              control.name='Never Clinical Exec', treated.name='< Median Clinical Execs',
                                              lambda.comparable=TRUE, se.method = 'none',
                                              lambda.plot.scale = 0,
                                              trajectory.linetype = 1, line.width=.8, effect.curvature=-.4,
                                              trajectory.alpha=1, effect.alpha=0,
                                              diagram.alpha=0, onset.alpha=1) +
  theme(legend.position=c(.5,-.095), legend.direction='horizontal',
        legend.key=element_blank(), legend.background=element_blank(),
        strip.background=element_blank(), strip.text.x = element_blank(),
        text = element_text(size=18)) + ylim(10.5,13.5) +
  scale_color_manual(values = c("For-profit" = "#D65828", "Never Clinical Exec" = "#AED1EC", "< Median Clinical Execs" ="#6BAED6", "> Median Clinical Execs" = "#2C6B8E"))  +
  annotate(geom="label", x=2013.1, y=13.5, 
           label=paste0("ATT (s.e.) = ", round(cont_belowmed_mort_did_size,2)," (",round(cont_belowmed_mort_se_size,2),")"), 
           size=4,
           fill="gray90") 
ggsave(cont_belowmed_mort_plot_size, filename=paste0(objects_path,"cont_belowmedmort_md_nomd_size_synth_graph.pdf"), width=5.5, height=6, units="in")


# above median 
cont_abovemed_mort_data_size <- cont_main_analysis_data_size %>%
  filter(year!=2015 & (above_med==1 | frac_doc_exec==0)) %>%
  mutate(did = post_2012 * above_med==1) %>%
  select(ID, year, weightedavg_mort, did, cmiv) %>%
  na.omit() %>%
  mutate(count=1) %>%
  group_by(ID) %>%
  mutate(min=min(year[did==1])) %>%
  mutate(sum=sum(count)) %>%
  ungroup() %>%
  filter(sum==5) %>%
  filter(min!=2014) %>%
  select(-sum, -count, -min) %>%
  mutate(ID=as.factor(ID))
cont_abovemed_mort_panel_size <- panel.matrices(as.data.frame(cont_abovemed_mort_data_size))
X_mat_cont_abovemed_mort_size <- cont_abovemed_mort_data_size %>%
  select(ID, year, cmiv) %>%
  melt(id.var=c("ID", "year")) %>%
  nest_by(variable) %>%
  mutate(X=list(dcast(data.table(cont_abovemed_mort_data_size), ID~year) %>%
                  .[data.table(ID=rownames(cont_abovemed_mort_panel_size$Y)), on="ID"] %>%
                  .[, ID := NULL] %>%
                  as.matrix()
  )) %>%
  .$X %>%
  abind(along=2)

cont_abovemed_mort_did_size <- synthdid_estimate(cont_abovemed_mort_panel_size$Y, cont_abovemed_mort_panel_size$N0, cont_abovemed_mort_panel_size$T0, X=X_mat_cont_abovemed_mort_size)

sprintf('point estimate: %1.2f', cont_abovemed_mort_did_size)
cont_abovemed_mort_se_size = sqrt(vcov(cont_abovemed_mort_did_size))

cont_abovemed_mort_plot_size <- synthdid_plot(cont_abovemed_mort_did_size, facet.vertical=FALSE,
                                              control.name='Never Clinical Exec', treated.name='> Median Clinical Execs',
                                              lambda.comparable=TRUE, se.method = 'none',
                                              lambda.plot.scale = 0,
                                              trajectory.linetype = 1, line.width=.8, effect.curvature=-.4,
                                              trajectory.alpha=1, effect.alpha=0,
                                              diagram.alpha=0, onset.alpha=1) +
  theme(legend.position=c(.5,-.095), legend.direction='horizontal',
        legend.key=element_blank(), legend.background=element_blank(),
        strip.background=element_blank(), strip.text.x = element_blank(),
        text = element_text(size=18)) + ylim(10.5,13.5) +
  scale_color_manual(values = c("For-profit" = "#D65828", "Never Clinical Exec" = "#AED1EC", "< Median Clinical Execs" ="#6BAED6", "> Median Clinical Execs" = "#2C6B8E"))  +
  annotate(geom="label", x=2013.1, y=13.5, 
           label=paste0("ATT (s.e.) = ", round(cont_abovemed_mort_did_size,2)," (",round(cont_abovemed_mort_se_size,2),")"), 
           size=4,
           fill="gray90") 
ggsave(cont_abovemed_mort_plot_size, filename=paste0(objects_path,"cont_abovemedmort_md_nomd_size_synth_graph.pdf"), width=5.5, height=6, units="in")


# FIGURE 16: case mix index as outcome ####
main_analysis_data <- hospital_data %>%
  filter(no_num_md_change_2010_2014==1 & !is.na(ever_has_md)) %>%
  mutate(never_has_md=ifelse(ever_has_md==1,0,1),
         post_2012 = ifelse(year>2012,1,0)) %>%
  mutate(did=post_2012*ever_has_md) %>%
  mutate(count=1) %>%
  group_by(ID, year) %>%
  mutate(sum=sum(count)) %>%
  ungroup() %>%
  filter(!(sum==2 & is.na(weightedavg_read))) %>%
  select(-count, -sum)

cmi_data <- main_analysis_data %>%
  filter(year!=2015) %>%
  select(ID, year, cmiv, did) %>%
  na.omit() %>%
  mutate(count=1) %>%
  group_by(ID) %>%
  mutate(min=min(year[did==1])) %>%
  mutate(sum=sum(count)) %>%
  ungroup() %>%
  filter(sum==5) %>%
  filter(min!=2014) %>%
  select(-sum, -count, -min) %>%
  mutate(ID=as.factor(ID))
cmi_panel <- panel.matrices(as.data.frame(cmi_data))

cmi_did <- synthdid_estimate(cmi_panel$Y, cmi_panel$N0, cmi_panel$T0)

cmi_se = sqrt(vcov(cmi_did))

main_cmi_plot <- synthdid_plot(cmi_did, facet.vertical=FALSE,
                               control.name='Never Clinical Exec', treated.name='Always Clinical Exec',
                               lambda.comparable=TRUE, se.method = 'none',
                               lambda.plot.scale = 0,
                               trajectory.linetype = 1, line.width=.8, effect.curvature=-.4,
                               trajectory.alpha=1, effect.alpha=0,
                               diagram.alpha=0, onset.alpha=1) +
  theme(legend.position=c(.5,-.095), legend.direction='horizontal',
        legend.key=element_blank(), legend.background=element_blank(),
        strip.background=element_blank(), strip.text.x = element_blank(),
        text = element_text(size=18)) + ylim(0.5,2) +
  scale_color_manual(values = c("For-profit" = "#D65828", "Never Clinical Exec" = "#AED1EC", "Ever Clinical Exec" ="#6BAED6", "Always Clinical Exec" = "#2C6B8E"))  +
  annotate(geom="label", x=2013.1, y=2, 
           label=paste0("ATT (s.e.) = ", round(cmi_did,2)," (",round(cmi_se,2),")"), 
           size=4,
           fill="gray90") 
ggsave(main_cmi_plot, filename=paste0(objects_path,"cmi_md_nomd_synth_graph.pdf"), width=5.5, height=6, units="in")


# TABLE 8: results by condition ####

condition_analysis_data <- hospital_data %>%
  filter(no_num_md_change_2010_2014==1 & !is.na(ever_has_md)) %>%
  mutate(never_has_md=ifelse(ever_has_md==1,0,1),
         post_2012 = ifelse(year>2012,1,0)) %>%
  mutate(did=post_2012*ever_has_md) %>%
  mutate(count=1) %>%
  group_by(ID, year) %>%
  mutate(sum=sum(count)) %>%
  ungroup() %>%
  filter(!(sum==2 & is.na(weightedavg_read))) %>%
  select(-count, -sum)

# Readmission: heart attack
ha_read_data <- condition_analysis_data %>%
  filter(year!=2015) %>%
  select(ID, year, rate_heartattack_readmission, did, cmiv) %>%
  na.omit() %>%
  mutate(count=1) %>%
  group_by(ID) %>%
  mutate(min=min(year[did==1])) %>%
  mutate(sum=sum(count)) %>%
  ungroup() %>%
  filter(sum==5) %>%
  filter(min!=2014) %>%
  select(-sum, -count, -min) %>%
  mutate(ID=as.factor(ID))
ha_read_panel <- panel.matrices(as.data.frame(ha_read_data))
X_mat_ha_read <- ha_read_data %>%
  select(ID, year, cmiv) %>%
  melt(id.var=c("ID", "year")) %>%
  nest_by(variable) %>%
  mutate(X=list(dcast(data.table(ha_read_data), ID~year) %>%
                  .[data.table(ID=rownames(ha_read_panel$Y)), on="ID"] %>%
                  .[, ID := NULL] %>%
                  as.matrix()
  )) %>%
  .$X %>%
  abind(along=2)

ha_read_did <- synthdid_estimate(ha_read_panel$Y, ha_read_panel$N0, ha_read_panel$T0, X=X_mat_ha_read)
ha_read_se <- sqrt(vcov(ha_read_did))
ha_read_obs <- (summary(ha_read_did)$dimensions[["N0"]]+summary(ha_read_did)$dimensions[["N1"]])*5

# Readmission: heart failure
hf_read_data <- condition_analysis_data %>%
  filter(year!=2015) %>%
  select(ID, year, rate_heartfailure_readmission, did, cmiv) %>%
  na.omit() %>%
  mutate(count=1) %>%
  group_by(ID) %>%
  mutate(min=min(year[did==1])) %>%
  mutate(sum=sum(count)) %>%
  ungroup() %>%
  filter(sum==5) %>%
  filter(min!=2014) %>%
  select(-sum, -count, -min) %>%
  mutate(ID=as.factor(ID))
hf_read_panel <- panel.matrices(as.data.frame(hf_read_data))
X_mat_hf_read <- hf_read_data %>%
  select(ID, year, cmiv) %>%
  melt(id.var=c("ID", "year")) %>%
  nest_by(variable) %>%
  mutate(X=list(dcast(data.table(hf_read_data), ID~year) %>%
                  .[data.table(ID=rownames(hf_read_panel$Y)), on="ID"] %>%
                  .[, ID := NULL] %>%
                  as.matrix()
  )) %>%
  .$X %>%
  abind(along=2)

hf_read_did <- synthdid_estimate(hf_read_panel$Y, hf_read_panel$N0, hf_read_panel$T0, X=X_mat_hf_read)
hf_read_se <- sqrt(vcov(hf_read_did))
hf_read_obs <- (summary(hf_read_did)$dimensions[["N0"]]+summary(hf_read_did)$dimensions[["N1"]])*5

# Readmission: pneumonia
pn_read_data <- condition_analysis_data %>%
  filter(year!=2015) %>%
  select(ID, year, rate_pneum_readmission, did, cmiv) %>%
  na.omit() %>%
  mutate(count=1) %>%
  group_by(ID) %>%
  mutate(min=min(year[did==1])) %>%
  mutate(sum=sum(count)) %>%
  ungroup() %>%
  filter(sum==5) %>%
  filter(min!=2014) %>%
  select(-sum, -count, -min) %>%
  mutate(ID=as.factor(ID))
pn_read_panel <- panel.matrices(as.data.frame(pn_read_data))
X_mat_pn_read <- pn_read_data %>%
  select(ID, year, cmiv) %>%
  melt(id.var=c("ID", "year")) %>%
  nest_by(variable) %>%
  mutate(X=list(dcast(data.table(pn_read_data), ID~year) %>%
                  .[data.table(ID=rownames(pn_read_panel$Y)), on="ID"] %>%
                  .[, ID := NULL] %>%
                  as.matrix()
  )) %>%
  .$X %>%
  abind(along=2)

pn_read_did <- synthdid_estimate(pn_read_panel$Y, pn_read_panel$N0, pn_read_panel$T0, X=X_mat_pn_read)
pn_read_se <- sqrt(vcov(pn_read_did))
pn_read_obs <- (summary(pn_read_did)$dimensions[["N0"]]+summary(pn_read_did)$dimensions[["N1"]])*5

# Mortality: heart attack
ha_mort_data <- condition_analysis_data %>%
  filter(year!=2015) %>%
  select(ID, year, rate_heartattack_mortality, did, cmiv) %>%
  na.omit() %>%
  mutate(count=1) %>%
  group_by(ID) %>%
  mutate(min=min(year[did==1])) %>%
  mutate(sum=sum(count)) %>%
  ungroup() %>%
  filter(sum==5) %>%
  filter(min!=2014) %>%
  select(-sum, -count, -min) %>%
  mutate(ID=as.factor(ID))
ha_mort_panel <- panel.matrices(as.data.frame(ha_mort_data))
X_mat_ha_mort <- ha_mort_data %>%
  select(ID, year, cmiv) %>%
  melt(id.var=c("ID", "year")) %>%
  nest_by(variable) %>%
  mutate(X=list(dcast(data.table(ha_mort_data), ID~year) %>%
                  .[data.table(ID=rownames(ha_mort_panel$Y)), on="ID"] %>%
                  .[, ID := NULL] %>%
                  as.matrix()
  )) %>%
  .$X %>%
  abind(along=2)

ha_mort_did <- synthdid_estimate(ha_mort_panel$Y, ha_mort_panel$N0, ha_mort_panel$T0, X=X_mat_ha_mort)
ha_mort_se <- sqrt(vcov(ha_mort_did))
ha_mort_obs <- (summary(ha_mort_did)$dimensions[["N0"]]+summary(ha_mort_did)$dimensions[["N1"]])*5

# Mortality: heart failure
hf_mort_data <- condition_analysis_data %>%
  filter(year!=2015) %>%
  select(ID, year, rate_heartfailure_mortality, did, cmiv) %>%
  na.omit() %>%
  mutate(count=1) %>%
  group_by(ID) %>%
  mutate(min=min(year[did==1])) %>%
  mutate(sum=sum(count)) %>%
  ungroup() %>%
  filter(sum==5) %>%
  filter(min!=2014) %>%
  select(-sum, -count, -min) %>%
  mutate(ID=as.factor(ID))
hf_mort_panel <- panel.matrices(as.data.frame(hf_mort_data))
X_mat_hf_mort <- hf_mort_data %>%
  select(ID, year, cmiv) %>%
  melt(id.var=c("ID", "year")) %>%
  nest_by(variable) %>%
  mutate(X=list(dcast(data.table(hf_mort_data), ID~year) %>%
                  .[data.table(ID=rownames(hf_mort_panel$Y)), on="ID"] %>%
                  .[, ID := NULL] %>%
                  as.matrix()
  )) %>%
  .$X %>%
  abind(along=2)

hf_mort_did <- synthdid_estimate(hf_mort_panel$Y, hf_mort_panel$N0, hf_mort_panel$T0, X=X_mat_hf_mort)
hf_mort_se <- sqrt(vcov(hf_mort_did))
hf_mort_obs <- (summary(hf_mort_did)$dimensions[["N0"]]+summary(hf_mort_did)$dimensions[["N1"]])*5

# Mortality: pneumonia
pn_mort_data <- condition_analysis_data %>%
  filter(year!=2015) %>%
  select(ID, year, rate_pneum_mortality, did, cmiv) %>%
  na.omit() %>%
  mutate(count=1) %>%
  group_by(ID) %>%
  mutate(min=min(year[did==1])) %>%
  mutate(sum=sum(count)) %>%
  ungroup() %>%
  filter(sum==5) %>%
  filter(min!=2014) %>%
  select(-sum, -count, -min) %>%
  mutate(ID=as.factor(ID))
pn_mort_panel <- panel.matrices(as.data.frame(pn_mort_data))
X_mat_pn_mort <- pn_mort_data %>%
  select(ID, year, cmiv) %>%
  melt(id.var=c("ID", "year")) %>%
  nest_by(variable) %>%
  mutate(X=list(dcast(data.table(pn_mort_data), ID~year) %>%
                  .[data.table(ID=rownames(pn_mort_panel$Y)), on="ID"] %>%
                  .[, ID := NULL] %>%
                  as.matrix()
  )) %>%
  .$X %>%
  abind(along=2)

pn_mort_did <- synthdid_estimate(pn_mort_panel$Y, pn_mort_panel$N0, pn_mort_panel$T0, X=X_mat_pn_mort)
pn_mort_se <- sqrt(vcov(pn_mort_did))
pn_mort_obs <- (summary(pn_mort_did)$dimensions[["N0"]]+summary(pn_mort_did)$dimensions[["N1"]])*5

coef <- c(ha_read_did, hf_read_did, pn_read_did, ha_mort_did, hf_mort_did, pn_mort_did)
se <- c(ha_read_se, hf_read_se, pn_read_se, ha_mort_se, hf_mort_se, pn_mort_se)

ha_read_obs <- format(ha_read_obs, big.mark = ",", scientific = FALSE)
hf_read_obs <- format(hf_read_obs, big.mark = ",", scientific = FALSE)
pn_read_obs <- format(pn_read_obs, big.mark = ",", scientific = FALSE)
ha_mort_obs <- format(ha_mort_obs, big.mark = ",", scientific = FALSE)
hf_mort_obs <- format(hf_mort_obs, big.mark = ",", scientific = FALSE)
pn_mort_obs <- format(pn_mort_obs, big.mark = ",", scientific = FALSE)

estimates <- as.data.frame(list(coef=coef, se=se)) %>%
  mutate(p=round((2*(1-pnorm(abs(coef/se)))), 4)) %>%
  mutate(coef=round(coef,2),
         se=round(se,2)) %>%
  mutate(stars = case_when(
    p >= 0.05 ~ "",
    p < 0.001 ~ "$^{***}$",
    p < 0.01 ~ "$^{**}$",
    p < 0.05 ~ "$^{*}$")) %>%
  mutate(coef=paste0(coef,stars)) %>%
  mutate(se = paste0("(",se,")")) %>%
  select(-p, -stars)

estimates <- as.data.frame(t(estimates)) %>%
  mutate(x=c("Ever Clinical Exec x Post Program", " ")) %>%
  select(x, V1, V2, V3, V4, V5, V6) %>%
  add_row(x="") %>%
  add_row(x="Hospital FE", V1="$\\checkmark$", V2="$\\checkmark$", V3="$\\checkmark$", V4="$\\checkmark$", V5="$\\checkmark$", V6="$\\checkmark$") %>%
  add_row(x="Year FE", V1="$\\checkmark$", V2="$\\checkmark$", V3="$\\checkmark$", V4="$\\checkmark$", V5="$\\checkmark$", V6="$\\checkmark$") %>%
  add_row(x="Observations", V1=ha_read_obs, V2=hf_read_obs, V3=pn_read_obs, 
          V4=ha_mort_obs, V5=hf_mort_obs, V6=pn_mort_obs)

condition_synthdid_table <- knitr::kable(estimates, format="latex",
                      row.names = FALSE,
                      col.names = c("", "(1)", "(2)", "(3)", "(4)", "(5)", "(6)"),
                      booktabs = TRUE,
                      table.envir="table",
                      caption="\\label{tab:results_by_condition}Effect of Clinically Trained Executive on Condition-Specific Readmission and Mortality Rates",
                      escape=F,
                      align=c("l","c","c","c"),
                      position="ht!") %>%
  add_header_above(c(" ", "AMI"=1, "HF"=1, "Pneum."=1,"AMI"=1, "HF"=1, "Pneum."=1)) %>%
  add_header_above(c(" ", "Readmission Rates"=3, "Mortality Rates"=3), underline=FALSE, line=FALSE) %>%
  add_footnote(c("Standard errors are clustered at the hospital level.",
                 "Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1"), notation="none")

write(condition_synthdid_table, file="Tables/condition_synthdid_table.tex")

# TABLE 9: main analysis, TWFE, matching, and synthetic control specifications ####
# create matched sample 
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
  mutate(matched_mainanalysis=1)
hospital_data <- hospital_data %>%
  left_join(matched_data, by="MCRNUM") %>%
  mutate(matched_mainanalysis = ifelse(is.na(matched_mainanalysis),0,matched_mainanalysis))

main_analysis_data <- hospital_data %>%
  filter(no_num_md_change_2010_2014==1 & !is.na(ever_has_md)) %>%
  mutate(never_has_md=ifelse(ever_has_md==1,0,1),
         post_2012 = ifelse(year>2012,1,0)) %>%
  mutate(did=post_2012*ever_has_md) %>%
  mutate(count=1) %>%
  group_by(ID, year) %>%
  mutate(sum=sum(count)) %>%
  ungroup() %>%
  filter(!(sum==2 & is.na(weightedavg_read))) %>%
  select(-count, -sum)

# twfe
main_read_twfe <- fixest::feols(weightedavg_read ~ did | ID + year, cluster = ~ID, data=main_analysis_data)
main_mort_twfe <- fixest::feols(weightedavg_mort ~ did | ID + year, cluster = ~ID, data=main_analysis_data)

# matched twfe
main_match_read_twfe <- fixest::feols(weightedavg_read ~ did | ID + year, cluster = ~ID, data=filter(main_analysis_data, matched_mainanalysis==1))
main_match_mort_twfe <- fixest::feols(weightedavg_mort ~ did | ID + year, cluster = ~ID, data=filter(main_analysis_data, matched_mainanalysis==1))

# synthetic control
wa_read_data_sc <- main_analysis_data %>%
  filter(year!=2015) %>%
  select(ID, year, weightedavg_read, did, cmiv) %>%
  na.omit() %>%
  mutate(count=1) %>%
  group_by(ID) %>%
  mutate(min=min(year[did==1])) %>%
  mutate(sum=sum(count)) %>%
  ungroup() %>%
  filter(sum==5) %>%
  filter(min!=2014) %>%
  select(-sum, -count, -min) %>%
  mutate(ID=as.factor(ID))
wa_read_panel_sc <- panel.matrices(as.data.frame(wa_read_data_sc))
X_mat_wa_read_sc <- wa_read_data_sc %>%
  select(ID, year, cmiv) %>%
  melt(id.var=c("ID", "year")) %>%
  nest_by(variable) %>%
  mutate(X=list(dcast(data.table(wa_read_data_sc), ID~year) %>%
                  .[data.table(ID=rownames(wa_read_panel_sc$Y)), on="ID"] %>%
                  .[, ID := NULL] %>%
                  as.matrix()
  )) %>%
  .$X %>%
  abind(along=2)

wa_read_sc <- sc_estimate(wa_read_panel_sc$Y, wa_read_panel_sc$N0, wa_read_panel_sc$T0, X=X_mat_wa_read_sc)
wa_read_sc_se = sqrt(vcov(wa_read_sc))
read_sc_obs <- (summary(wa_read_sc)$dimensions[["N0"]]+summary(wa_read_sc)$dimensions[["N1"]])*5

wa_mort_data_sc <- main_analysis_data %>%
  filter(year!=2015) %>%
  select(ID, year, weightedavg_mort, did, cmiv) %>%
  na.omit() %>%
  mutate(count=1) %>%
  group_by(ID) %>%
  mutate(min=min(year[did==1])) %>%
  mutate(sum=sum(count)) %>%
  ungroup() %>%
  filter(sum==5) %>%
  filter(min!=2014) %>%
  select(-sum, -count, -min) %>%
  mutate(ID=as.factor(ID))
wa_mort_panel_sc <- panel.matrices(as.data.frame(wa_mort_data_sc))
X_mat_wa_mort_sc <- wa_mort_data_sc %>%
  select(ID, year, cmiv) %>%
  melt(id.var=c("ID", "year")) %>%
  nest_by(variable) %>%
  mutate(X=list(dcast(data.table(wa_mort_data_sc), ID~year) %>%
                  .[data.table(ID=rownames(wa_mort_panel_sc$Y)), on="ID"] %>%
                  .[, ID := NULL] %>%
                  as.matrix()
  )) %>%
  .$X %>%
  abind(along=2)

wa_mort_sc <- sc_estimate(wa_mort_panel_sc$Y, wa_mort_panel_sc$N0, wa_mort_panel_sc$T0, X=X_mat_wa_mort_sc)
wa_mort_sc_se = sqrt(vcov(wa_mort_sc))
mort_sc_obs <- (summary(wa_mort_sc)$dimensions[["N0"]]+summary(wa_mort_sc)$dimensions[["N1"]])*5

# create objects for manual table 
coef <- c(main_read_twfe[["coefficients"]][["did"]], main_match_read_twfe[["coefficients"]][["did"]], wa_read_sc,
          main_mort_twfe[["coefficients"]][["did"]],  main_match_mort_twfe[["coefficients"]][["did"]], wa_mort_sc)
se <- c(main_read_twfe[["se"]][["did"]], main_match_read_twfe[["se"]][["did"]], wa_read_sc_se,
        main_mort_twfe[["se"]][["did"]], main_match_mort_twfe[["se"]][["did"]], wa_mort_sc_se)
obs <- c(format(main_read_twfe[["nobs"]],big.mark = ",", scientific = FALSE), format(main_match_read_twfe[["nobs"]],big.mark = ",", scientific = FALSE), format(read_sc_obs,big.mark = ",", scientific = FALSE),
         format(main_mort_twfe[["nobs"]],big.mark = ",", scientific = FALSE), format(main_match_mort_twfe[["nobs"]],big.mark = ",", scientific = FALSE), format(mort_sc_obs,big.mark = ",", scientific = FALSE))

estimates <- as.data.frame(list(coef=coef, se=se)) %>%
  mutate(p=round((2*(1-pnorm(abs(coef/se)))), 4)) %>%
  mutate(coef=round(coef,2),
         se=round(se,2)) %>%
  mutate(stars = case_when(
    p >= 0.05 ~ "",
    p < 0.001 ~ "$^{***}$",
    p < 0.01 ~ "$^{**}$",
    p < 0.05 ~ "$^{*}$")) %>%
  mutate(coef=paste0(coef,stars)) %>%
  mutate(se = paste0("(",se,")")) %>%
  select(-p, -stars)

estimates <- as.data.frame(t(estimates)) %>%
  mutate(x=c("Ever Clinical Exec x Post Program", " ")) %>%
  select(x, V1, V2, V3, V4, V5, V6) %>%
  add_row(x="") %>%
  add_row(x="Hospital FE", V1="$\\checkmark$", V2="$\\checkmark$", V3="$\\checkmark$", V4="$\\checkmark$", V5="$\\checkmark$", V6="$\\checkmark$") %>%
  add_row(x="Year FE", V1="$\\checkmark$", V2="$\\checkmark$", V3="$\\checkmark$", V4="$\\checkmark$", V5="$\\checkmark$", V6="$\\checkmark$") %>%
  add_row(x="Observations", V1=obs[[1]], V2=obs[[2]], V3=obs[[3]], V4=obs[[4]], V5=obs[[5]], V6=obs[[6]])

main_twfe_match_tab <- knitr::kable(estimates, format="latex",
                                         row.names = FALSE,
                                         col.names = c("", "(1)", "(2)", "(3)", "(4)", "(5)", "(6)"),
                                         booktabs = TRUE,
                                         table.envir="table",
                                         caption="\\label{tab:main_twfe_match}Effect of Clinically Trained Executive, Various Estimations",
                                         escape=F,
                                         align=c("l","c","c","c", "c", "c"),
                                         position="ht!") %>%
  add_header_above(c(" ", "TWFE"=1, "Matched"=1, "SC"=1, "TWFE"=1, "Matched"=1, "SC"=1)) %>%
  add_header_above(c(" ", "Readmission Rate"=3, "Mortality Rate"=3), underline=TRUE, line=TRUE) %>%
  add_footnote(c("Standard errors are clustered at the hospital level.",
                 "Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1"), notation="none")
write(main_twfe_match_tab, "Tables/main_twfe_match_tab.tex")

# FIGURE 17: main analysis event study, TWFE and matched specifications ####
main_es_data <- hospital_data %>%
  filter(no_num_md_change_2010_2014==1 & !is.na(ever_has_md)) %>%
  mutate(yr2010=ifelse(year==2010,1,0),
         yr2011=ifelse(year==2011,1,0),
         yr2012=ifelse(year==2012,1,0),
         yr2013=ifelse(year==2013,1,0),
         yr2014=ifelse(year==2014,1,0)) %>%
  mutate(es2010 = yr2010*ever_has_md,
         es2011 = yr2011*ever_has_md,
         es2012 = yr2012*ever_has_md,
         es2013 = yr2013*ever_has_md,
         es2014 = yr2014*ever_has_md)

main_read_es <- fixest::feols(weightedavg_read ~ es2010 + es2012 + es2013 + es2014 | ID + year, cluster = ~ID, data=main_es_data)
main_mort_es <- fixest::feols(weightedavg_mort ~ es2010 + es2012 + es2013 + es2014 | ID + year, cluster = ~ID, data=main_es_data)

main_match_read_es <- fixest::feols(weightedavg_read ~ es2010 + es2012 + es2013 + es2014 | ID + year, cluster = ~ID, data=filter(main_es_data, matched_mainanalysis==1))
main_match_mort_es <- fixest::feols(weightedavg_mort ~ es2010 + es2012 + es2013 + es2014 | ID + year, cluster = ~ID, data=filter(main_es_data, matched_mainanalysis==1))

# graph of estimates for readmission
main_read_es_data <- as.data.frame(list(vars = c("2010", "2012", "2013", "2014"), 
                           TWFE = main_read_es[["coefficients"]], se_TWFE = main_read_es[["se"]],
                           Matching = main_match_read_es[["coefficients"]], se_Match = main_match_read_es[["se"]])) %>%
  add_row(vars = "2011", TWFE = 0, se_TWFE = 0, Matching = 0, se_Match = 0) %>%
  mutate(vars=factor(vars,levels=c("2009", "2010", "2011", "2012", "2013", "2014", "2015"))) %>%
  mutate(category=ifelse(vars=="2009"|vars=="2010"|vars=="2011","Pre","Post")) %>%
  mutate(se_TWFE=as.numeric(se_TWFE),
         se_Match=as.numeric(se_Match))
main_read_es_data <- pivot_longer(main_read_es_data, cols=c(TWFE, Matching), names_to="Specification", values_to = "Estimate")
main_read_es_data <- pivot_longer(main_read_es_data, cols=c(se_TWFE, se_Match), names_to="Specification SE", values_to = "se")
main_read_es_data <- main_read_es_data %>%
  filter(Specification=="TWFE" & `Specification SE`=="se_TWFE" |
           Specification=="Matching" & `Specification SE`=="se_Match") %>%
  select(-`Specification SE`) 
main_read_es_graph <- ggplot(main_read_es_data, aes(x=vars, y=Estimate, group=Specification, color=Specification)) + 
  scale_color_brewer(palette="Set2") +
  geom_hline(yintercept=0, lty=2, lwd=.5, colour="black") +
  geom_errorbar(aes(ymin=Estimate - 1.96*se, ymax=Estimate + 1.96*se, color=Specification), 
                lwd=1, width=.18, size=1, position = position_dodge(width = 0.5)) +
  geom_point(aes(color=Specification), size=2, position = position_dodge(width = 0.5)) +
  geom_line(aes(color=Specification, linetype=Specification), position = position_dodge(width=0.5), linewidth=1) +
  theme_bw() + theme(text=element_text(size=18)) + xlab("") + ggtitle("Readmission") + 
  ylab("Estimate and 95% CI") +ylim(-1,1.5) + labs(color="Specification") + theme(legend.position = "bottom")

# graph of estimates for mortality
main_mort_es_data <- as.data.frame(list(vars = c("2010", "2012", "2013", "2014"), 
                                        TWFE = main_mort_es[["coefficients"]], se_TWFE = main_mort_es[["se"]],
                                        Matching = main_match_mort_es[["coefficients"]], se_Match = main_match_mort_es[["se"]])) %>%
  add_row(vars = "2011", TWFE = 0, se_TWFE = 0, Matching = 0, se_Match = 0) %>%
  mutate(vars=factor(vars,levels=c("2009", "2010", "2011", "2012", "2013", "2014", "2015"))) %>%
  mutate(category=ifelse(vars=="2009"|vars=="2010"|vars=="2011","Pre","Post")) %>%
  mutate(se_TWFE=as.numeric(se_TWFE),
         se_Match=as.numeric(se_Match))
main_mort_es_data <- pivot_longer(main_mort_es_data, cols=c(TWFE, Matching), names_to="Specification", values_to = "Estimate")
main_mort_es_data <- pivot_longer(main_mort_es_data, cols=c(se_TWFE, se_Match), names_to="Specification SE", values_to = "se")
main_mort_es_data <- main_mort_es_data %>%
  filter(Specification=="TWFE" & `Specification SE`=="se_TWFE" |
           Specification=="Matching" & `Specification SE`=="se_Match") %>%
  select(-`Specification SE`) 
main_mort_es_graph <- ggplot(main_mort_es_data, aes(x=vars, y=Estimate, group=Specification, color=Specification)) + 
  scale_color_brewer(palette="Set2") +
  geom_hline(yintercept=0, lty=2, lwd=.5, colour="black") +
  geom_errorbar(aes(ymin=Estimate - 1.96*se, ymax=Estimate + 1.96*se, color=Specification), 
                lwd=1, width=.18, size=1, position = position_dodge(width = 0.5)) +
  geom_point(aes(color=Specification), size=2, position = position_dodge(width = 0.5)) +
  geom_line(aes(color=Specification, linetype=Specification), position = position_dodge(width=0.5), linewidth=1) +
  theme_bw() + theme(text=element_text(size=18)) + xlab("") + ggtitle("Mortality") + 
  ylab("\n") +ylim(-1,1.5) + labs(color="Specification") + theme(legend.position = "bottom")

ggarrange(main_read_es_graph, main_mort_es_graph,
          nrow=1,
          common.legend=T,
          legend="right")
ggsave(plot=last_plot(), file="Objects/main_twfe_match_eventstudy.pdf", width=10, height=5, units="in")






# TABLE 10: compare to for-profit, TWFE, matched and synthetic control (readmission) ####
# create matching variable for for-profit vs. always MD
match_data_fp_always <- hospital_data %>%
  filter(ever_has_md==1 | profit_status=="forprofit") %>%
  filter(year==2010) %>%
  mutate(forprofit = ifelse(profit_status=="forprofit",1,0)) %>%
  select(MCRNUM, beds, forprofit,
         patnum_heartfailure_readmission, patnum_heartattack_readmission, patnum_pneum_readmission,
         ever_penalized, ever_pen_hf, ever_pen_ha, ever_pen_pnem) %>%
  na.omit()
match <- matchit(forprofit ~ beds + ever_pen_hf + ever_pen_ha + ever_pen_pnem + patnum_heartfailure_readmission + patnum_heartattack_readmission + patnum_pneum_readmission,
                 data = match_data_fp_always,
                 method = "cem")

matched_data <- match.data(match) %>%
  select(MCRNUM, weights) %>%
  mutate(matched_fp_always=1)
hospital_data <- hospital_data %>%
  left_join(matched_data, by="MCRNUM") %>%
  mutate(matched_fp_always = ifelse(is.na(matched_fp_always),0,matched_fp_always))

# create matching variable for for-profit vs. never MD
match_data_fp_never <- hospital_data %>%
  filter(ever_has_md==0 | profit_status=="forprofit") %>%
  filter(year==2010) %>%
  mutate(forprofit = ifelse(profit_status=="forprofit",1,0)) %>%
  select(MCRNUM, beds, forprofit,
         patnum_heartfailure_readmission, patnum_heartattack_readmission, patnum_pneum_readmission,
         ever_penalized, ever_pen_hf, ever_pen_ha, ever_pen_pnem) %>%
  na.omit()
match <- matchit(forprofit ~ beds + ever_pen_hf + ever_pen_ha + ever_pen_pnem + patnum_heartfailure_readmission + patnum_heartattack_readmission + patnum_pneum_readmission,
                 data = match_data_fp_never,
                 method = "cem")

matched_data <- match.data(match) %>%
  select(MCRNUM, weights) %>%
  mutate(matched_fp_never=1)
hospital_data <- hospital_data %>%
  left_join(matched_data, by="MCRNUM") %>%
  mutate(matched_fp_never = ifelse(is.na(matched_fp_never),0,matched_fp_never))

fp_analysis_data <- hospital_data %>%
  filter((no_num_md_change_2010_2014==1 & !is.na(ever_has_md)) | profit_status=="forprofit") %>%
  mutate(never_has_md=ifelse(ever_has_md==1,0,1),
         post_2012 = ifelse(year>2012,1,0),
         forprofit = ifelse(profit_status=="forprofit",1,0)) %>%
  mutate(did=post_2012*forprofit) %>%
  mutate(count=1) %>%
  group_by(ID, year) %>%
  mutate(sum=sum(count)) %>%
  ungroup() %>%
  filter(!(sum==2 & is.na(weightedavg_read))) %>%
  select(-count, -sum)

# compare always clinical to FP
fp_wa_read_md_data <- fp_analysis_data %>%
  filter(profit_status=="forprofit" | ever_has_md==1) %>%
  filter(year!=2015) %>%
  select(ID, year, weightedavg_read, did, cmiv, matched_fp_always) %>%
  mutate(did=ifelse(is.na(did),0,did)) %>%
  na.omit() %>%
  mutate(count=1) %>%
  group_by(ID) %>%
  mutate(min=min(year[did==1])) %>%
  mutate(sum=sum(count)) %>%
  ungroup() %>%
  filter(sum==5) %>%
  filter(min!=2014) %>%
  select(-sum, -count, -min) %>%
  mutate(ID=as.factor(ID))

# twfe
fp_read_always_twfe <- fixest::feols(weightedavg_read ~ did | ID + year, cluster = ~ID, data=fp_wa_read_md_data)

# matched
fp_read_always_match_twfe <- fixest::feols(weightedavg_read ~ did | ID + year, cluster = ~ID, data=filter(fp_wa_read_md_data, matched_fp_always==1))

# synthetic control
fp_wa_read_md_panel <- panel.matrices(as.data.frame(fp_wa_read_md_data))
X_mat_fp_wa_read_md <- fp_wa_read_md_data %>%
  select(ID, year, cmiv) %>%
  melt(id.var=c("ID", "year")) %>%
  nest_by(variable) %>%
  mutate(X=list(dcast(data.table(fp_wa_read_md_data), ID~year) %>%
                  .[data.table(ID=rownames(fp_wa_read_md_panel$Y)), on="ID"] %>%
                  .[, ID := NULL] %>%
                  as.matrix()
  )) %>%
  .$X %>%
  abind(along=2)

fp_wa_read_md_sc <- sc_estimate(fp_wa_read_md_panel$Y, fp_wa_read_md_panel$N0, fp_wa_read_md_panel$T0, X=X_mat_fp_wa_read_md)
fp_wa_read_md_sc_se = sqrt(vcov(fp_wa_read_md_sc))
fp_wa_read_md_sc_obs <- (summary(fp_wa_read_md_sc)$dimensions[["N0"]]+summary(fp_wa_read_md_sc)$dimensions[["N1"]])*5

# compare never clinical to FP
fp_wa_read_nomd_data <- fp_analysis_data %>%
  filter(profit_status=="forprofit" | ever_has_md==0) %>%
  filter(year!=2015) %>%
  select(ID, year, weightedavg_read, did, cmiv, matched_fp_never) %>%
  mutate(did=ifelse(is.na(did),0,did)) %>%
  na.omit() %>%
  mutate(count=1) %>%
  group_by(ID) %>%
  mutate(min=min(year[did==1])) %>%
  mutate(sum=sum(count)) %>%
  ungroup() %>%
  filter(sum==5) %>%
  filter(min!=2014) %>%
  select(-sum, -count, -min) %>%
  mutate(ID=as.factor(ID))

# twfe
fp_read_never_twfe <- fixest::feols(weightedavg_read ~ did | ID + year, cluster = ~ID, data=fp_wa_read_nomd_data)

# matched
fp_read_never_match_twfe <- fixest::feols(weightedavg_read ~ did | ID + year, cluster = ~ID, data=filter(fp_wa_read_nomd_data, matched_fp_never==1))

# synthetic control
fp_wa_read_nomd_panel <- panel.matrices(as.data.frame(fp_wa_read_nomd_data))
X_mat_fp_wa_read_nomd <- fp_wa_read_nomd_data %>%
  select(ID, year, cmiv) %>%
  melt(id.var=c("ID", "year")) %>%
  nest_by(variable) %>%
  mutate(X=list(dcast(data.table(fp_wa_read_nomd_data), ID~year) %>%
                  .[data.table(ID=rownames(fp_wa_read_nomd_panel$Y)), on="ID"] %>%
                  .[, ID := NULL] %>%
                  as.matrix()
  )) %>%
  .$X %>%
  abind(along=2)

fp_wa_read_nomd_sc <- sc_estimate(fp_wa_read_nomd_panel$Y, fp_wa_read_nomd_panel$N0, fp_wa_read_nomd_panel$T0, X=X_mat_fp_wa_read_nomd)
fp_wa_read_nomd_sc_se = sqrt(vcov(fp_wa_read_nomd_sc))
fp_wa_read_nomd_sc_obs <- (summary(fp_wa_read_nomd_sc)$dimensions[["N0"]]+summary(fp_wa_read_nomd_sc)$dimensions[["N1"]])*5

# create objects for manual table 
coef <- c(fp_read_always_twfe[["coefficients"]][["did"]], fp_read_always_match_twfe[["coefficients"]][["did"]], fp_wa_read_md_sc,
          fp_read_never_twfe[["coefficients"]][["did"]], fp_read_never_match_twfe[["coefficients"]][["did"]], fp_wa_read_nomd_sc)
se <- c(fp_read_always_twfe[["se"]][["did"]], fp_read_always_match_twfe[["se"]][["did"]], fp_wa_read_md_sc_se,
        fp_read_never_twfe[["se"]][["did"]], fp_read_never_match_twfe[["se"]][["did"]], fp_wa_read_nomd_sc_se)
obs <- c(format(fp_read_always_twfe[["nobs"]],big.mark = ",", scientific = FALSE), format(fp_read_always_match_twfe[["nobs"]],big.mark = ",", scientific = FALSE), format(fp_wa_read_md_sc_obs,big.mark = ",", scientific = FALSE),
         format(fp_read_never_twfe[["nobs"]],big.mark = ",", scientific = FALSE), format(fp_read_never_match_twfe[["nobs"]],big.mark = ",", scientific = FALSE), format(fp_wa_read_nomd_sc_obs,big.mark = ",", scientific = FALSE))

estimates <- as.data.frame(list(coef=coef, se=se)) %>%
  mutate(p=round((2*(1-pnorm(abs(coef/se)))), 4)) %>%
  mutate(coef=round(coef,2),
         se=round(se,2)) %>%
  mutate(stars = case_when(
    p >= 0.05 ~ "",
    p < 0.001 ~ "$^{***}$",
    p < 0.01 ~ "$^{**}$",
    p < 0.05 ~ "$^{*}$")) %>%
  mutate(coef=paste0(coef,stars)) %>%
  mutate(se = paste0("(",se,")")) %>%
  select(-p, -stars)

estimates <- as.data.frame(t(estimates)) %>%
  mutate(x=c("For-Profit x Post Program", " ")) %>%
  select(x, V1, V2, V3, V4, V5, V6) %>%
  add_row(x="") %>%
  add_row(x="Hospital FE", V1="$\\checkmark$", V2="$\\checkmark$", V3="$\\checkmark$", V4="$\\checkmark$", V5="$\\checkmark$", V6="$\\checkmark$") %>%
  add_row(x="Year FE", V1="$\\checkmark$", V2="$\\checkmark$", V3="$\\checkmark$", V4="$\\checkmark$", V5="$\\checkmark$", V6="$\\checkmark$") %>%
  add_row(x="Observations", V1=obs[[1]], V2=obs[[2]], V3=obs[[3]], V4=obs[[4]], V5=obs[[5]], V6=obs[[6]])

fp_always_estimators_tab <- knitr::kable(estimates, format="latex",
                                    row.names = FALSE,
                                    col.names = c("", "(1)", "(2)", "(3)", "(4)", "(5)", "(6)"),
                                    booktabs = TRUE,
                                    table.envir="table",
                                    caption="\\label{tab:forprofit_estimators}Compare For-Profit to Types of Leadership (Readmission Rates), Various Estimations",
                                    escape=F,
                                    align=c("l","c","c","c", "c", "c"),
                                    position="ht!") %>%
  add_header_above(c(" ", "TWFE"=1, "Matched"=1, "SC"=1, "TWFE"=1, "Matched"=1, "SC"=1)) %>%
  add_header_above(c(" ", "Compare to Always Clinical Exec"=3, "Compare to Never Clinical Exec"=3), underline=FALSE, line=TRUE) %>%
  add_footnote(c("Standard errors are clustered at the hospital level.",
                 "Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1"), notation="none")
write(fp_always_estimators_tab, "Tables/fp_always_estimators_read_tab.tex")









# TABLE 11: compare to for-profit, TWFE, Matched, and sc (mortality only) ####
# compare always clinical to FP
fp_wa_mort_md_data <- fp_analysis_data %>%
  filter(profit_status=="forprofit" | ever_has_md==1) %>%
  filter(year!=2015) %>%
  select(ID, year, weightedavg_mort, did, cmiv, matched_fp_always) %>%
  mutate(did=ifelse(is.na(did),0,did)) %>%
  na.omit() %>%
  mutate(count=1) %>%
  group_by(ID) %>%
  mutate(min=min(year[did==1])) %>%
  mutate(sum=sum(count)) %>%
  ungroup() %>%
  filter(sum==5) %>%
  filter(min!=2014) %>%
  select(-sum, -count, -min) %>%
  mutate(ID=as.factor(ID))

# twfe
fp_mort_always_twfe <- fixest::feols(weightedavg_mort ~ did | ID + year, cluster = ~ID, data=fp_wa_mort_md_data)

# matched
fp_mort_always_match_twfe <- fixest::feols(weightedavg_mort ~ did | ID + year, cluster = ~ID, data=filter(fp_wa_mort_md_data, matched_fp_always==1))

# synthetic control
fp_wa_mort_md_panel <- panel.matrices(as.data.frame(fp_wa_mort_md_data))
X_mat_fp_wa_mort_md <- fp_wa_mort_md_data %>%
  select(ID, year, cmiv) %>%
  melt(id.var=c("ID", "year")) %>%
  nest_by(variable) %>%
  mutate(X=list(dcast(data.table(fp_wa_mort_md_data), ID~year) %>%
                  .[data.table(ID=rownames(fp_wa_mort_md_panel$Y)), on="ID"] %>%
                  .[, ID := NULL] %>%
                  as.matrix()
  )) %>%
  .$X %>%
  abind(along=2)

fp_wa_mort_md_sc <- sc_estimate(fp_wa_mort_md_panel$Y, fp_wa_mort_md_panel$N0, fp_wa_mort_md_panel$T0, X=X_mat_fp_wa_mort_md)
fp_wa_mort_md_sc_se = sqrt(vcov(fp_wa_mort_md_sc))
fp_wa_mort_md_sc_obs <- (summary(fp_wa_mort_md_sc)$dimensions[["N0"]]+summary(fp_wa_mort_md_sc)$dimensions[["N1"]])*5

# compare never clinical to FP
fp_wa_mort_nomd_data <- fp_analysis_data %>%
  filter(profit_status=="forprofit" | ever_has_md==0) %>%
  filter(year!=2015) %>%
  select(ID, year, weightedavg_mort, did, cmiv, matched_fp_never) %>%
  mutate(did=ifelse(is.na(did),0,did)) %>%
  na.omit() %>%
  mutate(count=1) %>%
  group_by(ID) %>%
  mutate(min=min(year[did==1])) %>%
  mutate(sum=sum(count)) %>%
  ungroup() %>%
  filter(sum==5) %>%
  filter(min!=2014) %>%
  select(-sum, -count, -min) %>%
  mutate(ID=as.factor(ID))

# twfe
fp_mort_never_twfe <- fixest::feols(weightedavg_mort ~ did | ID + year, cluster = ~ID, data=fp_wa_mort_nomd_data)

# matched
fp_mort_never_match_twfe <- fixest::feols(weightedavg_mort ~ did | ID + year, cluster = ~ID, data=filter(fp_wa_mort_nomd_data, matched_fp_never==1))

# synthetic control
fp_wa_mort_nomd_panel <- panel.matrices(as.data.frame(fp_wa_mort_nomd_data))
X_mat_fp_wa_mort_nomd <- fp_wa_mort_nomd_data %>%
  select(ID, year, cmiv) %>%
  melt(id.var=c("ID", "year")) %>%
  nest_by(variable) %>%
  mutate(X=list(dcast(data.table(fp_wa_mort_nomd_data), ID~year) %>%
                  .[data.table(ID=rownames(fp_wa_mort_nomd_panel$Y)), on="ID"] %>%
                  .[, ID := NULL] %>%
                  as.matrix()
  )) %>%
  .$X %>%
  abind(along=2)

fp_wa_mort_nomd_sc <- sc_estimate(fp_wa_mort_nomd_panel$Y, fp_wa_mort_nomd_panel$N0, fp_wa_mort_nomd_panel$T0, X=X_mat_fp_wa_mort_nomd)
fp_wa_mort_nomd_sc_se = sqrt(vcov(fp_wa_mort_nomd_sc))
fp_wa_mort_nomd_sc_obs <- (summary(fp_wa_mort_nomd_sc)$dimensions[["N0"]]+summary(fp_wa_mort_nomd_sc)$dimensions[["N1"]])*5

# create objects for manual table 
coef <- c(fp_mort_always_twfe[["coefficients"]][["did"]], fp_mort_always_match_twfe[["coefficients"]][["did"]], fp_wa_mort_md_sc,
          fp_mort_never_twfe[["coefficients"]][["did"]], fp_mort_never_match_twfe[["coefficients"]][["did"]], fp_wa_mort_nomd_sc)
se <- c(fp_mort_always_twfe[["se"]][["did"]], fp_mort_always_match_twfe[["se"]][["did"]], fp_wa_mort_md_sc_se,
        fp_mort_never_twfe[["se"]][["did"]], fp_mort_never_match_twfe[["se"]][["did"]], fp_wa_mort_nomd_sc_se)
obs <- c(format(fp_mort_always_twfe[["nobs"]],big.mark = ",", scientific = FALSE), format(fp_mort_always_match_twfe[["nobs"]],big.mark = ",", scientific = FALSE), format(fp_wa_mort_md_sc_obs,big.mark = ",", scientific = FALSE),
         format(fp_mort_never_twfe[["nobs"]],big.mark = ",", scientific = FALSE), format(fp_mort_never_match_twfe[["nobs"]],big.mark = ",", scientific = FALSE), format(fp_wa_mort_nomd_sc_obs,big.mark = ",", scientific = FALSE))

estimates <- as.data.frame(list(coef=coef, se=se)) %>%
  mutate(p=round((2*(1-pnorm(abs(coef/se)))), 4)) %>%
  mutate(coef=round(coef,2),
         se=round(se,2)) %>%
  mutate(stars = case_when(
    p >= 0.05 ~ "",
    p < 0.001 ~ "$^{***}$",
    p < 0.01 ~ "$^{**}$",
    p < 0.05 ~ "$^{*}$")) %>%
  mutate(coef=paste0(coef,stars)) %>%
  mutate(se = paste0("(",se,")")) %>%
  select(-p, -stars)

estimates <- as.data.frame(t(estimates)) %>%
  mutate(x=c("For-Profit x Post Program", " ")) %>%
  select(x, V1, V2, V3, V4, V5, V6) %>%
  add_row(x="") %>%
  add_row(x="Hospital FE", V1="$\\checkmark$", V2="$\\checkmark$", V3="$\\checkmark$", V4="$\\checkmark$", V5="$\\checkmark$", V6="$\\checkmark$") %>%
  add_row(x="Year FE", V1="$\\checkmark$", V2="$\\checkmark$", V3="$\\checkmark$", V4="$\\checkmark$", V5="$\\checkmark$", V6="$\\checkmark$") %>%
  add_row(x="Observations", V1=obs[[1]], V2=obs[[2]], V3=obs[[3]], V4=obs[[4]], V5=obs[[5]], V6=obs[[6]])

fp_always_estimators_tab <- knitr::kable(estimates, format="latex",
                                         row.names = FALSE,
                                         col.names = c("", "(1)", "(2)", "(3)", "(4)", "(5)", "(6)"),
                                         booktabs = TRUE,
                                         table.envir="table",
                                         caption="\\label{tab:forprofit_mort_estimators}Compare For-Profit to Types of Leadership (Mortality Rates), Various Estimations",
                                         escape=F,
                                         align=c("l","c","c","c", "c", "c"),
                                         position="ht!") %>%
  add_header_above(c(" ", "TWFE"=1, "Matched"=1, "SC"=1, "TWFE"=1, "Matched"=1, "SC"=1)) %>%
  add_header_above(c(" ", "Compare to Always Clinical Exec"=3, "Compare to Never Clinical Exec"=3), underline=FALSE, line=TRUE) %>%
  add_footnote(c("Standard errors are clustered at the hospital level.",
                 "Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1"), notation="none")
write(fp_always_estimators_tab, "Tables/fp_always_estimators_mort_tab.tex")

# TABLE 12: decomposition with various estimators (readmission) ####
# estimates for signaling
ever_data <- hospital_data %>%
  filter(!is.na(no_num_md_change_2010_2014)) %>%
  mutate(post_2012 = ifelse(year>2012,1,0)) %>%
  mutate(did = post_2012*ever_has_md) %>%
  mutate(count=1) %>%
  group_by(ID, year) %>%
  mutate(sum=sum(count)) %>%
  ungroup() %>%
  filter(!(sum==2 & is.na(weightedavg_read))) %>%
  select(-count, -sum)

ever_read_data <- ever_data %>%
  filter(year!=2015) %>%
  select(ID, year, weightedavg_read, did, cmiv, matched_mainanalysis) %>%
  na.omit() %>%
  mutate(count=1) %>%
  group_by(ID) %>%
  mutate(min=min(year[did==1])) %>%
  mutate(sum=sum(count)) %>%
  ungroup() %>%
  filter(sum==5) %>%
  filter(min!=2014) %>%
  select(-sum, -count, -min) %>%
  mutate(ID=as.factor(ID))

# twfe 
signal_twfe <- fixest::feols(weightedavg_read ~ did | ID + year, cluster = ~ID, data=ever_read_data)

# matching 
signal_match_twfe <- fixest::feols(weightedavg_read ~ did | ID + year, cluster = ~ID, data=filter(ever_read_data, matched_mainanalysis==1))

# synthetic control
ever_read_panel <- panel.matrices(as.data.frame(ever_read_data))
X_mat_ever_read <- ever_read_data %>%
  select(ID, year, cmiv) %>%
  melt(id.var=c("ID", "year")) %>%
  nest_by(variable) %>%
  mutate(X=list(dcast(data.table(ever_read_data), ID~year) %>%
                  .[data.table(ID=rownames(ever_read_panel$Y)), on="ID"] %>%
                  .[, ID := NULL] %>%
                  as.matrix()
  )) %>%
  .$X %>%
  abind(along=2)

signal_sc <- sc_estimate(ever_read_panel$Y, ever_read_panel$N0, ever_read_panel$T0, X=X_mat_ever_read)
signal_sc_se = sqrt(vcov(signal_sc))
signal_sc_obs <- (summary(signal_sc)$dimensions[["N0"]]+summary(signal_sc)$dimensions[["N1"]])*5


# managing
data_2012 <- hospital_data %>%
  filter(ever_has_md==1) %>%
  mutate(has_md_2012 = ifelse(year==2012 & has_any_md==1,1,NA)) %>%
  group_by(ID) %>%
  fill(has_md_2012, .direction="downup") %>%
  ungroup() %>%
  mutate(has_md_2012 = ifelse(is.na(has_md_2012),0,has_md_2012)) %>%
  mutate(post_2012 = ifelse(year>2012,1,0)) %>%
  mutate(did = post_2012*has_md_2012) %>%
  mutate(count=1) %>%
  group_by(ID, year) %>%
  mutate(sum=sum(count)) %>%
  ungroup() %>%
  filter(!(sum==2 & is.na(weightedavg_read))) %>%
  select(-count, -sum)

read_2012_data <- data_2012 %>%
  filter(year!=2015) %>%
  select(ID, year, weightedavg_read, did, cmiv, matched_mainanalysis) %>%
  na.omit() %>%
  mutate(count=1) %>%
  group_by(ID) %>%
  mutate(min=min(year[did==1])) %>%
  mutate(sum=sum(count)) %>%
  ungroup() %>%
  filter(sum==5) %>%
  filter(min!=2014) %>%
  select(-sum, -count, -min) %>%
  mutate(ID=as.factor(ID))

# twfe
manage_twfe <- fixest::feols(weightedavg_read ~ did | ID + year, cluster = ~ID, data=read_2012_data)

# matched 
manage_match_twfe <- fixest::feols(weightedavg_read ~ did | ID + year, cluster = ~ID, data=filter(read_2012_data, matched_mainanalysis==1))

# synthetic control
read_2012_panel <- panel.matrices(as.data.frame(read_2012_data))
X_mat_2012_read <- read_2012_data %>%
  select(ID, year, cmiv) %>%
  melt(id.var=c("ID", "year")) %>%
  nest_by(variable) %>%
  mutate(X=list(dcast(data.table(read_2012_data), ID~year) %>%
                  .[data.table(ID=rownames(read_2012_panel$Y)), on="ID"] %>%
                  .[, ID := NULL] %>%
                  as.matrix()
  )) %>%
  .$X %>%
  abind(along=2)

manage_sc <- sc_estimate(read_2012_panel$Y, read_2012_panel$N0, read_2012_panel$T0, X=X_mat_2012_read)
manage_sc_se = sqrt(vcov(manage_sc))
manage_sc_obs <- (summary(manage_sc)$dimensions[["N0"]]+summary(signal_sc)$dimensions[["N1"]])*5

# create objects for manual table 
coef <- c(signal_twfe[["coefficients"]][["did"]], signal_match_twfe[["coefficients"]][["did"]], signal_sc,
          manage_twfe[["coefficients"]][["did"]], manage_match_twfe[["coefficients"]][["did"]], manage_sc)
se <- c(signal_twfe[["se"]][["did"]], signal_match_twfe[["se"]][["did"]], signal_sc_se,
        manage_twfe[["se"]][["did"]], manage_match_twfe[["se"]][["did"]], manage_sc_se)
obs <- c(format(signal_twfe[["nobs"]],big.mark = ",", scientific = FALSE), format(signal_match_twfe[["nobs"]],big.mark = ",", scientific = FALSE), format(signal_sc_obs,big.mark = ",", scientific = FALSE),
         format(manage_twfe[["nobs"]],big.mark = ",", scientific = FALSE), format(manage_match_twfe[["nobs"]],big.mark = ",", scientific = FALSE), format(manage_sc_obs,big.mark = ",", scientific = FALSE))

estimates <- as.data.frame(list(coef=coef, se=se)) %>%
  mutate(p=round((2*(1-pnorm(abs(coef/se)))), 4)) %>%
  mutate(coef=round(coef,2),
         se=round(se,2)) %>%
  mutate(stars = case_when(
    p >= 0.05 ~ "",
    p < 0.001 ~ "$^{***}$",
    p < 0.01 ~ "$^{**}$",
    p < 0.05 ~ "$^{*}$")) %>%
  mutate(coef=paste0(coef,stars)) %>%
  mutate(se = paste0("(",se,")")) %>%
  select(-p, -stars)

estimates <- as.data.frame(t(estimates)) %>%
  mutate(x=c("Estimate", " ")) %>%
  select(x, V1, V2, V3, V4, V5, V6) %>%
  add_row(x="") %>%
  add_row(x="Hospital FE", V1="$\\checkmark$", V2="$\\checkmark$", V3="$\\checkmark$", V4="$\\checkmark$", V5="$\\checkmark$", V6="$\\checkmark$") %>%
  add_row(x="Year FE", V1="$\\checkmark$", V2="$\\checkmark$", V3="$\\checkmark$", V4="$\\checkmark$", V5="$\\checkmark$", V6="$\\checkmark$") %>%
  add_row(x="Observations", V1=obs[[1]], V2=obs[[2]], V3=obs[[3]], V4=obs[[4]], V5=obs[[5]], V6=obs[[6]])

signal_manage_estimators_tab <- knitr::kable(estimates, format="latex",
                                         row.names = FALSE,
                                         col.names = c("", "(1)", "(2)", "(3)", "(4)", "(5)", "(6)"),
                                         booktabs = TRUE,
                                         table.envir="table",
                                         caption="\\label{tab:signal_manage_read_estimators}Signaling vs. Managing Decomp. (Readmission Rates), Various Estimations",
                                         escape=F,
                                         align=c("l","c","c","c", "c", "c"),
                                         position="ht!") %>%
  add_header_above(c(" ", "TWFE"=1, "Matched"=1, "SC"=1, "TWFE"=1, "Matched"=1, "SC"=1)) %>%
  add_header_above(c(" ", "Signaling Effect"=3, "Managing Effect"=3), underline=FALSE, line=TRUE) %>%
  add_footnote(c("Standard errors are clustered at the hospital level.",
                 "Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1"), notation="none")
write(signal_manage_estimators_tab, "Tables/signal_manage_estimators_read_tab.tex")






# TABLE 13: decomposition with various estimators (mortality) ####
ever_mort_data <- ever_data %>%
  filter(year!=2015) %>%
  select(ID, year, weightedavg_mort, did, cmiv, matched_mainanalysis) %>%
  na.omit() %>%
  mutate(count=1) %>%
  group_by(ID) %>%
  mutate(min=min(year[did==1])) %>%
  mutate(sum=sum(count)) %>%
  ungroup() %>%
  filter(sum==5) %>%
  filter(min!=2014) %>%
  select(-sum, -count, -min) %>%
  mutate(ID=as.factor(ID))

# twfe 
signal_twfe <- fixest::feols(weightedavg_mort ~ did | ID + year, cluster = ~ID, data=ever_mort_data)

# matching 
signal_match_twfe <- fixest::feols(weightedavg_mort ~ did | ID + year, cluster = ~ID, data=filter(ever_mort_data, matched_mainanalysis==1))

# synthetic control
ever_mort_panel <- panel.matrices(as.data.frame(ever_mort_data))
X_mat_ever_mort <- ever_mort_data %>%
  select(ID, year, cmiv) %>%
  melt(id.var=c("ID", "year")) %>%
  nest_by(variable) %>%
  mutate(X=list(dcast(data.table(ever_mort_data), ID~year) %>%
                  .[data.table(ID=rownames(ever_mort_panel$Y)), on="ID"] %>%
                  .[, ID := NULL] %>%
                  as.matrix()
  )) %>%
  .$X %>%
  abind(along=2)

signal_sc <- sc_estimate(ever_mort_panel$Y, ever_mort_panel$N0, ever_mort_panel$T0, X=X_mat_ever_mort)
signal_sc_se = sqrt(vcov(signal_sc))
signal_sc_obs <- (summary(signal_sc)$dimensions[["N0"]]+summary(signal_sc)$dimensions[["N1"]])*5


# managing
data_2012 <- hospital_data %>%
  filter(ever_has_md==1) %>%
  mutate(has_md_2012 = ifelse(year==2012 & has_any_md==1,1,NA)) %>%
  group_by(ID) %>%
  fill(has_md_2012, .direction="downup") %>%
  ungroup() %>%
  mutate(has_md_2012 = ifelse(is.na(has_md_2012),0,has_md_2012)) %>%
  mutate(post_2012 = ifelse(year>2012,1,0)) %>%
  mutate(did = post_2012*has_md_2012) %>%
  mutate(count=1) %>%
  group_by(ID, year) %>%
  mutate(sum=sum(count)) %>%
  ungroup() %>%
  filter(!(sum==2 & is.na(weightedavg_mort))) %>%
  select(-count, -sum)

mort_2012_data <- data_2012 %>%
  filter(year!=2015) %>%
  select(ID, year, weightedavg_mort, did, cmiv, matched_mainanalysis) %>%
  na.omit() %>%
  mutate(count=1) %>%
  group_by(ID) %>%
  mutate(min=min(year[did==1])) %>%
  mutate(sum=sum(count)) %>%
  ungroup() %>%
  filter(sum==5) %>%
  filter(min!=2014) %>%
  select(-sum, -count, -min) %>%
  mutate(ID=as.factor(ID))

# twfe
manage_twfe <- fixest::feols(weightedavg_mort ~ did | ID + year, cluster = ~ID, data=mort_2012_data)

# matched 
manage_match_twfe <- fixest::feols(weightedavg_mort ~ did | ID + year, cluster = ~ID, data=filter(mort_2012_data, matched_mainanalysis==1))

# synthetic control
mort_2012_panel <- panel.matrices(as.data.frame(mort_2012_data))
X_mat_2012_mort <- mort_2012_data %>%
  select(ID, year, cmiv) %>%
  melt(id.var=c("ID", "year")) %>%
  nest_by(variable) %>%
  mutate(X=list(dcast(data.table(mort_2012_data), ID~year) %>%
                  .[data.table(ID=rownames(mort_2012_panel$Y)), on="ID"] %>%
                  .[, ID := NULL] %>%
                  as.matrix()
  )) %>%
  .$X %>%
  abind(along=2)

manage_sc <- sc_estimate(mort_2012_panel$Y, mort_2012_panel$N0, mort_2012_panel$T0, X=X_mat_2012_mort)
manage_sc_se = sqrt(vcov(manage_sc))
manage_sc_obs <- (summary(manage_sc)$dimensions[["N0"]]+summary(signal_sc)$dimensions[["N1"]])*5

# create objects for manual table 
coef <- c(signal_twfe[["coefficients"]][["did"]], signal_match_twfe[["coefficients"]][["did"]], signal_sc,
          manage_twfe[["coefficients"]][["did"]], manage_match_twfe[["coefficients"]][["did"]], manage_sc)
se <- c(signal_twfe[["se"]][["did"]], signal_match_twfe[["se"]][["did"]], signal_sc_se,
        manage_twfe[["se"]][["did"]], manage_match_twfe[["se"]][["did"]], manage_sc_se)
obs <- c(format(signal_twfe[["nobs"]],big.mark = ",", scientific = FALSE), format(signal_match_twfe[["nobs"]],big.mark = ",", scientific = FALSE), format(signal_sc_obs,big.mark = ",", scientific = FALSE),
         format(manage_twfe[["nobs"]],big.mark = ",", scientific = FALSE), format(manage_match_twfe[["nobs"]],big.mark = ",", scientific = FALSE), format(manage_sc_obs,big.mark = ",", scientific = FALSE))

estimates <- as.data.frame(list(coef=coef, se=se)) %>%
  mutate(p=round((2*(1-pnorm(abs(coef/se)))), 4)) %>%
  mutate(coef=round(coef,2),
         se=round(se,2)) %>%
  mutate(stars = case_when(
    p >= 0.05 ~ "",
    p < 0.001 ~ "$^{***}$",
    p < 0.01 ~ "$^{**}$",
    p < 0.05 ~ "$^{*}$")) %>%
  mutate(coef=paste0(coef,stars)) %>%
  mutate(se = paste0("(",se,")")) %>%
  select(-p, -stars)

estimates <- as.data.frame(t(estimates)) %>%
  mutate(x=c("Estimate", " ")) %>%
  select(x, V1, V2, V3, V4, V5, V6) %>%
  add_row(x="") %>%
  add_row(x="Hospital FE", V1="$\\checkmark$", V2="$\\checkmark$", V3="$\\checkmark$", V4="$\\checkmark$", V5="$\\checkmark$", V6="$\\checkmark$") %>%
  add_row(x="Year FE", V1="$\\checkmark$", V2="$\\checkmark$", V3="$\\checkmark$", V4="$\\checkmark$", V5="$\\checkmark$", V6="$\\checkmark$") %>%
  add_row(x="Observations", V1=obs[[1]], V2=obs[[2]], V3=obs[[3]], V4=obs[[4]], V5=obs[[5]], V6=obs[[6]])

signal_manage_estimators_tab <- knitr::kable(estimates, format="latex",
                                             row.names = FALSE,
                                             col.names = c("", "(1)", "(2)", "(3)", "(4)", "(5)", "(6)"),
                                             booktabs = TRUE,
                                             table.envir="table",
                                             caption="\\label{tab:signal_manage_mort_estimators}Signaling vs. Managing Decomp. (mortmission Rates), Various Estimations",
                                             escape=F,
                                             align=c("l","c","c","c", "c", "c"),
                                             position="ht!") %>%
  add_header_above(c(" ", "TWFE"=1, "Matched"=1, "SC"=1, "TWFE"=1, "Matched"=1, "SC"=1)) %>%
  add_header_above(c(" ", "Signaling Effect"=3, "Managing Effect"=3), underline=FALSE, line=TRUE) %>%
  add_footnote(c("Standard errors are clustered at the hospital level.",
                 "Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1"), notation="none")
write(signal_manage_estimators_tab, "Tables/signal_manage_estimators_mort_tab.tex")
