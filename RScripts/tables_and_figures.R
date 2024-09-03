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

options(knitr.kable.NA=" ")
group.colors <- c("For-profit" = "#D65828", "Never Clinical Exec" = "#C2DEF2", "Ever Clinical Exec" ="#6BAED6", "Always Clinical Exec" = "#2C6B8E")
 
# Produce all tables and figures for the body of the paper #########

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
  mutate(frac_doc_exec = num_doctors/num_execs)


# TABLE 1: summary statistics of individual executives who are clinically trained ####
doc_exec_data <- doc_exec_data %>%
  mutate(surgery_doc = ifelse(t_class == "Surgery", 1, 0),
         anest_doc = ifelse(t_class == "Anesthesiology", 1, 0),
         family_doc = ifelse(t_class == "Family Medicine",1,0),
         emergency_doc = ifelse(t_class == "Emergency Medicine",1,0)) %>%
  mutate(other_doc = ifelse(int_med_doc==0 & surgery_doc==0 & anest_doc==0 & family_doc==0 & emergency_doc==0, 1, 0)) %>%
  mutate(female = ifelse(sex=="F",1,0)) %>%
  mutate(ever_ceo = ifelse(is.na(ever_ceo),0,ever_ceo),
         ever_cmo = ifelse(is.na(ever_cmo),0,ever_cmo))

doc_exec_stats <- doc_exec_data %>%
  summarise_at(c("Age"="min_age",
                 "Internal Medicine" = "int_med_doc",
                 "Surgery" = "surgery_doc",
                 "Anesthesiologist" = "anest_doc",
                 "Other" = "other_doc",
                 "Family Medicine" = "family_doc",
                 "Emergency Medicine" = "emergency_doc",
                 "Female" = "female",
                 "CEO" = "ever_ceo",
                 "CMO" = "ever_cmo"),
               list(mean), na.rm=T) %>%
  mutate_if(is.numeric, ~ifelse(abs(.)==Inf,NA,.))  %>%
  gather(key=variable,value=m) 

n <- doc_exec_data %>%
  distinct(npi) %>%
  nrow()

doc_exec_stats <- doc_exec_stats %>%
  add_row(variable="N", m=n)

doc_exec_stats_tab <- knitr::kable(doc_exec_stats[c(1,8,9,10,2,3,4,6,7,5,11),],
                          row.names = FALSE,
                          format="latex",
                          table.envir="table",
                          col.names=c("Variable","Mean"),
                          digits=2,
                          caption="\\label{doc_sumstats}Clinical Executive Summary Statistics",
                          booktabs=TRUE,
                          escape=F,
                          align=c("l","c"),
                          position="ht!") %>%
  kable_styling(full_width=F) %>%
  pack_rows(index = c(" " = 4, 
                      "Specialty" = 6,
                      " " = 1))
write(doc_exec_stats_tab, file="Tables/doc_exec_stats_tab.tex")


# TABLE 2: hospital level summary stats split by ever clinical, always clinical, never clinical ####
# broken down by ever MD, always MD, never MD
# includes executive team summary stats
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

stats <- ever_md_stats %>%
  left_join(always_md_stats, by="variable") %>%
  left_join(never_md_stats, by="variable")

subsample_stats_tab <- knitr::kable(stats[c(1,7,17,18,8,3,4,5,6,2,11,14,16,12,13,15,10,19),],
                                           row.names = FALSE,
                                           format="latex",
                                           table.envir="table",
                                           col.names=c("Variable","Ever Clinical Exec", "Always Clinical Exec", "Never Clinical Exec"),
                                           digits=2,
                                           caption="\\label{tab:sumstats_samples_stable}Summary Statistics by Leadership Team Type",
                                           booktabs=TRUE,
                                           escape=F,
                                           align=c("l","c","c","c","c","c"),
                                           position="ht!") %>%
  kable_styling(full_width=F) %>%
  pack_rows(index = c("Hospital Characteristics" = 4, "Executive Team Characteristics" = 5, "Penalty/Payment Variables" = 8,
                      " " = 1))
write(subsample_stats_tab, file="Tables/stable_sample_sumstats.tex")

# FIGURE 1: graph outcome variables over time ####
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

outcomes <- rbind(ever_md_outcome_stats, always_md_outcome_stats, never_md_outcome_stats)

read <- ggplot(outcomes, aes(x=year, y=`Weighted Avg. Readmission Rate_m`, color=sample, linetype=sample, shape=sample)) + geom_point(size=3) + geom_line() +
  geom_vline(xintercept = 2012, linetype = "dotted") + xlim(2010,2014) +
  geom_line(linewidth=1.2) + theme_bw() + xlab("") + ylab("WA Readmission\n") + theme(legend.title = element_blank()) +
  theme(text=element_text(size=18)) + scale_color_manual(values = c("For-profit" = "#D65828", "Never Clinical Exec" = "#AED1EC", "Ever Clinical Exec" ="#6BAED6", "Always Clinical Exec" = "#2C6B8E")) 

mort <- ggplot(outcomes, aes(x=year, y=`Weighted Avg. Mortality Rate_m`, color=sample, linetype=sample, shape=sample)) + geom_point(size=3) + geom_line() +
  geom_vline(xintercept = 2012, linetype = "dotted") + xlim(2010,2014) +
  geom_line(linewidth=1.2) + theme_bw() + xlab("") + ylab("WA Mortality\n") + theme(legend.title = element_blank()) +
  theme(text=element_text(size=18)) + scale_color_manual(values = c("For-profit" = "#D65828", "Never Clinical Exec" = "#AED1EC", "Ever Clinical Exec" ="#6BAED6", "Always Clinical Exec" = "#2C6B8E"))  + ylim(11.75,12.75)
cmi <- ggplot(outcomes, aes(x=year, y=`Case Mix Index_m`, color=sample, linetype=sample, shape=sample)) + geom_point(size=3) + geom_line() +
  geom_vline(xintercept = 2012, linetype = "dotted") + xlim(2010,2014) +
  geom_line(linewidth=1.2) + theme_bw() + xlab("") + ylab("Case Mix Index\n") + theme(legend.title = element_blank()) +
  theme(text=element_text(size=18)) + scale_color_manual(values = c("For-profit" = "#D65828", "Never Clinical Exec" = "#AED1EC", "Ever Clinical Exec" ="#6BAED6", "Always Clinical Exec" = "#2C6B8E"))  + ylim(1,2)

ggarrange(read, mort, cmi,
          nrow = 3,
          ncol = 1,
          common.legend = T,
          legend = "right")

ggsave(paste0(objects_path, "outcomes_graph.pdf"), height=9, width=7, units="in")

# FIGURE 2: main result, always clinical vs. never clinical graphs for readmission and mortality ####
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

# Readmission
wa_read_data <- main_analysis_data %>%
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
wa_read_panel <- panel.matrices(as.data.frame(wa_read_data))
X_mat_wa_read <- wa_read_data %>%
  select(ID, year, cmiv) %>%
  melt(id.var=c("ID", "year")) %>%
  nest_by(variable) %>%
  mutate(X=list(dcast(data.table(wa_read_data), ID~year) %>%
                  .[data.table(ID=rownames(wa_read_panel$Y)), on="ID"] %>%
                  .[, ID := NULL] %>%
                  as.matrix()
  )) %>%
  .$X %>%
  abind(along=2)

wa_read_did <- synthdid_estimate(wa_read_panel$Y, wa_read_panel$N0, wa_read_panel$T0, X=X_mat_wa_read)

sprintf('point estimate: %1.2f', test)
wa_read_se = sqrt(vcov(wa_read_did))

main_read_plot <- synthdid_plot(wa_read_did, facet.vertical=FALSE,
                        control.name='Never Clinical Exec', treated.name='Always Clinical Exec',
                        lambda.comparable=TRUE, se.method = 'none',
                        lambda.plot.scale = 0,
                        trajectory.linetype = 1, line.width=.8, effect.curvature=-.4,
                        trajectory.alpha=1, effect.alpha=0,
                        diagram.alpha=0, onset.alpha=1) +
  theme(legend.position=c(.5,-.095), legend.direction='horizontal',
        legend.key=element_blank(), legend.background=element_blank(),
        strip.background=element_blank(), strip.text.x = element_blank(),
        text = element_text(size=18)) + ylim(19.5,22.5) +
  scale_color_manual(values = c("For-profit" = "#D65828", "Never Clinical Exec" = "#AED1EC", "Ever Clinical Exec" ="#6BAED6", "Always Clinical Exec" = "#2C6B8E"))  +
  annotate(geom="label", x=2013.1, y=22.5, 
                                                label=paste0("ATT (s.e.) = ", round(wa_read_did,2)," (",round(wa_read_se,2),")"), 
                                                size=4,
                                                fill="gray90") 
ggsave(main_read_plot, filename=paste0(objects_path,"read_md_nomd_synth_graph.pdf"), width=5.5, height=6, units="in")


control_weights <- as.data.frame(synthdid_controls(wa_read_did)) %>%
  add_rownames(var="ID") %>%
  arrange(`estimate 1`)
n_control = control_weights %>%
  nrow()
time_weights <- as.data.frame(synthdid_controls(wa_read_did, weight.type = "lambda", mass=1)) %>%
  add_rownames(var="year")

ggplot(control_weights, aes(x=reorder(ID,`estimate 1`), y=`estimate 1`, group=ID)) + geom_bar(stat="identity") +
  xlab(paste0("Control Hospitals (n=",n_control,")")) + ylab("Weights\n") + theme_minimal() + ylim(0,.01) +
  theme(axis.text.x=element_blank(), text=element_text(size=18))  
ggsave(paste0(objects_path, "main_read_control_weights.pdf"), height=4, width=7, units="in")

ggplot(time_weights, aes(x=year, y=`estimate 1`)) + geom_bar(stat="identity") +
  xlab("Year") + ylab("Weights\n") + theme_minimal() + ylim(0,1) +
  theme(text=element_text(size=18))  
ggsave(paste0(objects_path, "main_read_time_weights.pdf"), height=4, width=6, units="in")


# Mortality
wa_mort_data <- main_analysis_data %>%
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
wa_mort_panel <- panel.matrices(as.data.frame(wa_mort_data))
X_mat_wa_mort <- wa_mort_data %>%
  select(ID, year, cmiv) %>%
  melt(id.var=c("ID", "year")) %>%
  nest_by(variable) %>%
  mutate(X=list(dcast(data.table(wa_mort_data), ID~year) %>%
                  .[data.table(ID=rownames(wa_mort_panel$Y)), on="ID"] %>%
                  .[, ID := NULL] %>%
                  as.matrix()
  )) %>%
  .$X %>%
  abind(along=2)

wa_mort_did <- synthdid_estimate(wa_mort_panel$Y, wa_mort_panel$N0, wa_mort_panel$T0, X=X_mat_wa_mort)

sprintf('point estimate: %1.2f', wa_mort_did)
wa_mort_se = sqrt(vcov(wa_mort_did))

main_mort_plot <- synthdid_plot(wa_mort_did, facet.vertical=FALSE,
                                control.name='Never Clinical Exec', treated.name='Always Clinical Exec',
                                lambda.comparable=TRUE, se.method = 'none',
                                lambda.plot.scale = 0,
                                trajectory.linetype = 1, line.width=.8, effect.curvature=-.4,
                                trajectory.alpha=1, effect.alpha=0,
                                diagram.alpha=0, onset.alpha=1) + ylim(12,13) +
  theme(legend.position=c(.5,-.095), legend.direction='horizontal',
        legend.key=element_blank(), legend.background=element_blank(),
        strip.background=element_blank(), strip.text.x = element_blank(),
        text = element_text(size=18))  +
  scale_color_manual(values = c("For-profit" = "#D65828", "Never Clinical Exec" = "#AED1EC", "Ever Clinical Exec" ="#6BAED6", "Always Clinical Exec" = "#2C6B8E"))  +
  annotate(geom="label", x=2013.1, y=13, 
           label=paste0("ATT (s.e.) = ", round(wa_mort_did,2)," (",round(wa_mort_se,2),")"), 
           size=4,
           fill="gray90") 
ggsave(main_mort_plot, filename=paste0(objects_path,"mort_md_nomd_synth_graph.pdf"), width=5.5, height=6, units="in")

control_weights <- as.data.frame(synthdid_controls(wa_mort_did)) %>%
  add_rownames(var="ID") %>%
  arrange(`estimate 1`)
n_control = control_weights %>%
  nrow()
time_weights <- as.data.frame(synthdid_controls(wa_mort_did, weight.type = "lambda", mass=1)) %>%
  add_rownames(var="year")

ggplot(control_weights, aes(x=reorder(ID,`estimate 1`), y=`estimate 1`, group=ID)) + geom_bar(stat="identity") +
  xlab(paste0("Control Hospitals (n=",n_control,")")) + ylab("Weights\n") + theme_minimal() + ylim(0,.01) +
  theme(axis.text.x=element_blank(), text=element_text(size=18))  
ggsave(paste0(objects_path, "main_mort_control_weights.pdf"), height=4, width=7, units="in")

ggplot(time_weights, aes(x=year, y=`estimate 1`)) + geom_bar(stat="identity") +
  xlab("Year") + ylab("Weights\n") + theme_minimal() + ylim(0,1) +
  theme(text=element_text(size=18))  
ggsave(paste0(objects_path, "main_mort_time_weights.pdf"), height=4, width=6, units="in")


# FIGURE 3/4: Intensive Margin main results ####
cont_main_analysis_data <- hospital_data %>%
  filter(no_num_md_change_2010_2014==1 & !is.na(ever_has_md)) 
cont_main_analysis_data_ever <- cont_main_analysis_data %>%
  filter(ever_has_md==1)

median <- median(cont_main_analysis_data_ever$frac_doc_exec)

cont_main_analysis_data <- cont_main_analysis_data %>%
  mutate(never_has_md=ifelse(ever_has_md==1,0,1),
         post_2012 = ifelse(year>2012,1,0),
         one_fourth = ifelse(frac_doc_exec>0 & frac_doc_exec<=.25,1,0),
         one_half = ifelse(frac_doc_exec>.25 & frac_doc_exec<=.5,1,0),
         below_med = ifelse(frac_doc_exec<median & frac_doc_exec>0,1,0),
         above_med = ifelse(frac_doc_exec>=median & frac_doc_exec>0,1,0)) %>%
  mutate(count=1) %>%
  group_by(ID, year) %>%
  mutate(sum=sum(count)) %>%
  ungroup() %>%
  filter(!(sum==2 & is.na(weightedavg_read))) %>%
  select(-count, -sum)

# Readmission: one quarter
cont_onefourth_read_data <- cont_main_analysis_data %>%
  filter(year!=2015 & (frac_doc_exec<=.25)) %>%
  mutate(did = post_2012 * one_fourth==1) %>%
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
cont_onefourth_read_panel <- panel.matrices(as.data.frame(cont_onefourth_read_data))
X_mat_cont_onefourth_read <- cont_onefourth_read_data %>%
  select(ID, year, cmiv) %>%
  melt(id.var=c("ID", "year")) %>%
  nest_by(variable) %>%
  mutate(X=list(dcast(data.table(cont_onefourth_read_data), ID~year) %>%
                  .[data.table(ID=rownames(cont_onefourth_read_panel$Y)), on="ID"] %>%
                  .[, ID := NULL] %>%
                  as.matrix()
  )) %>%
  .$X %>%
  abind(along=2)

cont_onefourth_read_did <- synthdid_estimate(cont_onefourth_read_panel$Y, cont_onefourth_read_panel$N0, cont_onefourth_read_panel$T0, X=X_mat_cont_onefourth_read)

sprintf('point estimate: %1.2f', cont_onefourth_read_did)
cont_onefourth_read_se = sqrt(vcov(cont_onefourth_read_did))

# Readmission: one half
cont_onehalf_read_data <- cont_main_analysis_data %>%
  filter(year!=2015 & ((frac_doc_exec<=.5 & frac_doc_exec>.25) | frac_doc_exec==0)) %>%
  mutate(did = post_2012 * one_half==1) %>%
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
cont_onehalf_read_panel <- panel.matrices(as.data.frame(cont_onehalf_read_data))
X_mat_cont_onehalf_read <- cont_onehalf_read_data %>%
  select(ID, year, cmiv) %>%
  melt(id.var=c("ID", "year")) %>%
  nest_by(variable) %>%
  mutate(X=list(dcast(data.table(cont_onehalf_read_data), ID~year) %>%
                  .[data.table(ID=rownames(cont_onehalf_read_panel$Y)), on="ID"] %>%
                  .[, ID := NULL] %>%
                  as.matrix()
  )) %>%
  .$X %>%
  abind(along=2)

cont_onehalf_read_did <- synthdid_estimate(cont_onehalf_read_panel$Y, cont_onehalf_read_panel$N0, cont_onehalf_read_panel$T0, X=X_mat_cont_onehalf_read)

sprintf('point estimate: %1.2f', cont_onehalf_read_did)
cont_onehalf_read_se = sqrt(vcov(cont_onehalf_read_did))

observe <- cont_main_analysis_data %>%
  filter(frac_doc_exec>.5) %>%
  select(year,ID,frac_doc_exec)


cont_onefourth_read_plot <- synthdid_plot(cont_onefourth_read_did, facet.vertical=FALSE,
                                          control.name='Never Clinical Exec', treated.name='< 1/4 Clinical Execs',
                                          lambda.comparable=TRUE, se.method = 'none',
                                          lambda.plot.scale = 0,
                                          trajectory.linetype = 1, line.width=.8, effect.curvature=-.4,
                                          trajectory.alpha=1, effect.alpha=0,
                                          diagram.alpha=0, onset.alpha=1) +
  theme(legend.position=c(.5,-.095), legend.direction='horizontal',
        legend.key=element_blank(), legend.background=element_blank(),
        strip.background=element_blank(), strip.text.x = element_blank(),
        text = element_text(size=18)) + ylim(19.5,22.5) +
  scale_color_manual(values = c("For-profit" = "#D65828", "Never Clinical Exec" = "#AED1EC", "< 1/4 Clinical Execs" ="#6BAED6", "Always Clinical Exec" = "#2C6B8E"))  +
  annotate(geom="label", x=2013.1, y=22.5, 
           label=paste0("ATT (s.e.) = ", round(cont_onefourth_read_did,2)," (",round(cont_onefourth_read_se,2),")"), 
           size=4,
           fill="gray90") 
ggsave(cont_onefourth_read_plot, filename=paste0(objects_path,"cont_onefourthread_md_nomd_synth_graph.pdf"), width=5.5, height=6, units="in")

cont_onehalf_read_plot <- synthdid_plot(cont_onehalf_read_did, facet.vertical=FALSE,
                                        control.name='Never Clinical Exec', treated.name='> 1/4 Clinical Execs',
                                        lambda.comparable=TRUE, se.method = 'none',
                                        lambda.plot.scale = 0,
                                        trajectory.linetype = 1, line.width=.8, effect.curvature=-.4,
                                        trajectory.alpha=1, effect.alpha=0,
                                        diagram.alpha=0, onset.alpha=1) +
  theme(legend.position=c(.5,-.095), legend.direction='horizontal',
        legend.key=element_blank(), legend.background=element_blank(),
        strip.background=element_blank(), strip.text.x = element_blank(),
        text = element_text(size=18)) + ylim(19.5,22.5) +
  scale_color_manual(values = c("For-profit" = "#D65828", "Never Clinical Exec" = "#AED1EC", "< 1/4 Clinical Execs" ="#6BAED6", "> 1/4 Clinical Execs" = "#2C6B8E"))  +
  annotate(geom="label", x=2013.1, y=22.5, 
           label=paste0("ATT (s.e.) = ", round(cont_onehalf_read_did,2)," (",round(cont_onehalf_read_se,2),")"), 
           size=4,
           fill="gray90") 
ggsave(cont_onehalf_read_plot, filename=paste0(objects_path,"cont_onehalfread_md_nomd_synth_graph.pdf"), width=5.5, height=6, units="in")

# below median 
cont_belowmed_read_data <- cont_main_analysis_data %>%
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
cont_belowmed_read_panel <- panel.matrices(as.data.frame(cont_belowmed_read_data))
X_mat_cont_belowmed_read <- cont_belowmed_read_data %>%
  select(ID, year, cmiv) %>%
  melt(id.var=c("ID", "year")) %>%
  nest_by(variable) %>%
  mutate(X=list(dcast(data.table(cont_belowmed_read_data), ID~year) %>%
                  .[data.table(ID=rownames(cont_belowmed_read_panel$Y)), on="ID"] %>%
                  .[, ID := NULL] %>%
                  as.matrix()
  )) %>%
  .$X %>%
  abind(along=2)

cont_belowmed_read_did <- synthdid_estimate(cont_belowmed_read_panel$Y, cont_belowmed_read_panel$N0, cont_belowmed_read_panel$T0, X=X_mat_cont_belowmed_read)

sprintf('point estimate: %1.2f', cont_belowmed_read_did)
cont_belowmed_read_se = sqrt(vcov(cont_belowmed_read_did))

cont_belowmed_read_plot <- synthdid_plot(cont_belowmed_read_did, facet.vertical=FALSE,
                                         control.name='Never Clinical Exec', treated.name='< Median Clinical Execs',
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
           label=paste0("ATT (s.e.) = ", round(cont_belowmed_read_did,2)," (",round(cont_belowmed_read_se,2),")"), 
           size=4,
           fill="gray90") 
ggsave(cont_belowmed_read_plot, filename=paste0(objects_path,"cont_belowmedread_md_nomd_synth_graph.pdf"), width=5.5, height=6, units="in")


# above median 
cont_abovemed_read_data <- cont_main_analysis_data %>%
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
cont_abovemed_read_panel <- panel.matrices(as.data.frame(cont_abovemed_read_data))
X_mat_cont_abovemed_read <- cont_abovemed_read_data %>%
  select(ID, year, cmiv) %>%
  melt(id.var=c("ID", "year")) %>%
  nest_by(variable) %>%
  mutate(X=list(dcast(data.table(cont_abovemed_read_data), ID~year) %>%
                  .[data.table(ID=rownames(cont_abovemed_read_panel$Y)), on="ID"] %>%
                  .[, ID := NULL] %>%
                  as.matrix()
  )) %>%
  .$X %>%
  abind(along=2)

cont_abovemed_read_did <- synthdid_estimate(cont_abovemed_read_panel$Y, cont_abovemed_read_panel$N0, cont_abovemed_read_panel$T0, X=X_mat_cont_abovemed_read)

sprintf('point estimate: %1.2f', cont_abovemed_read_did)
cont_abovemed_read_se = sqrt(vcov(cont_abovemed_read_did))

cont_abovemed_read_plot <- synthdid_plot(cont_abovemed_read_did, facet.vertical=FALSE,
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
           label=paste0("ATT (s.e.) = ", round(cont_abovemed_read_did,2)," (",round(cont_abovemed_read_se,2),")"), 
           size=4,
           fill="gray90") 
ggsave(cont_abovemed_read_plot, filename=paste0(objects_path,"cont_abovemedread_md_nomd_synth_graph.pdf"), width=5.5, height=6, units="in")



# Mortality
# one quarter
cont_onefourth_mort_data <- cont_main_analysis_data %>%
  filter(year!=2015 & (frac_doc_exec<=.25)) %>%
  mutate(did = post_2012 * one_fourth==1) %>%
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
cont_onefourth_mort_panel <- panel.matrices(as.data.frame(cont_onefourth_mort_data))
X_mat_cont_onefourth_mort <- cont_onefourth_mort_data %>%
  select(ID, year, cmiv) %>%
  melt(id.var=c("ID", "year")) %>%
  nest_by(variable) %>%
  mutate(X=list(dcast(data.table(cont_onefourth_mort_data), ID~year) %>%
                  .[data.table(ID=rownames(cont_onefourth_mort_panel$Y)), on="ID"] %>%
                  .[, ID := NULL] %>%
                  as.matrix()
  )) %>%
  .$X %>%
  abind(along=2)

cont_onefourth_mort_did <- synthdid_estimate(cont_onefourth_mort_panel$Y, cont_onefourth_mort_panel$N0, cont_onefourth_mort_panel$T0, X=X_mat_cont_onefourth_mort)

sprintf('point estimate: %1.2f', cont_onefourth_mort_did)
cont_onefourth_mort_se = sqrt(vcov(cont_onefourth_mort_did))

# one half
cont_onehalf_mort_data <- cont_main_analysis_data %>%
  filter(year!=2015 & ((frac_doc_exec<=.5 & frac_doc_exec>.25) | frac_doc_exec==0)) %>%
  mutate(did = post_2012 * one_half==1) %>%
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
cont_onehalf_mort_panel <- panel.matrices(as.data.frame(cont_onehalf_mort_data))
X_mat_cont_onehalf_mort <- cont_onehalf_mort_data %>%
  select(ID, year, cmiv) %>%
  melt(id.var=c("ID", "year")) %>%
  nest_by(variable) %>%
  mutate(X=list(dcast(data.table(cont_onehalf_mort_data), ID~year) %>%
                  .[data.table(ID=rownames(cont_onehalf_mort_panel$Y)), on="ID"] %>%
                  .[, ID := NULL] %>%
                  as.matrix()
  )) %>%
  .$X %>%
  abind(along=2)

cont_onehalf_mort_did <- synthdid_estimate(cont_onehalf_mort_panel$Y, cont_onehalf_mort_panel$N0, cont_onehalf_mort_panel$T0, X=X_mat_cont_onehalf_mort)

sprintf('point estimate: %1.2f', cont_onehalf_mort_did)
cont_onehalf_mort_se = sqrt(vcov(cont_onehalf_mort_did))


cont_onefourth_mort_plot <- synthdid_plot(cont_onefourth_mort_did, facet.vertical=FALSE,
                                          control.name='Never Clinical Exec', treated.name='< 1/4 Clinical Execs',
                                          lambda.comparable=TRUE, se.method = 'none',
                                          lambda.plot.scale = 0,
                                          trajectory.linetype = 1, line.width=.8, effect.curvature=-.4,
                                          trajectory.alpha=1, effect.alpha=0,
                                          diagram.alpha=0, onset.alpha=1) +
  theme(legend.position=c(.5,-.095), legend.direction='horizontal',
        legend.key=element_blank(), legend.background=element_blank(),
        strip.background=element_blank(), strip.text.x = element_blank(),
        text = element_text(size=18)) + ylim(11,13) +
  scale_color_manual(values = c("For-profit" = "#D65828", "Never Clinical Exec" = "#AED1EC", "< 1/4 Clinical Execs" ="#6BAED6", "Always Clinical Exec" = "#2C6B8E"))  +
  annotate(geom="label", x=2013.1, y=13, 
           label=paste0("ATT (s.e.) = ", round(cont_onefourth_mort_did,2)," (",round(cont_onefourth_mort_se,2),")"), 
           size=4,
           fill="gray90") 
ggsave(cont_onefourth_mort_plot, filename=paste0(objects_path,"cont_onefourthmort_md_nomd_synth_graph.pdf"), width=5.5, height=6, units="in")

cont_onehalf_mort_plot <- synthdid_plot(cont_onehalf_mort_did, facet.vertical=FALSE,
                                        control.name='Never Clinical Exec', treated.name='> 1/4 Clinical Execs',
                                        lambda.comparable=TRUE, se.method = 'none',
                                        lambda.plot.scale = 0,
                                        trajectory.linetype = 1, line.width=.8, effect.curvature=-.4,
                                        trajectory.alpha=1, effect.alpha=0,
                                        diagram.alpha=0, onset.alpha=1) +
  theme(legend.position=c(.5,-.095), legend.direction='horizontal',
        legend.key=element_blank(), legend.background=element_blank(),
        strip.background=element_blank(), strip.text.x = element_blank(),
        text = element_text(size=18)) + ylim(11,13) +
  scale_color_manual(values = c("For-profit" = "#D65828", "Never Clinical Exec" = "#AED1EC", "< 1/4 Clinical Execs" ="#6BAED6", "> 1/4 Clinical Execs" = "#2C6B8E"))  +
  annotate(geom="label", x=2013.1, y=13, 
           label=paste0("ATT (s.e.) = ", round(cont_onehalf_mort_did,2)," (",round(cont_onehalf_mort_se,2),")"), 
           size=4,
           fill="gray90") 
ggsave(cont_onehalf_mort_plot, filename=paste0(objects_path,"cont_onehalfmort_md_nomd_synth_graph.pdf"), width=5.5, height=6, units="in")

# below median 
cont_belowmed_mort_data <- cont_main_analysis_data %>%
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
cont_belowmed_mort_panel <- panel.matrices(as.data.frame(cont_belowmed_mort_data))
X_mat_cont_belowmed_mort <- cont_belowmed_mort_data %>%
  select(ID, year, cmiv) %>%
  melt(id.var=c("ID", "year")) %>%
  nest_by(variable) %>%
  mutate(X=list(dcast(data.table(cont_belowmed_mort_data), ID~year) %>%
                  .[data.table(ID=rownames(cont_belowmed_mort_panel$Y)), on="ID"] %>%
                  .[, ID := NULL] %>%
                  as.matrix()
  )) %>%
  .$X %>%
  abind(along=2)

cont_belowmed_mort_did <- synthdid_estimate(cont_belowmed_mort_panel$Y, cont_belowmed_mort_panel$N0, cont_belowmed_mort_panel$T0, X=X_mat_cont_belowmed_mort)

sprintf('point estimate: %1.2f', cont_belowmed_mort_did)
cont_belowmed_mort_se = sqrt(vcov(cont_belowmed_mort_did))

cont_belowmed_mort_plot <- synthdid_plot(cont_belowmed_mort_did, facet.vertical=FALSE,
                                         control.name='Never Clinical Exec', treated.name='< Median Clinical Execs',
                                         lambda.comparable=TRUE, se.method = 'none',
                                         lambda.plot.scale = 0,
                                         trajectory.linetype = 1, line.width=.8, effect.curvature=-.4,
                                         trajectory.alpha=1, effect.alpha=0,
                                         diagram.alpha=0, onset.alpha=1) +
  theme(legend.position=c(.5,-.095), legend.direction='horizontal',
        legend.key=element_blank(), legend.background=element_blank(),
        strip.background=element_blank(), strip.text.x = element_blank(),
        text = element_text(size=18)) + ylim(11,13) +
  scale_color_manual(values = c("For-profit" = "#D65828", "Never Clinical Exec" = "#AED1EC", "< Median Clinical Execs" ="#6BAED6", "> Median Clinical Execs" = "#2C6B8E"))  +
  annotate(geom="label", x=2013.1, y=13, 
           label=paste0("ATT (s.e.) = ", round(cont_belowmed_mort_did,2)," (",round(cont_belowmed_mort_se,2),")"), 
           size=4,
           fill="gray90") 
ggsave(cont_belowmed_mort_plot, filename=paste0(objects_path,"cont_belowmedmort_md_nomd_synth_graph.pdf"), width=5.5, height=6, units="in")


# above median 
cont_abovemed_mort_data <- cont_main_analysis_data %>%
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
cont_abovemed_mort_panel <- panel.matrices(as.data.frame(cont_abovemed_mort_data))
X_mat_cont_abovemed_mort <- cont_abovemed_mort_data %>%
  select(ID, year, cmiv) %>%
  melt(id.var=c("ID", "year")) %>%
  nest_by(variable) %>%
  mutate(X=list(dcast(data.table(cont_abovemed_mort_data), ID~year) %>%
                  .[data.table(ID=rownames(cont_abovemed_mort_panel$Y)), on="ID"] %>%
                  .[, ID := NULL] %>%
                  as.matrix()
  )) %>%
  .$X %>%
  abind(along=2)

cont_abovemed_mort_did <- synthdid_estimate(cont_abovemed_mort_panel$Y, cont_abovemed_mort_panel$N0, cont_abovemed_mort_panel$T0, X=X_mat_cont_abovemed_mort)

sprintf('point estimate: %1.2f', cont_abovemed_mort_did)
cont_abovemed_mort_se = sqrt(vcov(cont_abovemed_mort_did))

cont_abovemed_mort_plot <- synthdid_plot(cont_abovemed_mort_did, facet.vertical=FALSE,
                                         control.name='Never Clinical Exec', treated.name='> Median Clinical Execs',
                                         lambda.comparable=TRUE, se.method = 'none',
                                         lambda.plot.scale = 0,
                                         trajectory.linetype = 1, line.width=.8, effect.curvature=-.4,
                                         trajectory.alpha=1, effect.alpha=0,
                                         diagram.alpha=0, onset.alpha=1) +
  theme(legend.position=c(.5,-.095), legend.direction='horizontal',
        legend.key=element_blank(), legend.background=element_blank(),
        strip.background=element_blank(), strip.text.x = element_blank(),
        text = element_text(size=18)) + ylim(11,13) +
  scale_color_manual(values = c("For-profit" = "#D65828", "Never Clinical Exec" = "#AED1EC", "< Median Clinical Execs" ="#6BAED6", "> Median Clinical Execs" = "#2C6B8E"))  +
  annotate(geom="label", x=2013.1, y=13, 
           label=paste0("ATT (s.e.) = ", round(cont_abovemed_mort_did,2)," (",round(cont_abovemed_mort_se,2),")"), 
           size=4,
           fill="gray90") 
ggsave(cont_abovemed_mort_plot, filename=paste0(objects_path,"cont_abovemedmort_md_nomd_synth_graph.pdf"), width=5.5, height=6, units="in")

# FIGURE 5: comparing always and never clinical to for-profit (readmission) ####
fp_analysis_data <- hospital_data %>%
  filter((no_num_md_change_2010_2014==1 & !is.na(ever_has_md)) | profit_status=="forprofit") %>%
  mutate(never_has_md=ifelse(ever_has_md==1,0,1),
         post_2012 = ifelse(year>2012,1,0)) %>%
  mutate(did=post_2012*ever_has_md) %>%
  mutate(count=1) %>%
  group_by(ID, year) %>%
  mutate(sum=sum(count)) %>%
  ungroup() %>%
  filter(!(sum==2 & is.na(weightedavg_read))) %>%
  select(-count, -sum)

# Readmission: compare clinical to FP
fp_wa_read_md_data <- fp_analysis_data %>%
  filter(profit_status=="forprofit" | ever_has_md==1) %>%
  filter(year!=2015) %>%
  select(ID, year, weightedavg_read, did, cmiv) %>%
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

fp_wa_read_md_did <- synthdid_estimate(fp_wa_read_md_panel$Y, fp_wa_read_md_panel$N0, fp_wa_read_md_panel$T0, X=X_mat_fp_wa_read_md)

fp_wa_read_md_se = sqrt(vcov(fp_wa_read_md_did))

fp_read_md_plot <- synthdid_plot(fp_wa_read_md_did, facet.vertical=FALSE,
                                control.name='For-profit', treated.name='Always Clinical Exec',
                                lambda.comparable=TRUE, se.method = 'none',
                                lambda.plot.scale = 0,
                                trajectory.linetype = 1, line.width=.8, effect.curvature=-.4,
                                trajectory.alpha=1, effect.alpha=0,
                                diagram.alpha=0, onset.alpha=1) +
  theme(legend.position=c(.5,-.095), legend.direction='horizontal',
        legend.key=element_blank(), legend.background=element_blank(),
        strip.background=element_blank(), strip.text.x = element_blank(),
        text = element_text(size=18)) + ylim(19.5,22.5) +
  scale_color_manual(values = c("For-profit" = "#D65828", "Never Clinical Exec" = "#AED1EC", "Ever Clinical Exec" ="#6BAED6", "Always Clinical Exec" = "#2C6B8E"))  +
  annotate(geom="label", x=2013.1, y=22.5, 
           label=paste0("ATT (s.e.) = ", round(fp_wa_read_md_did,2)," (",round(fp_wa_read_md_se,2),")"), 
           size=4,
           fill="gray90") 
ggsave(fp_read_md_plot, filename=paste0(objects_path,"fp_read_md_synth_graph.pdf"), width=5.5, height=6, units="in")

# Readmission: compare non-clinical to FP
fp_wa_read_nomd_data <- fp_analysis_data %>%
  filter(profit_status=="forprofit" | ever_has_md==0) %>%
  filter(year!=2015) %>%
  mutate(did = never_has_md*post_2012) %>%
  select(ID, year, weightedavg_read, did, cmiv) %>%
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

fp_wa_read_nomd_did <- synthdid_estimate(fp_wa_read_nomd_panel$Y, fp_wa_read_nomd_panel$N0, fp_wa_read_nomd_panel$T0, X=X_mat_fp_wa_read_nomd)

fp_wa_read_nomd_se = sqrt(vcov(fp_wa_read_nomd_did))

fp_read_nomd_plot <- synthdid_plot(fp_wa_read_nomd_did, facet.vertical=FALSE,
                                 control.name='For-profit', treated.name='Never Clinical Exec',
                                 lambda.comparable=TRUE, se.method = 'none',
                                 lambda.plot.scale = 0,
                                 trajectory.linetype = 1, line.width=.8, effect.curvature=-.4,
                                 trajectory.alpha=1, effect.alpha=0,
                                 diagram.alpha=0, onset.alpha=1) +
  theme(legend.position=c(.5,-.095), legend.direction='horizontal',
        legend.key=element_blank(), legend.background=element_blank(),
        strip.background=element_blank(), strip.text.x = element_blank(),
        text = element_text(size=18)) + ylim(19.5,22.5) +
  scale_color_manual(values = c("For-profit" = "#D65828", "Never Clinical Exec" = "#AED1EC", "Ever Clinical Exec" ="#6BAED6", "Always Clinical Exec" = "#2C6B8E"))  +
  annotate(geom="label", x=2013.1, y=22.5, 
           label=paste0("ATT (s.e.) = ", round(fp_wa_read_nomd_did,2)," (",round(fp_wa_read_nomd_se,2),")"), 
           size=4,
           fill="gray90") 
ggsave(fp_read_nomd_plot, filename=paste0(objects_path,"fp_read_nomd_synth_graph.pdf"), width=5.5, height=6, units="in")


# FIGURE 6: compare always and never clinical to for-profit (mortality) ####
# mortality: compare clinical to FP
fp_wa_mort_md_data <- fp_analysis_data %>%
  filter(profit_status=="forprofit" | ever_has_md==1) %>%
  filter(year!=2015) %>%
  select(ID, year, weightedavg_mort, did, cmiv) %>%
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

fp_wa_mort_md_did <- synthdid_estimate(fp_wa_mort_md_panel$Y, fp_wa_mort_md_panel$N0, fp_wa_mort_md_panel$T0, X=X_mat_fp_wa_mort_md)

fp_wa_mort_md_se = sqrt(vcov(fp_wa_mort_md_did))

fp_mort_md_plot <- synthdid_plot(fp_wa_mort_md_did, facet.vertical=FALSE,
                                 control.name='For-profit', treated.name='Always Clinical Exec',
                                 lambda.comparable=TRUE, se.method = 'none',
                                 lambda.plot.scale = 0,
                                 trajectory.linetype = 1, line.width=.8, effect.curvature=-.4,
                                 trajectory.alpha=1, effect.alpha=0,
                                 diagram.alpha=0, onset.alpha=1) +
  theme(legend.position=c(.5,-.095), legend.direction='horizontal',
        legend.key=element_blank(), legend.background=element_blank(),
        strip.background=element_blank(), strip.text.x = element_blank(),
        text = element_text(size=18)) + ylim(12,13) +
  scale_color_manual(values = c("For-profit" = "#D65828", "Never Clinical Exec" = "#AED1EC", "Ever Clinical Exec" ="#6BAED6", "Always Clinical Exec" = "#2C6B8E"))  +
  annotate(geom="label", x=2013.1, y=13, 
           label=paste0("ATT (s.e.) = ", round(fp_wa_mort_md_did,2)," (",round(fp_wa_mort_md_se,2),")"), 
           size=4,
           fill="gray90") 
ggsave(fp_mort_md_plot, filename=paste0(objects_path,"fp_mort_md_synth_graph.pdf"), width=5.5, height=6, units="in")

# mortmission: compare non-clinical to FP
fp_wa_mort_nomd_data <- fp_analysis_data %>%
  filter(profit_status=="forprofit" | ever_has_md==0) %>%
  filter(year!=2015) %>%
  mutate(did = never_has_md*post_2012) %>%
  select(ID, year, weightedavg_mort, did, cmiv) %>%
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

fp_wa_mort_nomd_did <- synthdid_estimate(fp_wa_mort_nomd_panel$Y, fp_wa_mort_nomd_panel$N0, fp_wa_mort_nomd_panel$T0, X=X_mat_fp_wa_mort_nomd)

fp_wa_mort_nomd_se = sqrt(vcov(fp_wa_mort_nomd_did))

fp_mort_nomd_plot <- synthdid_plot(fp_wa_mort_nomd_did, facet.vertical=FALSE,
                                   control.name='For-profit', treated.name='Never Clinical Exec',
                                   lambda.comparable=TRUE, se.method = 'none',
                                   lambda.plot.scale = 0,
                                   trajectory.linetype = 1, line.width=.8, effect.curvature=-.4,
                                   trajectory.alpha=1, effect.alpha=0,
                                   diagram.alpha=0, onset.alpha=1) +
  theme(legend.position=c(.5,-.095), legend.direction='horizontal',
        legend.key=element_blank(), legend.background=element_blank(),
        strip.background=element_blank(), strip.text.x = element_blank(),
        text = element_text(size=18)) + ylim(12,13) +
  scale_color_manual(values = c("For-profit" = "#D65828", "Never Clinical Exec" = "#AED1EC", "Ever Clinical Exec" ="#6BAED6", "Always Clinical Exec" = "#2C6B8E"))  +
  annotate(geom="label", x=2013.1, y=13, 
           label=paste0("ATT (s.e.) = ", round(fp_wa_mort_nomd_did,2)," (",round(fp_wa_mort_nomd_se,2),")"), 
           size=4,
           fill="gray90") 
ggsave(fp_mort_nomd_plot, filename=paste0(objects_path,"fp_mort_nomd_synth_graph.pdf"), width=5.5, height=6, units="in")



# FIGURE 7: graph leadership team changes over time ####
change_data <- hospital_data %>%
  mutate(yr2010=ifelse(year==2010,1,0),
         yr2011=ifelse(year==2011,1,0),
         yr2012=ifelse(year==2012,1,0),
         yr2013=ifelse(year==2013,1,0),
         yr2014=ifelse(year==2014,1,0)) %>%
  mutate(num_doc_2010 = ifelse(year==2010, num_doctors, NA)) %>%
  mutate(num_doc_change_2010 = ifelse(year==2010, num_doc_change, NA)) %>%
  group_by(ID) %>%
  fill(num_doc_2010, num_doc_change_2010, .direction="downup") %>%
  ungroup() %>%
  mutate(num_doctors = ifelse(year==2009, num_doc_2010-num_doc_change_2010, num_doctors))

change_data <- change_data %>%
  group_by(ID) %>%
  arrange(year) %>%
  mutate(lag_num_doctors = dplyr::lag(num_doctors)) %>%
  ungroup() %>%
  mutate(any_doc_change = ifelse((num_doctors>0 & lag_num_doctors==0) | (num_doctors==0 & lag_num_doctors>0),1,0)) 

change_sum_stats <- change_data %>%
  select(year, any_doc_change, hire, fire) %>%
  rename(`Hire Clinical Exec`=hire, `Fire Clinical Exec`=fire, `Any Change Clinical Exec`=any_doc_change) %>%
  group_by(year) %>%
  summarise_all(list(mean), na.rm=T) %>%
  filter(year!=2009 & year!=2015) %>%
  mutate(`Any Change Clinical Exec` = ifelse(year==2010, NA, `Any Change Clinical Exec`)) %>%
  fill(`Any Change Clinical Exec`, .direction="up") %>%
  pivot_longer(cols = 2:4, names_to = "variable", values_to = "mean")


ggplot(change_sum_stats, aes(x=year, y=mean, group=variable, color=variable)) + geom_point(size=3) + geom_line() +
  ylim(0,1) + theme_bw() +
  geom_vline(xintercept = 2012, linetype = "dotted") +
  geom_line(linewidth=1.2) + theme_bw() + xlab("") + ylab("Change Variables\n") + theme(legend.title = element_blank()) +
  theme(text=element_text(size=18)) + 
  scale_color_brewer(palette="Set2")
ggsave(plot=last_plot(), file="Objects/change_means.pdf", height=5, width=9, units="in")


# FIGURE 8: leadership change analysis ####
change_data <- change_data %>%
  filter(!is.na(no_num_md_change_2010_2014)) %>%
  mutate(post_2011=ifelse(year>2011,1,0)) %>%
  mutate(did_pen=post_2011*ever_penalized) %>%
  mutate(did_pay = post_2011*ever_hvbp) %>%
  mutate(yr_2010 = ifelse(year==2010,1,0)) %>%
  mutate(first_tercile = ifelse(rate_tercile==1,1,0)) %>%
  mutate(first_tercile = ifelse(is.na(first_tercile),0,first_tercile)) %>%
  mutate(second_tercile = ifelse(rate_tercile==2,1,0)) %>%
  mutate(second_tercile = ifelse(is.na(second_tercile),0,second_tercile)) %>%
  mutate(third_tercile = ifelse(rate_tercile==3,1,0)) %>%
  mutate(third_tercile = ifelse(is.na(third_tercile),0,third_tercile))

observe <- change_data %>%
  select(ID, year, did_pen, first_tercile)

# 1. Are changes concentrated around 2011-2012?
num_md_change_year_did <- fixest::feols(num_doc_change ~ yr2011 + yr2012 + yr2013 + yr2014 | ID, cluster = ~ID, data=change_data)

# 2. Are hospitals who eventually get penalized more likely to change leadership?
num_md_change_did <- fixest::feols(num_doc_change ~ ever_penalized:yr2011 + ever_penalized:yr2012 + 
                                     ever_penalized:yr2013 + ever_penalized:yr2014 | ID + year, cluster = ~ID, data=change_data)

# 3. Within penalized hospitals, are those in different terciles more likely to change leadership?
num_md_change_terc_did <- fixest::feols(num_doc_change ~ second_tercile:yr2011 + second_tercile:yr2012 + second_tercile:yr2013 + second_tercile:yr2014 +
                                          third_tercile:yr2011 + third_tercile:yr2012 + third_tercile:yr2013 + third_tercile:yr2014 | ID + year, cluster = ~ID, 
                                        data = filter(change_data, ever_penalized==1))



# 2. Are hospitals who eventually get payments from HVBP more likely to change leadership?
num_md_change_pay_did <- fixest::feols(num_doc_change ~ ever_hvbp:yr2011 + ever_hvbp:yr2012 + ever_hvbp:yr2013 + ever_hvbp:yr2014  | ID + year, cluster = ~ID, data=change_data)

change_analysis_tab <- etable(num_md_change_did, num_md_change_pay_did, num_md_change_year_did, tex=TRUE,
                              dict = c("yr2011"="2011", "yr2012"="2012", "yr2013"="2013", "yr2014"="2014", "did"="Ever Penalized x Post 2011",
                                       "ID"="Hospital", "year"="Year", "md_change"="Change in Any MD", "num_doc_change"="Change in Num. MDs",
                                       "did_pen"="Ever Penalized HRRP x Post 2011", "did_pay"="Ever Payment HVBP x Post 2011"),
                              style.tex = style.tex("aer"), fitstat = ~n,
                              digits = "r2", title = "Leadership Change Correlation with Programs",
                              drop.section="fixef", label = "tab:change_analysis",
                              notes = c("Results from Equations \\ref{eq:change1} and \\ref{eq:change2}.",
                                        "Standard errors are clustered at the hospital level.",
                                        "Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1"))

num_md_change_data <- as.data.frame(num_md_change_did$coeftable) %>%
  add_rownames(var = "variable") %>%
  mutate(`Interaction Term` = "Ever Penalized") %>%
  mutate(variable = str_remove(variable, "ever_penalized")) %>%
  mutate(variable = str_remove_all(variable, ":")) %>%
  mutate(variable = str_remove_all(variable, "yr"))
num_md_change_pay_data <- as.data.frame(num_md_change_pay_did$coeftable) %>%
  add_rownames(var = "variable") %>%
  mutate(`Interaction Term` = "Ever Payment") %>%
  mutate(variable = str_remove(variable, "ever_hvbp")) %>%
  mutate(variable = str_remove_all(variable, ":")) %>%
  mutate(variable = str_remove_all(variable, "yr"))
num_md_change_year_data <- as.data.frame(num_md_change_year_did$coeftable) %>%
  add_rownames(var = "variable") %>%
  mutate(`Interaction Term` = "None") %>%
  mutate(variable = str_remove_all(variable, "yr"))

change_analysis_data <- rbind(num_md_change_data, num_md_change_pay_data, num_md_change_year_data) %>%
  mutate(lower = Estimate - 1.96 * `Std. Error`) %>%
  mutate(upper = Estimate + 1.96 * `Std. Error`) %>%
  select(variable, Estimate, lower, upper, `Interaction Term`) %>%
  mutate(variable = as.numeric(variable))

pd <- position_dodge(0.2)
ggplot(change_analysis_data, aes(x=variable, y=Estimate, group=`Interaction Term`, color=`Interaction Term`)) + geom_hline(yintercept=0, linetype=2, linewidth=.5) +
  geom_point(size=3, position=pd) + geom_line(aes(linetype=`Interaction Term`), linewidth=1, position=pd) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.2, linewidth=1, position=pd) +
  ylim(-.5,.5) + theme_bw() + xlab(" ") + theme(text = element_text(size=18)) +
  scale_color_brewer(palette = "Set2", direction = -1) 
ggsave(plot=last_plot(), file = "Objects/change_analysis_plot.pdf", height=5, width=9, units="in")  

# TABLE 3: decomposition analysis ####
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

ever_read_did <- synthdid_estimate(ever_read_panel$Y, ever_read_panel$N0, ever_read_panel$T0, X=X_mat_ever_read)

ever_mort_data <- ever_data %>%
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

ever_mort_did <- synthdid_estimate(ever_mort_panel$Y, ever_mort_panel$N0, ever_mort_panel$T0, X=X_mat_ever_mort)

# comparing 2012
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

read_2012_did <- synthdid_estimate(read_2012_panel$Y, read_2012_panel$N0, read_2012_panel$T0, X=X_mat_2012_read)

mort_2012_data <- data_2012 %>%
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

mort_2012_did <- synthdid_estimate(mort_2012_panel$Y, mort_2012_panel$N0, mort_2012_panel$T0, X=X_mat_2012_mort)

# make table
coef_ever <- c(ever_read_did, NA, ever_mort_did, NA)
coef_2012 <- c(NA, read_2012_did,NA, mort_2012_did)
se_ever <- c(sqrt(vcov(ever_read_did)),NA,
             sqrt(vcov(ever_mort_did)), NA)
se_2012 <- c(NA, sqrt(vcov(read_2012_did)), NA,
             sqrt(vcov(mort_2012_did)))

obs1 <- (summary(ever_read_did)$dimensions[["N0"]]+summary(ever_read_did)$dimensions[["N1"]])*5
obs2 <- (summary(read_2012_did)$dimensions[["N0"]]+summary(read_2012_did)$dimensions[["N1"]])*5
obs3 <- (summary(ever_mort_did)$dimensions[["N0"]]+summary(ever_mort_did)$dimensions[["N1"]])*5
obs4 <- (summary(mort_2012_did)$dimensions[["N0"]]+summary(mort_2012_did)$dimensions[["N1"]])*5

obs1 <- as.character(obs1)
obs2 <- as.character(obs2)
obs3 <- as.character(obs3)
obs4 <- as.character(obs4)

estimates <- as.data.frame(list(coef_ever=coef_ever, se_ever=se_ever, coef_2012=coef_2012, se_2012=se_2012)) %>%
  mutate(p=round((2*(1-pnorm(abs(coef_ever/se_ever)))), 4)) %>%
  mutate(p=ifelse(is.na(p),round((2*(1-pnorm(abs(coef_2012/se_2012)))), 4),p)) %>%
  mutate(coef_ever=round(coef_ever,2),
         coef_2012=round(coef_2012,2),
         se_ever=round(se_ever,2),
         se_2012=round(se_2012,2)) %>%
  mutate(stars = case_when(
    p >= 0.05 ~ "",
    p < 0.001 ~ "$^{***}$",
    p < 0.01 ~ "$^{**}$",
    p < 0.05 ~ "$^{*}$")) %>%
  mutate(coef_ever=ifelse(!is.na(coef_ever),paste0(coef_ever,stars),coef_ever)) %>%
  mutate(coef_2012=ifelse(!is.na(coef_2012),paste0(coef_2012,stars),coef_2012)) %>%
  mutate(se_ever = ifelse(!is.na(se_ever),paste0("(",se_ever,")"),se_ever)) %>%
  mutate(se_2012 = ifelse(!is.na(se_2012),paste0("(",se_2012,")"),se_2012)) %>%
  select(-p, -stars)

estimates <- as.data.frame(t(estimates)) %>%
  mutate(x=c("Ever MD Exec x Post Programs", "", "2012 MD Exec x Post Programs", "")) %>%
  select(x, V1, V2, V3, V4) %>%
  add_row(x=" ") %>%
  add_row(x="\\textbf{Comparison Group:}") %>%
  add_row(x="Never MD", V1="$\\checkmark$", V3="$\\checkmark$") %>%
  add_row(x="Has MD not in 2012", V2="$\\checkmark$", V4="$\\checkmark$") %>%
  add_row(x=" ") %>%
  add_row(x="Observations", V1=obs1, V2=obs2, V3=obs3, V4=obs4)

tab1b <- knitr::kable(estimates, format="latex",
                      row.names = FALSE,
                      col.names = c("", "(1)", "(2)", "(1)", "(2)"),
                      booktabs = TRUE,
                      table.envir="table",
                      caption="\\label{tab:MD_noMD_readmort_decomp_synth}Decomposition of Readmissions and Mortality Synthetic DiD Results",
                      escape=F,
                      align=c("l","c","c","c","c"),
                      position="ht!") %>%
  add_header_above(c(" ", "Weighted Avg. Readmission Rate"=2, "Weighted Avg. Mortality Rate"=2), underline=FALSE, line=FALSE) %>%
  add_footnote(c("Results from Equation with readmission and mortality rates as outcome variables.",
                 "Standard errors are clustered at the hospital level.",
                 "Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1"), notation="none")
write(tab1b,file="Tables/MD_noMD_readmort_decomp_synth.tex")

# FIGURE 10: does specialty matter? ####
observe <- hospital_data %>%
  filter(year>=2010 & year<=2014) %>%
  select(ID, year, num_int_med_doctors, num_doctors)

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
  mutate(post_2012 = ifelse(year>2012,1,0)) 

# Readmissions
# compare never int med with always int med
specialty_read_data <- specialty_data %>%
  filter((always_int_med==1 & no_num_int_med_changes==1) | (never_int_med==1 & no_num_md_change_2010_2014)) %>%
  filter(num_doctors>0) %>%
  mutate(did = post_2012*always_int_med) %>%
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
specialty_read_panel <- panel.matrices(as.data.frame(specialty_read_data))
X_mat_specialty_read <- specialty_read_data %>%
  select(ID, year, cmiv) %>%
  melt(id.var=c("ID", "year")) %>%
  nest_by(variable) %>%
  mutate(X=list(dcast(data.table(specialty_read_data), ID~year) %>%
                  .[data.table(ID=rownames(specialty_read_panel$Y)), on="ID"] %>%
                  .[, ID := NULL] %>%
                  as.matrix()
  )) %>%
  .$X %>%
  abind(along=2)

specialty_read_did <- synthdid_estimate(specialty_read_panel$Y, specialty_read_panel$N0, specialty_read_panel$T0, X=X_mat_specialty_read)

sprintf('point estimate: %1.2f', specialty_read_did)
specialty_read_se = sqrt(vcov(specialty_read_did))

specialty_read_plot <- synthdid_plot(specialty_read_did, facet.vertical=FALSE,
                                control.name='Other Clinical Exec', treated.name='Internal Med Exec',
                                lambda.comparable=TRUE, se.method = 'none',
                                lambda.plot.scale = 0,
                                trajectory.linetype = 1, line.width=.8, effect.curvature=-.4,
                                trajectory.alpha=1, effect.alpha=0,
                                diagram.alpha=0, onset.alpha=1) +
  theme(legend.position=c(.5,-.095), legend.direction='horizontal',
        legend.key=element_blank(), legend.background=element_blank(),
        strip.background=element_blank(), strip.text.x = element_blank(),
        text = element_text(size=18)) + ylim(19.5,22.5) +
  scale_color_manual(values = c("For-profit" = "#D65828", "Never Clinical Exec" = "#AED1EC", "Ever Clinical Exec" ="#6BAED6", "Always Clinical Exec" = "#2C6B8E",
                                "Other Clinical Exec" = "#c6bfca", 'Internal Med Exec' = "#A5C499"))  +
  annotate(geom="label", x=2013.1, y=22.5, 
           label=paste0("ATT (s.e.) = ", round(specialty_read_did,2)," (",round(specialty_read_se,2),")"), 
           size=4,
           fill="gray90") 
ggsave(specialty_read_plot, filename=paste0(objects_path,"read_specialty_synth_graph.pdf"), width=5.5, height=6, units="in")

# compare always int med to no doctor
specialty_nomd_read_data <- specialty_data %>%
  filter(always_int_med==1 | ever_has_md==0) %>%
  mutate(did = post_2012*always_int_med) %>%
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
specialty_nomd_read_panel <- panel.matrices(as.data.frame(specialty_nomd_read_data))
X_mat_specialty_nomd_read <- specialty_nomd_read_data %>%
  select(ID, year, cmiv) %>%
  melt(id.var=c("ID", "year")) %>%
  nest_by(variable) %>%
  mutate(X=list(dcast(data.table(specialty_nomd_read_data), ID~year) %>%
                  .[data.table(ID=rownames(specialty_nomd_read_panel$Y)), on="ID"] %>%
                  .[, ID := NULL] %>%
                  as.matrix()
  )) %>%
  .$X %>%
  abind(along=2)

specialty_nomd_read_did <- synthdid_estimate(specialty_nomd_read_panel$Y, specialty_nomd_read_panel$N0, specialty_nomd_read_panel$T0, X=X_mat_specialty_nomd_read)

sprintf('point estimate: %1.2f', specialty_nomd_read_did)
specialty_nomd_read_se = sqrt(vcov(specialty_nomd_read_did))

specialty_nomd_read_plot <- synthdid_plot(specialty_nomd_read_did, facet.vertical=FALSE,
                                     control.name='Never Clinical Exec', treated.name='Always Internal Medicine Exec',
                                     lambda.comparable=TRUE, se.method = 'none',
                                     lambda.plot.scale = 0,
                                     trajectory.linetype = 1, line.width=.8, effect.curvature=-.4,
                                     trajectory.alpha=1, effect.alpha=0,
                                     diagram.alpha=0, onset.alpha=1) +
  theme(legend.position=c(.5,-.095), legend.direction='horizontal',
        legend.key=element_blank(), legend.background=element_blank(),
        strip.background=element_blank(), strip.text.x = element_blank(),
        text = element_text(size=18)) + ylim(19.5,22.5) +
  scale_color_manual(values = c("For-profit" = "#D65828", "Never Clinical Exec" = "#AED1EC", "Ever Clinical Exec" ="#6BAED6", "Always Clinical Exec" = "#2C6B8E",
                                "Always Any Clinical Exec" = "#2C6B8E", 'Always Internal Medicine Exec' = "#A5C499"))  +
  annotate(geom="label", x=2013.1, y=22.5, 
           label=paste0("ATT (s.e.) = ", round(specialty_nomd_read_did,2)," (",round(specialty_nomd_read_se,2),")"), 
           size=4,
           fill="gray90") 
ggsave(specialty_nomd_read_plot, filename=paste0(objects_path,"read_specialty_nomd_synth_graph.pdf"), width=5.5, height=6, units="in")

# compare other docs med to no doctor
nospecialty_nomd_read_data <- specialty_data %>%
  filter(no_num_md_change_2010_2014==1) %>%
  filter((always_int_med==0 & ever_has_md==1) | ever_has_md==0) %>%
  mutate(did = post_2012*ever_has_md) %>%
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
nospecialty_nomd_read_panel <- panel.matrices(as.data.frame(nospecialty_nomd_read_data))
X_mat_nospecialty_nomd_read <- nospecialty_nomd_read_data %>%
  select(ID, year, cmiv) %>%
  melt(id.var=c("ID", "year")) %>%
  nest_by(variable) %>%
  mutate(X=list(dcast(data.table(nospecialty_nomd_read_data), ID~year) %>%
                  .[data.table(ID=rownames(nospecialty_nomd_read_panel$Y)), on="ID"] %>%
                  .[, ID := NULL] %>%
                  as.matrix()
  )) %>%
  .$X %>%
  abind(along=2)

nospecialty_nomd_read_did <- synthdid_estimate(nospecialty_nomd_read_panel$Y, nospecialty_nomd_read_panel$N0, nospecialty_nomd_read_panel$T0, X=X_mat_nospecialty_nomd_read)

sprintf('point estimate: %1.2f', nospecialty_nomd_read_did)
nospecialty_nomd_read_se = sqrt(vcov(nospecialty_nomd_read_did))

nospecialty_nomd_read_plot <- synthdid_plot(nospecialty_nomd_read_did, facet.vertical=FALSE,
                                          control.name='Never Clinical Exec', treated.name='Always Other Clinical Exec',
                                          lambda.comparable=TRUE, se.method = 'none',
                                          lambda.plot.scale = 0,
                                          trajectory.linetype = 1, line.width=.8, effect.curvature=-.4,
                                          trajectory.alpha=1, effect.alpha=0,
                                          diagram.alpha=0, onset.alpha=1) +
  theme(legend.position=c(.5,-.095), legend.direction='horizontal',
        legend.key=element_blank(), legend.background=element_blank(),
        strip.background=element_blank(), strip.text.x = element_blank(),
        text = element_text(size=18)) + ylim(19.5,22.5) +
  scale_color_manual(values = c("For-profit" = "#D65828", "Never Clinical Exec" = "#AED1EC", "Ever Clinical Exec" ="#6BAED6", "Always Clinical Exec" = "#2C6B8E",
                                "Always Any Clinical Exec" = "#2C6B8E", 'Always Other Clinical Exec' = "#A5C499"))  +
  annotate(geom="label", x=2013.1, y=22.5, 
           label=paste0("ATT (s.e.) = ", round(nospecialty_nomd_read_did,2)," (",round(nospecialty_nomd_read_se,2),")"), 
           size=4,
           fill="gray90") 
ggsave(nospecialty_nomd_read_plot, filename=paste0(objects_path,"read_nospecialty_nomd_synth_graph.pdf"), width=5.5, height=6, units="in")

# Mortality
# compare never int med with always int med
specialty_mort_data <- specialty_data %>%
  filter((always_int_med==1 & no_num_int_med_changes==1) | (never_int_med==1 & no_num_md_change_2010_2014)) %>%
  filter(num_doctors>0) %>%
  mutate(did = post_2012*always_int_med) %>%
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
specialty_mort_panel <- panel.matrices(as.data.frame(specialty_mort_data))
X_mat_specialty_mort <- specialty_mort_data %>%
  select(ID, year, cmiv) %>%
  melt(id.var=c("ID", "year")) %>%
  nest_by(variable) %>%
  mutate(X=list(dcast(data.table(specialty_mort_data), ID~year) %>%
                  .[data.table(ID=rownames(specialty_mort_panel$Y)), on="ID"] %>%
                  .[, ID := NULL] %>%
                  as.matrix()
  )) %>%
  .$X %>%
  abind(along=2)

specialty_mort_did <- synthdid_estimate(specialty_mort_panel$Y, specialty_mort_panel$N0, specialty_mort_panel$T0, X=X_mat_specialty_mort)

sprintf('point estimate: %1.2f', specialty_mort_did)
specialty_mort_se = sqrt(vcov(specialty_mort_did))

specialty_mort_plot <- synthdid_plot(specialty_mort_did, facet.vertical=FALSE,
                                     control.name='Other Clinical Exec', treated.name='Internal Med Exec',
                                     lambda.comparable=TRUE, se.method = 'none',
                                     lambda.plot.scale = 0,
                                     trajectory.linetype = 1, line.width=.8, effect.curvature=-.4,
                                     trajectory.alpha=1, effect.alpha=0,
                                     diagram.alpha=0, onset.alpha=1) +
  theme(legend.position=c(.5,-.095), legend.direction='horizontal',
        legend.key=element_blank(), legend.background=element_blank(),
        strip.background=element_blank(), strip.text.x = element_blank(),
        text = element_text(size=18)) + ylim(11,13) +
  scale_color_manual(values = c("For-profit" = "#D65828", "Never Clinical Exec" = "#AED1EC", "Ever Clinical Exec" ="#6BAED6", "Always Clinical Exec" = "#2C6B8E",
                                "Other Clinical Exec" = "#c6bfca", 'Internal Med Exec' = "#A5C499"))  +
  annotate(geom="label", x=2013.1, y=13, 
           label=paste0("ATT (s.e.) = ", round(specialty_mort_did,2)," (",round(specialty_mort_se,2),")"), 
           size=4,
           fill="gray90") 
ggsave(specialty_mort_plot, filename=paste0(objects_path,"mort_specialty_synth_graph.pdf"), width=5.5, height=6, units="in")

# compare always int med to no doctor
specialty_nomd_mort_data <- specialty_data %>%
  filter(always_int_med==1 | ever_has_md==0) %>%
  mutate(did = post_2012*always_int_med) %>%
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
specialty_nomd_mort_panel <- panel.matrices(as.data.frame(specialty_nomd_mort_data))
X_mat_specialty_nomd_mort <- specialty_nomd_mort_data %>%
  select(ID, year, cmiv) %>%
  melt(id.var=c("ID", "year")) %>%
  nest_by(variable) %>%
  mutate(X=list(dcast(data.table(specialty_nomd_mort_data), ID~year) %>%
                  .[data.table(ID=rownames(specialty_nomd_mort_panel$Y)), on="ID"] %>%
                  .[, ID := NULL] %>%
                  as.matrix()
  )) %>%
  .$X %>%
  abind(along=2)

specialty_nomd_mort_did <- synthdid_estimate(specialty_nomd_mort_panel$Y, specialty_nomd_mort_panel$N0, specialty_nomd_mort_panel$T0, X=X_mat_specialty_nomd_mort)

sprintf('point estimate: %1.2f', specialty_nomd_mort_did)
specialty_nomd_mort_se = sqrt(vcov(specialty_nomd_mort_did))

specialty_nomd_mort_plot <- synthdid_plot(specialty_nomd_mort_did, facet.vertical=FALSE,
                                          control.name='Never Clinical Exec', treated.name='Always Internal Medicine Exec',
                                          lambda.comparable=TRUE, se.method = 'none',
                                          lambda.plot.scale = 0,
                                          trajectory.linetype = 1, line.width=.8, effect.curvature=-.4,
                                          trajectory.alpha=1, effect.alpha=0,
                                          diagram.alpha=0, onset.alpha=1) +
  theme(legend.position=c(.5,-.095), legend.direction='horizontal',
        legend.key=element_blank(), legend.background=element_blank(),
        strip.background=element_blank(), strip.text.x = element_blank(),
        text = element_text(size=18)) + ylim(11,13) +
  scale_color_manual(values = c("For-profit" = "#D65828", "Never Clinical Exec" = "#AED1EC", "Ever Clinical Exec" ="#6BAED6", "Always Clinical Exec" = "#2C6B8E",
                                "Always Any Clinical Exec" = "#2C6B8E", 'Always Internal Medicine Exec' = "#A5C499"))  +
  annotate(geom="label", x=2013.1, y=13, 
           label=paste0("ATT (s.e.) = ", round(specialty_nomd_mort_did,2)," (",round(specialty_nomd_mort_se,2),")"), 
           size=4,
           fill="gray90") 
ggsave(specialty_nomd_mort_plot, filename=paste0(objects_path,"mort_specialty_nomd_synth_graph.pdf"), width=5.5, height=6, units="in")

# compare other docs med to no doctor
nospecialty_nomd_mort_data <- specialty_data %>%
  filter(no_num_md_change_2010_2014==1) %>%
  filter((always_int_med==0 & ever_has_md==1) | ever_has_md==0) %>%
  mutate(did = post_2012*ever_has_md) %>%
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
nospecialty_nomd_mort_panel <- panel.matrices(as.data.frame(nospecialty_nomd_mort_data))
X_mat_nospecialty_nomd_mort <- nospecialty_nomd_mort_data %>%
  select(ID, year, cmiv) %>%
  melt(id.var=c("ID", "year")) %>%
  nest_by(variable) %>%
  mutate(X=list(dcast(data.table(nospecialty_nomd_mort_data), ID~year) %>%
                  .[data.table(ID=rownames(nospecialty_nomd_mort_panel$Y)), on="ID"] %>%
                  .[, ID := NULL] %>%
                  as.matrix()
  )) %>%
  .$X %>%
  abind(along=2)

nospecialty_nomd_mort_did <- synthdid_estimate(nospecialty_nomd_mort_panel$Y, nospecialty_nomd_mort_panel$N0, nospecialty_nomd_mort_panel$T0, X=X_mat_nospecialty_nomd_mort)

sprintf('point estimate: %1.2f', nospecialty_nomd_mort_did)
nospecialty_nomd_mort_se = sqrt(vcov(nospecialty_nomd_mort_did))

nospecialty_nomd_mort_plot <- synthdid_plot(nospecialty_nomd_mort_did, facet.vertical=FALSE,
                                            control.name='Never Clinical Exec', treated.name='Always Other Clinical Exec',
                                            lambda.comparable=TRUE, se.method = 'none',
                                            lambda.plot.scale = 0,
                                            trajectory.linetype = 1, line.width=.8, effect.curvature=-.4,
                                            trajectory.alpha=1, effect.alpha=0,
                                            diagram.alpha=0, onset.alpha=1) +
  theme(legend.position=c(.5,-.095), legend.direction='horizontal',
        legend.key=element_blank(), legend.background=element_blank(),
        strip.background=element_blank(), strip.text.x = element_blank(),
        text = element_text(size=18)) + ylim(11,13) +
  scale_color_manual(values = c("For-profit" = "#D65828", "Never Clinical Exec" = "#AED1EC", "Ever Clinical Exec" ="#6BAED6", "Always Clinical Exec" = "#2C6B8E",
                                "Always Any Clinical Exec" = "#2C6B8E", 'Always Other Clinical Exec' = "#A5C499"))  +
  annotate(geom="label", x=2013.1, y=13, 
           label=paste0("ATT (s.e.) = ", round(nospecialty_nomd_mort_did,2)," (",round(nospecialty_nomd_mort_se,2),")"), 
           size=4,
           fill="gray90") 
ggsave(nospecialty_nomd_mort_plot, filename=paste0(objects_path,"mort_nospecialty_nomd_synth_graph.pdf"), width=5.5, height=6, units="in")











