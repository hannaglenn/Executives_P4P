library(readr)
library(dplyr)
library(fixest)
library(ggplot2)
library(ggpubr)
library(tidyr)

# Read in hospital_data created in "create_hospital_data.R"
hospital_data <- readRDS(paste0(created_data_path, "all_hospital_data.rds")) %>%
  mutate(yr2009 = ifelse(year==2009,1,0),
         yr2010 = ifelse(year==2010,1,0),
         yr2011 = ifelse(year==2011,1,0),
         yr2012 = ifelse(year==2012,1,0),
         yr2013 = ifelse(year==2013,1,0),
         yr2014 = ifelse(year==2014,1,0),
         yr2015 = ifelse(year==2015,1,0)) %>%
  rename(heartattack_mortality = rate_heartattack_mortality,
         heartattack_readmission = rate_heartattack_readmission,
         heartfailure_mortality = rate_heartfailure_mortality,
         heartfailure_readmission = rate_heartfailure_readmission,
         pneum_mortality = rate_pneum_mortality,
         pneum_readmission = rate_pneum_readmission) %>%
  mutate(post_2012 = ifelse(year>=2012, 1, 0)) %>%
  mutate(post_2011 = ifelse(year>=2011, 1, 0)) %>%
  mutate(has_any_md = ifelse(num_doctors>0, 1, 0)) %>%
  mutate(did = has_any_md*post_2012) %>%
  mutate(did_2011 = has_any_md*post_2011) %>%
  mutate(es2009 = yr2009*has_any_md,
         es2010 = yr2010*has_any_md,
         es2011 = yr2011*has_any_md,
         es2012 = yr2012*has_any_md,
         es2013 = yr2013*has_any_md,
         es2014 = yr2014*has_any_md,
         es2015 = yr2015*has_any_md)

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

# create sample of no md changes for all hospitals
noMDchg_all_sample <- hospital_data %>%
  filter(no_md_change_2010_2014==1) %>%
  mutate(post = ifelse(year>=minyr_pen, 1, 0))


# Create the 7 samples I want ####

# 1. No MD changes, penalized
noMDchg_pen_sample <- hospital_data %>%
  filter(no_md_changes_2010_2014==1) %>%
  filter(ever_penalized==1) %>%
  select(ID, year, heartattack_mortality, heartattack_readmission, pneum_mortality, pneum_readmission, heartfailure_mortality, heartfailure_readmission, has_any_md, minyr_pen, yr2009, yr2010, yr2011, yr2012, yr2013, yr2014, yr2015, weightedavg_read, weightedavg_mort, post_2012, did, did_2011,
         es2009, es2010, es2011, es2012, es2013, es2014, es2015) %>%
  mutate(post = ifelse(year>=minyr_pen, 1, 0))

# 2. No MD changes, penalized for HF
noMDchg_hfpen_sample <- hospital_data %>%
  filter(no_md_changes_2010_2014==1) %>%
  filter(ever_pen_hf>0) %>%
  mutate(has_any_md = ifelse(total_docs>0, 1, 0)) %>%
  select(ID, year, heartattack_mortality, heartattack_readmission, pneum_mortality, pneum_readmission, heartfailure_mortality, heartfailure_readmission, has_any_md, minyr_pen, yr2009, yr2010, yr2011, yr2012, yr2013, yr2014, yr2015, weightedavg_read, weightedavg_mort, post_2012, did, did_2011) %>%
  mutate(post = ifelse(year>=minyr_pen, 1, 0))

# 3. No MD changes, penalized for HA
noMDchg_hapen_sample <- hospital_data %>%
  filter(no_md_changes_2010_2014==1) %>%
  filter(ever_pen_ha>0) %>%
  mutate(has_any_md = ifelse(total_docs>0, 1, 0)) %>%
  select(ID, year, heartattack_mortality, heartattack_readmission, pneum_mortality, pneum_readmission, heartfailure_mortality, heartfailure_readmission, has_any_md, minyr_pen, yr2009, yr2010, yr2011, yr2012, yr2013, yr2014, yr2015, weightedavg_read, weightedavg_mort, post_2012, did, did_2011) %>%
  mutate(post = ifelse(year>=minyr_pen, 1, 0))

# 4. No MD changes, penalized for pnem
noMDchg_pnempen_sample <- hospital_data %>%
  filter(no_md_changes_2010_2014==1) %>%
  filter(ever_pen_pnem>0) %>%
  mutate(has_any_md = ifelse(total_docs>0, 1, 0)) %>%
  select(ID, year, heartattack_mortality, heartattack_readmission, pneum_mortality, pneum_readmission, heartfailure_mortality, heartfailure_readmission, has_any_md, minyr_pen, yr2009, yr2010, yr2011, yr2012, yr2013, yr2014, yr2015, weightedavg_read, weightedavg_mort, post_2012, did, did_2011) %>%
  mutate(post = ifelse(year>=minyr_pen, 1, 0))

# 5. No MD changes, bottom tercile for excess readmissions
noMDchg_bottom_sample <- hospital_data %>%
  filter(no_md_changes_2010_2014==1) %>%
  filter(rate_tercile==1) %>%
  mutate(has_any_md = ifelse(total_docs>0, 1, 0)) %>%
  select(ID, year, heartattack_mortality, heartattack_readmission, pneum_mortality, pneum_readmission, heartfailure_mortality, heartfailure_readmission, has_any_md, minyr_pen, yr2009, yr2010, yr2011, yr2012, yr2013, yr2014, yr2015, weightedavg_read, weightedavg_mort, post_2012, did, did_2011) %>%
  mutate(post = ifelse(year>=minyr_pen, 1, 0))

# 6. No MD changes, middle tercile for excess readmissions
noMDchg_middle_sample <- hospital_data %>%
  filter(no_md_changes_2010_2014==1) %>%
  filter(rate_tercile==2) %>%
  mutate(has_any_md = ifelse(total_docs>0, 1, 0)) %>%
  select(ID, year, heartattack_mortality, heartattack_readmission, pneum_mortality, pneum_readmission, heartfailure_mortality, heartfailure_readmission, has_any_md, minyr_pen, yr2009, yr2010, yr2011, yr2012, yr2013, yr2014, yr2015, weightedavg_read, weightedavg_mort, post_2012, did, did_2011) %>%
  mutate(post = ifelse(year>=minyr_pen, 1, 0))

# 7. No MD changes, top tercile for excess readmissions
noMDchg_top_sample <- hospital_data %>%
  filter(no_md_changes_2010_2014==1) %>%
  filter(rate_tercile==3) %>%
  mutate(has_any_md = ifelse(total_docs>0, 1, 0)) %>%
  select(ID, year, heartattack_mortality, heartattack_readmission, pneum_mortality, pneum_readmission, heartfailure_mortality, heartfailure_readmission, has_any_md, minyr_pen, yr2009, yr2010, yr2011, yr2012, yr2013, yr2014, yr2015, weightedavg_read, weightedavg_mort, post_2012, did, did_2011) %>%
  mutate(post = ifelse(year>=minyr_pen, 1, 0))

# 8. No MD changes, matched hospitals
noMDchg_matched_sample <- hospital_data %>%
  filter(no_md_changes_2010_2014==1) %>%
  filter(matched==1) %>%
  mutate(has_any_md = ifelse(total_docs>0, 1, 0)) %>%
  select(ID, year, heartattack_mortality, heartattack_readmission, pneum_mortality, pneum_readmission, heartfailure_mortality, heartfailure_readmission, has_any_md, minyr_pen, yr2009, yr2010, yr2011, yr2012, yr2013, yr2014, yr2015, weightedavg_read, weightedavg_mort, post_2012, did, did_2011) %>%
  mutate(post = ifelse(year>=minyr_pen, 1, 0))


samples_list <- list(noMDchg_all_sample, noMDchg_pen_sample, noMDchg_matched_sample,
                     noMDchg_hfpen_sample, noMDchg_hapen_sample,
                     noMDchg_pnempen_sample, noMDchg_top_sample, noMDchg_middle_sample, noMDchg_bottom_sample)

samples_list <- list(noMDchg_all_sample)

# Create Descriptive Graphs for Paper ################
# 1. weighted average readmissions and mortality on same graph
weighted_read_graph_data <- noMDchg_all_sample %>%
  group_by(has_any_md, year) %>%
  summarise_at(c("heartattack_readmission", "heartfailure_readmission", "pneum_readmission"), list(mean), na.rm=T) %>%
  filter(year!=2008) %>%
  mutate(has_any_md = as.factor(has_any_md)) %>%
  pivot_longer(c("heartattack_readmission", "heartfailure_readmission", "pneum_readmission"), names_to = "outcome", values_to = "rate") %>%
  mutate(outcome = ifelse(outcome=="weightedavg_read", "weighted average", outcome)) %>%
  mutate(outcome = ifelse(outcome=="heartattack_readmission", "AMI", outcome)) %>%
  mutate(outcome = ifelse(outcome=="heartfailure_readmission", "heart failure", outcome)) %>%
  mutate(outcome = ifelse(outcome=="pneum_readmission", "pneumonia", outcome)) %>%
  rename(`clinical experience`=has_any_md, `category`=outcome)
weighted_read_graph <- ggplot(weighted_read_graph_data, aes(x=year, y=rate, color=category, linetype=`clinical experience`)) + geom_vline(xintercept = 2012, linetype = "dotted") + 
  geom_line(size=.75) + theme_bw() + xlab("\nyear") + ggtitle("readmission") +
  ylim(10,25) + theme(text=element_text(size=18)) + scale_color_brewer(palette="Set2")

weighted_mort_graph_data <- noMDchg_all_sample %>%
  group_by(has_any_md, year) %>%
  summarise_at(c("heartattack_mortality", "heartfailure_mortality", "pneum_mortality"), list(mean), na.rm=T) %>%
  filter(year!=2008) %>%
  mutate(has_any_md = as.factor(has_any_md)) %>%
  pivot_longer(c("heartattack_mortality", "heartfailure_mortality", "pneum_mortality"), names_to = "outcome", values_to = "rate") %>%
  mutate(outcome = ifelse(outcome=="weightedavg_mort", "weighted average", outcome)) %>%
  mutate(outcome = ifelse(outcome=="heartattack_mortality", "AMI", outcome)) %>%
  mutate(outcome = ifelse(outcome=="heartfailure_mortality", "heart failure", outcome)) %>%
  mutate(outcome = ifelse(outcome=="pneum_mortality", "pneumonia", outcome)) %>%
  rename(`clinical experience`=has_any_md, `category`=outcome)
weighted_mort_graph <- ggplot(weighted_mort_graph_data, aes(x=year, y=rate, color=category, linetype=`clinical experience`)) + geom_vline(xintercept = 2012, linetype = "dotted") + 
  geom_line(size=.75) + theme_bw() + xlab("\nyear") + ggtitle("mortality") +
  ylim(10,25) + theme(text=element_text(size=18)) + scale_color_brewer(palette="Set2")

plot <- ggarrange(weighted_read_graph, weighted_mort_graph, nrow = 1,
                  common.legend = TRUE,
                  legend = "right")

ggsave(plot = plot, filename = paste0(objects_path, "weighted_read_mort_graph.pdf"), width = 14, height = 6, units = "in")


# Create tables for paper #########

# table just looking at the effect of the policy on all hospitals
policy_tables_read <- lapply(samples_list, function(x){
  all <- fixest::feols(weightedavg_read ~ post_2012 | ID, cluster = ~ID, data=x)
  pnem <- fixest::feols(pneum_readmission ~ post_2012 | ID, cluster = ~ID,data=x)
  hf <- fixest::feols(heartfailure_readmission ~ post_2012 | ID, cluster = ~ID,data=x)
  ami <- fixest::feols(heartattack_readmission ~ post_2012 | ID, cluster = ~ID,data=x)
  
  table <- etable(all, pnem, hf, ami, tex = TRUE,
                  dict = c("weightedavg_read"="Weighted Avg.", "pneum_readmission" = "Pneumonia", "heartfailure_readmission" = "Heart Failure",
                           "heartattack_readmission" = "AMI", "has_any_md" = "Clinical Experience Team", "post_2012" = "Post Penalty",
                           "did" = "Clinical Experience Team x Post Penalty"),
                  style.tex = style.tex("aer"), fitstat = ~n,
                  drop = "Intercept",
                  digits = "r2")
})

policy_tables_mort <- lapply(samples_list, function(x){
  all <- fixest::feols(weightedavg_mort ~ post_2012 + yr2009 + yr2010 + yr2013 + yr2014 + yr2015 | ID, cluster = ~ID, data=x)
  pnem <- fixest::feols(pneum_mortality ~ post_2012 + yr2009 + yr2010 + yr2013 + yr2014 + yr2015 | ID, cluster = ~ID,data=x)
  hf <- fixest::feols(heartfailure_mortality ~ post_2012 + yr2009 + yr2010 + yr2013 + yr2014 + yr2015 | ID, cluster = ~ID,data=x)
  ami <- fixest::feols(heartattack_mortality ~ post_2012 + yr2009 + yr2010 + yr2013 + yr2014 + yr2015 | ID, cluster = ~ID,data=x)
  
  table <- etable(all, pnem, hf, ami, tex = TRUE,
                  dict = c("weightedavg_mort"="Weighted Avg.", "pneum_mortality" = "Pneumonia", "heartfailure_mortality" = "Heart Failure",
                           "heartattack_mortality" = "AMI", "has_any_md" = "Clinical Experience Team", "post_2012" = "Post Penalty",
                           "did" = "Clinical Experience Team x Post Penalty"),
                  style.tex = style.tex("aer"), fitstat = ~n,
                  drop = c("Intercept", "yr2009", "yr2010", "yr2013", "yr2014", "yr2015"),
                  digits = "r2")
})

# create tables for readmissions
read_tables <- lapply(samples_list, function(x){
  all <- fixest::feols(weightedavg_read ~ did | ID + year, cluster = ~ID, data=x)
  pnem <- fixest::feols(pneum_readmission ~ did | ID + year, cluster = ~ID,data=x)
  hf <- fixest::feols(heartfailure_readmission ~ did | ID + year, cluster = ~ID,data=x)
  ami <- fixest::feols(heartattack_readmission ~ did | ID + year, cluster = ~ID,data=x)
  
  table <- etable(all, pnem, hf, ami, tex = TRUE,
                  dict = c("weightedavg_read"="Weighted Avg.", "pneum_readmission" = "Pneumonia", "heartfailure_readmission" = "Heart Failure",
                           "heartattack_readmission" = "AMI", "has_any_md" = "Clinical Experience Team", "post_2012" = "Post Penalty",
                           "did" = "Clinical Experience Team x Post Penalty"),
                  style.tex = style.tex("aer"), fitstat = ~n,
                  drop = "Intercept",
                  digits = "r2")
})

# create tables for mortality
mort_tables <- lapply(samples_list, function(x){
  all <- fixest::feols(weightedavg_mort ~ did | ID + year, cluster = ~ID, data=x)
  pnem <- fixest::feols(pneum_mortality ~ did | ID + year, cluster = ~ID,data=x)
  hf <- fixest::feols(heartfailure_mortality ~ did | ID + year, cluster = ~ID,data=x)
  ami <- fixest::feols(heartattack_mortality ~ did | ID + year, cluster = ~ID,data=x)
  
  table <- etable(all, pnem, hf, ami, tex = TRUE,
                  dict = c("weightedavg_mort"="Weighted Avg.", "pneum_mortality" = "Pneumonia", "heartfailure_mortality" = "Heart Failure",
                           "heartattack_mortality" = "AMI", "has_any_md" = "Clinical Experience Team", "post_2012" = "Post Penalty",
                           "did" = "Clinical Experience Team x Post Penalty"),
                  style.tex = style.tex("aer"), fitstat = ~n,
                  drop = "Intercept",
                  digits = "r2")
})



# table for readmissions, all hospitals
noMDchg_all_wa_read_did <- fixest::feols(weightedavg_read ~ did | ID + year, cluster = ~ID, data=noMDchg_all_sample)
noMDchg_pnem_read_did <- fixest::feols(pneum_readmission ~ did | ID + year, cluster = ~ID,data=noMDchg_all_sample)
noMDchg_hf_read_did <- fixest::feols(heartfailure_readmission ~ did | ID + year, cluster = ~ID,data=noMDchg_all_sample)
noMDchg_ha_read_did <- fixest::feols(heartattack_readmission ~ did | ID + year, cluster = ~ID,data=noMDchg_all_sample)
etable(noMDchg_all_wa_read_did, noMDchg_pnem_read_did, noMDchg_hf_read_did, noMDchg_ha_read_did)
etable(noMDchg_all_wa_read_did, noMDchg_pnem_read_did, noMDchg_hf_read_did, noMDchg_ha_read_did, tex = TRUE,
       dict = c("weightedavg_read"="Weighted Avg.", "pneum_readmission" = "Pneumonia", "heartfailure_readmission" = "Heart Failure",
                "heartattack_readmission" = "AMI", "has_any_md" = "Clinical Experience Team", "post_2012" = "Post Penalty",
                "did" = "Clinical Experience Team x Post Penalty"),
       style.tex = style.tex("aer"), fitstat = ~n,
       drop = "Intercept",
       digits = "r2")

# table for readmissions, penalized hospitals 
noMDchg_pen_wa_read_did <- fixest::feols(weightedavg_read ~ did | ID + year, cluster = ~ID, data=noMDchg_pen_sample)
noMDchg_pneumpen_pnem_read_did <- fixest::feols(pneum_readmission ~ did | ID + year, cluster = ~ID,data=noMDchg_pnempen_sample)
noMDchg_hfpen_hf_read_did <- fixest::feols(heartfailure_readmission ~ did | ID + year, cluster = ~ID,data=noMDchg_hfpen_sample)
noMDchg_hapen_ha_read_did <- fixest::feols(heartattack_readmission ~ did | ID + year, cluster = ~ID,data=noMDchg_hapen_sample)
etable(noMDchg_pen_wa_read_did, noMDchg_pneumpen_pnem_read_did, noMDchg_hfpen_hf_read_did, noMDchg_hapen_ha_read_did)
etable(noMDchg_pen_wa_read_did, noMDchg_pneumpen_pnem_read_did, noMDchg_hfpen_hf_read_did, noMDchg_hapen_ha_read_did, tex = TRUE,
       dict = c("weightedavg_read"="Weighted Avg.", "pneum_readmission" = "Pneumonia", "heartfailure_readmission" = "Heart Failure",
                "heartattack_readmission" = "AMI", "has_any_md" = "Clinical Experience Team", "post_2012" = "Post Penalty",
                "did" = "Clinical Experience Team x Post Penalty"),
       style.tex = style.tex("aer"), fitstat = ~n,
       drop = "Intercept",
       extraline = list("All Penalized" = c("$\\checkmark$", "", "", ""),
                        "Penalized for Pneum." = c("", "$\\checkmark$", "", ""),
                        "Penalized for HF" = c("", "", "$\\checkmark$", ""),
                        "Penalized for AMI" = c("", "", "", "$\\checkmark$")),
       digits = "r2")


# table for mortality, all hospitals
noMDchg_all_wa_mort_did <- fixest::feols(weightedavg_mort ~ did | ID + year, cluster = ~ID, data=noMDchg_all_sample)
noMDchg_pnem_mort_did <- fixest::feols(pneum_mortality ~ did | ID + year, cluster = ~ID,data=noMDchg_all_sample)
noMDchg_hf_mort_did <- fixest::feols(heartfailure_mortality ~ did | ID + year, cluster = ~ID,data=noMDchg_all_sample)
noMDchg_ha_mort_did <- fixest::feols(heartattack_mortality ~ did | ID + year, cluster = ~ID,data=noMDchg_all_sample)
etable(noMDchg_all_wa_mort_did, noMDchg_pnem_mort_did, noMDchg_hf_mort_did, noMDchg_ha_mort_did)
etable(noMDchg_all_wa_mort_did, noMDchg_pnem_mort_did, noMDchg_hf_mort_did, noMDchg_ha_mort_did, tex = TRUE,
       dict = c("weightedavg_read"="Weighted Avg.", "pneum_readmission" = "Pneumonia", "heartfailure_readmission" = "Heart Failure",
                "heartattack_readmission" = "AMI", "has_any_md" = "Clinical Experience Team", "post_2012" = "Post Penalty",
                "did" = "Clinical Experience Team x Post Penalty"),
       style.tex = style.tex("aer"), fitstat = ~n,
       drop = "Intercept",
       digits = "r2")

# table for mortality, penalized hospitals
noMDchg_pen_wa_mort_did <- fixest::feols(weightedavg_mort ~ did | ID + year, cluster = ~ID, data=noMDchg_pen_sample)
noMDchg_pneumpen_pnem_mort_did <- fixest::feols(pneum_mortality ~ did | ID + year, cluster = ~ID, data=noMDchg_pnempen_sample)
noMDchg_hfpen_hf_mort_did <- fixest::feols(heartfailure_mortality ~ did | ID + year, cluster = ~ID, data=noMDchg_hfpen_sample)
noMDchg_hapen_ha_mort_did <- fixest::feols(heartattack_mortality ~ did | ID + year, cluster = ~ID, data=noMDchg_hapen_sample)
etable(noMDchg_pen_wa_mort_did, noMDchg_pneumpen_pnem_mort_did, noMDchg_hfpen_hf_mort_did, noMDchg_hapen_ha_mort_did, tex = TRUE,
       dict = c("weightedavg_mort"="Weighted Avg.", "pneum_mortality" = "Pneumonia", "heartfailure_mortality" = "Heart Failure",
                "heartattack_mortality" = "AMI", "has_any_md" = "Clinical Experience Team", "post_2012" = "Post Penalty",
                "did"="Clinical Experience Team x Post Penalty"),
       style.tex = style.tex("aer"), fitstat = ~n,
       drop = "Intercept",
       extraline = list("All Penalized" = c("$\\checkmark$", "", "", ""),
                        "Penalized for Pneum." = c("", "$\\checkmark$", "", ""),
                        "Penalized for HF" = c("", "", "$\\checkmark$", ""),
                        "Penalized for AMI" = c("", "", "", "$\\checkmark$")))


# create event study graph for paper ########
# weighted average readmissions
noMDchg_pen_wa_read_es <- fixest::feols(weightedavg_read ~ es2009 + es2010 + es2012 + es2013 + es2014 + es2015 | ID + year, cluster = ~ID,
                                        data = noMDchg_pen_sample)

data <- as.data.frame(list(vars = c("2009", "2010", "2012", "2013", "2014", "2015"), 
                           Estimate = noMDchg_pen_wa_read_es$coefficients, se = noMDchg_pen_wa_read_es$se)) %>%
  add_row(vars = "2011", Estimate = 0, se = 0) %>%
  mutate(vars=factor(vars,levels=c("2009", "2010", "2011", "2012", "2013", "2014", "2015"))) %>%
  mutate(category=ifelse(vars=="2009"|vars=="2010"|vars=="2011","Pre","Post")) %>%
  mutate(se=as.numeric(se))

read_es <- ggplot(data, aes(vars, Estimate)) + 
  geom_hline(yintercept=0, lty=2, lwd=.5, colour="black") +
  geom_errorbar(aes(ymin=Estimate - 1.96*se, ymax=Estimate + 1.96*se, color=category), 
                lwd=.5, width=.18) +
  geom_point(aes(fill=category, color=category), size=1.5, pch=21) +
  theme_bw() + theme(text=element_text(size=18),
                     legend.title = element_blank()) + xlab("\nyear") +
  ylab("Estimate and 95% CI") +
  scale_color_manual(labels = c("Post", "Pre"), values=c("#E69F00", "#999999"),name="") +
  scale_fill_manual(labels = c("Post", "Pre"), values=c("#E69F00", "#999999"),name="") + ylim(-1,1)
ggsave(read_es, filename = paste0(objects_path, "read_es_graph.pdf"), width = 11, height = 6, units = "in")

# weighted average mortality
noMDchg_pen_wa_mort_es <- fixest::feols(weightedavg_mort ~ es2009 + es2010 + es2012 + es2013 + es2014 + es2015 | ID + year, cluster = ~ID,
                                        data = noMDchg_pen_sample)

data <- as.data.frame(list(vars = c("2009", "2010", "2012", "2013", "2014", "2015"), 
                           Estimate = noMDchg_pen_wa_mort_es$coefficients, se = noMDchg_pen_wa_mort_es$se)) %>%
  add_row(vars = "2011", Estimate = 0, se = 0) %>%
  mutate(vars=factor(vars,levels=c("2009", "2010", "2011", "2012", "2013", "2014", "2015"))) %>%
  mutate(category=ifelse(vars=="2009"|vars=="2010"|vars=="2011","Pre","Post")) %>%
  mutate(se=as.numeric(se))

mort_es <- ggplot(data, aes(vars, Estimate)) + 
  geom_hline(yintercept=0, lty=2, lwd=.5, colour="black") +
  geom_errorbar(aes(ymin=Estimate - 1.96*se, ymax=Estimate + 1.96*se, color=category), 
                lwd=.5, width=.18) +
  geom_point(aes(fill=category, color=category), size=1.5, pch=21) +
  theme_bw() + theme(text=element_text(size=18),
                     legend.title = element_blank()) + xlab("\nyear") +
  ylab(" ") +
  scale_color_manual(labels = c("Post", "Pre"), values=c("#E69F00", "#999999"),name="") +
  scale_fill_manual(labels = c("Post", "Pre"), values=c("#E69F00", "#999999"),name="") + ylim(-1,1) + ggtitle("Weighted Average Mortality Rate")

plot <- ggarrange(read_es, mort_es, nrow = 1,
                  common.legend = TRUE, legend = "right")

ggsave(plot = mort_es, filename = paste0(objects_path, "mort_es_graph.pdf"), width = 11, height = 6, units = "in")

