library(readr)
library(dplyr)
library(fixest)
library(ggplot2)
library(ggpubr)
library(tidyr)

# Read in hospital_data created in "create_hospital_data.R"
hospital_data <- readRDS(paste0(created_data_path, "penalized_hospital_data(temp).rds")) %>%
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
  mutate(has_any_md = ifelse(total_docs>0, 1, 0)) %>%
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


# Create the 7 samples I want ####

# 1. No MD changes, penalized
noMDchg_pen_sample <- hospital_data %>%
  filter(no_md_changes_2010_2014==1) %>%
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





# Create Graphs for Paper ################
# 1. weighted average readmissions and mortality on same graph
weighted_read_mort_graph_data <- noMDchg_pen_sample %>%
  group_by(has_any_md, year) %>%
  summarise_at(c("weightedavg_read", "weightedavg_mort"), list(mean), na.rm=T) %>%
  filter(year!=2008) %>%
  mutate(has_any_md = as.factor(has_any_md)) %>%
  pivot_longer(c("weightedavg_read", "weightedavg_mort"), names_to = "outcome", values_to = "rate") %>%
  mutate(outcome = ifelse(outcome=="weightedavg_mort", "mortality", "readmission")) %>%
  rename(`clinical experience`=has_any_md)
weighted_read_mort_graph <- ggplot(weighted_read_mort_graph_data, aes(x=year, y=rate, color=outcome, linetype=`clinical experience`)) + geom_vline(xintercept = 2012, linetype = "dotted") + 
  geom_line(size=.75) + theme_bw() + 
  ylim(11,23) + theme(text=element_text(size=18))
ggsave(plot = weighted_read_mort_graph, filename = paste0(objects_path, "weighted_read_mort_graph.pdf"), width = 9, height = 6, units = "in")


# Create tables for paper #########

# table for readmissions
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




# table for mortality
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
  theme_bw() + theme(text=element_text(size=10),
                     legend.title = element_blank()) + xlab("Event Time") +
  ylab("Estimate and 95% CI") +
  scale_color_manual(labels = c("Post", "Pre"), values=c("#E69F00", "#999999"),name="") +
  scale_fill_manual(labels = c("Post", "Pre"), values=c("#E69F00", "#999999"),name="") + ylim(-1,1) + ggtitle("Weighted Average Readmission Rate")

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
  theme_bw() + theme(text=element_text(size=10),
                     legend.title = element_blank()) + xlab("Event Time") +
  ylab("Estimate and 95% CI") +
  scale_color_manual(labels = c("Post", "Pre"), values=c("#E69F00", "#999999"),name="") +
  scale_fill_manual(labels = c("Post", "Pre"), values=c("#E69F00", "#999999"),name="") + ylim(-1,1) + ggtitle("Weighted Average Mortality Rate")

plot <- ggarrange(read_es, mort_es, nrow = 1,
                  common.legend = TRUE, legend = "right")

ggsave(plot, filename = paste0(objects_path, "wa_eventstudy.pdf"), width = 9, height = 6, units = "in")

# Create Graphs for each Sample ##################

#1 No MD change, penalized
noMDchg_pen_graph_data <- noMDchg_pen_sample %>%
  group_by(has_any_md, year) %>%
  summarise_at(c("heartattack_mortality", "heartattack_readmission", "pneum_mortality", "pneum_readmission", "heartfailure_mortality", "heartfailure_readmission", "weightedavg_read", "weightedavg_mort"), list(mean), na.rm=T) %>%
  filter(year!=2008) %>%
  mutate(has_any_md = as.factor(has_any_md))
noMDchg_pen_read_graph_data <- pivot_longer(noMDchg_pen_graph_data, c("heartattack_mortality", "heartattack_readmission", "pneum_mortality", "pneum_readmission", "heartfailure_mortality", "heartfailure_readmission", "weightedavg_read", "weightedavg_mort"), names_to = "outcome", values_to = "rate") %>%
  filter(outcome %in% c("pneum_readmission", "heartfailure_readmission", "heartattack_readmission", "weightedavg_read"))
noMDchg_pen_mort_graph_data <- pivot_longer(noMDchg_pen_graph_data, c("heartattack_mortality", "heartattack_readmission", "pneum_mortality", "pneum_readmission", "heartfailure_mortality", "heartfailure_readmission", "weightedavg_read", "weightedavg_mort"), names_to = "outcome", values_to = "rate") %>%
  filter(outcome %in% c("pneum_mortality", "heartfailure_mortality", "heartattack_mortality", "weightedavg_mort"))

noMDchg_pen_read_graph <- ggplot(noMDchg_pen_read_graph_data, aes(x=year, y=rate, color=outcome, linetype=has_any_md)) + geom_vline(xintercept = 2012) + geom_line(size=.75) + theme_bw() + 
  ylim(15,30) + ggtitle("all penalized hospitals")

noMDchg_pen_mort_graph <- ggplot(noMDchg_pen_mort_graph_data, aes(x=year, y=rate, color=outcome, linetype=has_any_md)) + geom_vline(xintercept = 2012) + geom_line(size=.75) + theme_bw() + 
  ylim(0,20) + ggtitle("all penalized hospitals")



#2 No MD change, penalized for HF
noMDchg_hfpen_graph_data <- noMDchg_hfpen_sample %>%
  group_by(has_any_md, year) %>%
  summarise_at(c("heartattack_mortality", "heartattack_readmission", "pneum_mortality", "pneum_readmission", "heartfailure_mortality", "heartfailure_readmission"), list(mean), na.rm=T) %>%
  filter(year!=2008) %>%
  mutate(has_any_md = as.factor(has_any_md))
noMDchg_hfpen_read_graph_data <- pivot_longer(noMDchg_hfpen_graph_data, c("heartattack_mortality", "heartattack_readmission", "pneum_mortality", "pneum_readmission", "heartfailure_mortality", "heartfailure_readmission"), names_to = "outcome", values_to = "rate") %>%
  filter(outcome %in% c("pneum_readmission", "heartfailure_readmission", "heartattack_readmission"))
noMDchg_hfpen_mort_graph_data <- pivot_longer(noMDchg_hfpen_graph_data, c("heartattack_mortality", "heartattack_readmission", "pneum_mortality", "pneum_readmission", "heartfailure_mortality", "heartfailure_readmission"), names_to = "outcome", values_to = "rate") %>%
  filter(outcome %in% c("pneum_mortality", "heartfailure_mortality", "heartattack_mortality"))

noMDchg_hfpen_read_graph <- ggplot(noMDchg_hfpen_read_graph_data, aes(x=year, y=rate, color=outcome, linetype=has_any_md)) + geom_vline(xintercept = 2012) + geom_line(size=.75) + theme_bw() + 
  ylim(15,30) + ggtitle("hospitals penalized for heart failure")

noMDchg_hfpen_mort_graph <- ggplot(noMDchg_hfpen_mort_graph_data, aes(x=year, y=rate, color=outcome, linetype=has_any_md)) + geom_vline(xintercept = 2012) + geom_line(size=.75) + theme_bw() + 
  ylim(0,20) + ggtitle("hospitals penalized for heart failure")

#3 No MD change, penalized for HA
noMDchg_hapen_graph_data <- noMDchg_hapen_sample %>%
  group_by(has_any_md, year) %>%
  summarise_at(c("heartattack_mortality", "heartattack_readmission", "pneum_mortality", "pneum_readmission", "heartfailure_mortality", "heartfailure_readmission"), list(mean), na.rm=T) %>%
  filter(year!=2008) %>%
  mutate(has_any_md = as.factor(has_any_md))
noMDchg_hapen_read_graph_data <- pivot_longer(noMDchg_hapen_graph_data, c("heartattack_mortality", "heartattack_readmission", "pneum_mortality", "pneum_readmission", "heartfailure_mortality", "heartfailure_readmission"), names_to = "outcome", values_to = "rate") %>%
  filter(outcome %in% c("pneum_readmission", "heartfailure_readmission", "heartattack_readmission"))
noMDchg_hapen_mort_graph_data <- pivot_longer(noMDchg_hapen_graph_data, c("heartattack_mortality", "heartattack_readmission", "pneum_mortality", "pneum_readmission", "heartfailure_mortality", "heartfailure_readmission"), names_to = "outcome", values_to = "rate") %>%
  filter(outcome %in% c("pneum_mortality", "heartfailure_mortality", "heartattack_mortality"))

noMDchg_hapen_read_graph <- ggplot(noMDchg_hapen_read_graph_data, aes(x=year, y=rate, color=outcome, linetype=has_any_md)) + geom_vline(xintercept = 2012) + geom_line(size=.75) + theme_bw() + 
  ylim(15,30) + ggtitle("hospitals penalized for heart attack")

noMDchg_hapen_mort_graph <- ggplot(noMDchg_hapen_mort_graph_data, aes(x=year, y=rate, color=outcome, linetype=has_any_md)) + geom_vline(xintercept = 2012) + geom_line(size=.75) + theme_bw() + 
  ylim(0,20) + ggtitle("hospitals penalized for heart attack")

#4 No MD change, penalized for pnem
noMDchg_pnempen_graph_data <- noMDchg_pnempen_sample %>%
  group_by(has_any_md, year) %>%
  summarise_at(c("heartattack_mortality", "heartattack_readmission", "pneum_mortality", "pneum_readmission", "heartfailure_mortality", "heartfailure_readmission"), list(mean), na.rm=T) %>%
  filter(year!=2008) %>%
  mutate(has_any_md = as.factor(has_any_md))
noMDchg_pnempen_read_graph_data <- pivot_longer(noMDchg_pnempen_graph_data, c("heartattack_mortality", "heartattack_readmission", "pneum_mortality", "pneum_readmission", "heartfailure_mortality", "heartfailure_readmission"), names_to = "outcome", values_to = "rate") %>%
  filter(outcome %in% c("pneum_readmission", "heartfailure_readmission", "heartattack_readmission"))
noMDchg_pnempen_mort_graph_data <- pivot_longer(noMDchg_pnempen_graph_data, c("heartattack_mortality", "heartattack_readmission", "pneum_mortality", "pneum_readmission", "heartfailure_mortality", "heartfailure_readmission"), names_to = "outcome", values_to = "rate") %>%
  filter(outcome %in% c("pneum_mortality", "heartfailure_mortality", "heartattack_mortality"))

noMDchg_pnempen_read_graph <- ggplot(noMDchg_pnempen_read_graph_data, aes(x=year, y=rate, color=outcome, linetype=has_any_md)) + geom_vline(xintercept = 2012) + geom_line(size=.75) + theme_bw() + 
  ylim(15,30) + ggtitle("hospitals penalized for pnem")

noMDchg_pnempen_mort_graph <- ggplot(noMDchg_pnempen_mort_graph_data, aes(x=year, y=rate, color=outcome, linetype=has_any_md)) + geom_vline(xintercept = 2012) + geom_line(size=.75) + theme_bw() + 
  ylim(0,20) + ggtitle("hospitals penalized for pnem")

#5 No MD change, bottom tercile
noMDchg_bottom_graph_data <- noMDchg_bottom_sample %>%
  group_by(has_any_md, year) %>%
  summarise_at(c("heartattack_mortality", "heartattack_readmission", "pneum_mortality", "pneum_readmission", "heartfailure_mortality", "heartfailure_readmission"), list(mean), na.rm=T) %>%
  filter(year!=2008) %>%
  mutate(has_any_md = as.factor(has_any_md))
noMDchg_bottom_read_graph_data <- pivot_longer(noMDchg_bottom_graph_data, c("heartattack_mortality", "heartattack_readmission", "pneum_mortality", "pneum_readmission", "heartfailure_mortality", "heartfailure_readmission"), names_to = "outcome", values_to = "rate") %>%
  filter(outcome %in% c("pneum_readmission", "heartfailure_readmission", "heartattack_readmission"))
noMDchg_bottom_mort_graph_data <- pivot_longer(noMDchg_bottom_graph_data, c("heartattack_mortality", "heartattack_readmission", "pneum_mortality", "pneum_readmission", "heartfailure_mortality", "heartfailure_readmission"), names_to = "outcome", values_to = "rate") %>%
  filter(outcome %in% c("pneum_mortality", "heartfailure_mortality", "heartattack_mortality"))

noMDchg_bottom_read_graph <- ggplot(noMDchg_bottom_read_graph_data, aes(x=year, y=rate, color=outcome, linetype=has_any_md)) + geom_vline(xintercept = 2012) + geom_line(size=.75) + theme_bw() + 
  ylim(15,30) + ggtitle("bottom tercile")

noMDchg_bottom_mort_graph <- ggplot(noMDchg_bottom_mort_graph_data, aes(x=year, y=rate, color=outcome, linetype=has_any_md)) + geom_vline(xintercept = 2012) + geom_line(size=.75) + theme_bw() + 
  ylim(0,20) + ggtitle("bottom tercile")

#6 No MD change, middle tercile
noMDchg_middle_graph_data <- noMDchg_middle_sample %>%
  group_by(has_any_md, year) %>%
  summarise_at(c("heartattack_mortality", "heartattack_readmission", "pneum_mortality", "pneum_readmission", "heartfailure_mortality", "heartfailure_readmission"), list(mean), na.rm=T) %>%
  filter(year!=2008) %>%
  mutate(has_any_md = as.factor(has_any_md))
noMDchg_middle_read_graph_data <- pivot_longer(noMDchg_middle_graph_data, c("heartattack_mortality", "heartattack_readmission", "pneum_mortality", "pneum_readmission", "heartfailure_mortality", "heartfailure_readmission"), names_to = "outcome", values_to = "rate") %>%
  filter(outcome %in% c("pneum_readmission", "heartfailure_readmission", "heartattack_readmission"))
noMDchg_middle_mort_graph_data <- pivot_longer(noMDchg_middle_graph_data, c("heartattack_mortality", "heartattack_readmission", "pneum_mortality", "pneum_readmission", "heartfailure_mortality", "heartfailure_readmission"), names_to = "outcome", values_to = "rate") %>%
  filter(outcome %in% c("pneum_mortality", "heartfailure_mortality", "heartattack_mortality"))

noMDchg_middle_read_graph <- ggplot(noMDchg_middle_read_graph_data, aes(x=year, y=rate, color=outcome, linetype=has_any_md)) + geom_vline(xintercept = 2012) + geom_line(size=.75) + theme_bw() + 
  ylim(15,30) + ggtitle("middle tercile")

noMDchg_middle_mort_graph <- ggplot(noMDchg_middle_mort_graph_data, aes(x=year, y=rate, color=outcome, linetype=has_any_md)) + geom_vline(xintercept = 2012) + geom_line(size=.75) + theme_bw() + 
  ylim(0,20) + ggtitle("middle tercile")

#7 No MD change, top tercile
noMDchg_top_graph_data <- noMDchg_top_sample %>%
  group_by(has_any_md, year) %>%
  summarise_at(c("heartattack_mortality", "heartattack_readmission", "pneum_mortality", "pneum_readmission", "heartfailure_mortality", "heartfailure_readmission"), list(mean), na.rm=T) %>%
  filter(year!=2008) %>%
  mutate(has_any_md = as.factor(has_any_md))
noMDchg_top_read_graph_data <- pivot_longer(noMDchg_top_graph_data, c("heartattack_mortality", "heartattack_readmission", "pneum_mortality", "pneum_readmission", "heartfailure_mortality", "heartfailure_readmission"), names_to = "outcome", values_to = "rate") %>%
  filter(outcome %in% c("pneum_readmission", "heartfailure_readmission", "heartattack_readmission"))
noMDchg_top_mort_graph_data <- pivot_longer(noMDchg_top_graph_data, c("heartattack_mortality", "heartattack_readmission", "pneum_mortality", "pneum_readmission", "heartfailure_mortality", "heartfailure_readmission"), names_to = "outcome", values_to = "rate") %>%
  filter(outcome %in% c("pneum_mortality", "heartfailure_mortality", "heartattack_mortality"))

noMDchg_top_read_graph <- ggplot(noMDchg_top_read_graph_data, aes(x=year, y=rate, color=outcome, linetype=has_any_md)) + geom_vline(xintercept = 2012) + geom_line(size=.75) + theme_bw() + 
  ylim(15,30) + ggtitle("top tercile")

noMDchg_top_mort_graph <- ggplot(noMDchg_top_mort_graph_data, aes(x=year, y=rate, color=outcome, linetype=has_any_md)) + geom_vline(xintercept = 2012) + geom_line(size=.75) + theme_bw() + 
  ylim(0,20) + ggtitle("top tercile")

# put graphs in side by side plots, penalty readmission
noMDchg_pen_read_plot <- ggarrange(noMDchg_pen_read_graph, noMDchg_hfpen_read_graph, noMDchg_hapen_read_graph, noMDchg_pnempen_read_graph, 
          ncol = 2, 
          nrow = 2,
          common.legend = TRUE,
          legend = "right")
annotate_figure(noMDchg_pen_read_plot, top = text_grob("Average Readmission Rates\n", color = "black", face = "bold", size = 14))

# put graphs in side by side plots, penalty mortality
noMDchg_pen_mort_plot <- ggarrange(noMDchg_pen_mort_graph, noMDchg_hfpen_mort_graph, noMDchg_hapen_mort_graph, noMDchg_pnempen_mort_graph, 
                                   ncol = 2, 
                                   nrow = 2,
                                   common.legend = TRUE,
                                   legend = "right")
annotate_figure(noMDchg_pen_mort_plot, top = text_grob("Average Mortality Rates\n", color = "black", face = "bold", size = 14))

# put graphs in side by side plots, tercile readmission
noMDchg_terc_read_plot <- ggarrange(noMDchg_pen_read_graph, noMDchg_bottom_read_graph, noMDchg_middle_read_graph, noMDchg_top_read_graph, 
                                   ncol = 2, 
                                   nrow = 2,
                                   common.legend = TRUE,
                                   legend = "right")
annotate_figure(noMDchg_terc_read_plot, top = text_grob("Average Readmission Rates\n", color = "black", face = "bold", size = 14))

# put graphs in side by side plots, tercile mortality
noMDchg_terc_mort_plot <- ggarrange(noMDchg_pen_mort_graph, noMDchg_bottom_mort_graph, noMDchg_middle_mort_graph, noMDchg_top_mort_graph, 
                                    ncol = 2, 
                                    nrow = 2,
                                    common.legend = TRUE,
                                    legend = "right")
annotate_figure(noMDchg_terc_mort_plot, top = text_grob("Average Mortality Rates\n", color = "black", face = "bold", size = 14))




# Regressions for each sample ##############

# 1. run basic 2x2 diff in diff for each sample on each outcome
# 1a. all penalized hospitals
noMDchg_pen_pnem_read_did <- fixest::feols(pneum_readmission ~ has_any_md + post_2012 + has_any_md*post_2012, data=noMDchg_pen_sample)
noMDchg_pen_hf_read_did <- fixest::feols(heartfailure_readmission ~ has_any_md + post_2012 + has_any_md*post_2012, data=noMDchg_pen_sample)
noMDchg_pen_ha_read_did <- fixest::feols(heartattack_readmission ~ has_any_md + post_2012 + has_any_md*post_2012, data=noMDchg_pen_sample)
noMDchg_pen_pnem_mort_did <- fixest::feols(pneum_mortality ~ has_any_md + post_2012 + has_any_md*post_2012, data=noMDchg_pen_sample)
noMDchg_pen_hf_mort_did <- fixest::feols(heartfailure_mortality ~ has_any_md + post_2012 + has_any_md*post_2012, data=noMDchg_pen_sample)
noMDchg_pen_ha_mort_did <- fixest::feols(heartattack_mortality ~ has_any_md + post_2012 + has_any_md*post_2012, data=noMDchg_pen_sample)
etable(noMDchg_pen_pnem_read_did, noMDchg_pen_hf_read_did, noMDchg_pen_ha_read_did, noMDchg_pen_pnem_mort_did, noMDchg_pen_hf_mort_did, noMDchg_pen_ha_mort_did)

noMDchg_pen_wa_read_did <- fixest::feols(weightedavg_read ~ has_any_md + post_2012 + has_any_md*post_2012, data=noMDchg_pen_sample)
noMDchg_pen_wa_mort_did <- fixest::feols(weightedavg_mort ~ has_any_md + post_2012 + has_any_md*post_2012, data=noMDchg_pen_sample)
etable(noMDchg_pen_wa_read_did, noMDchg_pen_wa_mort_did)

# 1b. penalized for pneumonia
noMDchg_pneumpen_pnem_read_did <- fixest::feols(pneum_readmission ~ has_any_md + post + has_any_md*post, data=noMDchg_pnempen_sample)
noMDchg_pneumpen_hf_read_did <- fixest::feols(heartfailure_readmission ~ has_any_md + post + has_any_md*post, data=noMDchg_pnempen_sample)
noMDchg_pneumpen_ha_read_did <- fixest::feols(heartattack_readmission ~ has_any_md + post + has_any_md*post, data=noMDchg_pnempen_sample)
noMDchg_pneumpen_pnem_mort_did <- fixest::feols(pneum_mortality ~ has_any_md + post + has_any_md*post, data=noMDchg_pnempen_sample)
noMDchg_pneumpen_hf_mort_did <- fixest::feols(heartfailure_mortality ~ has_any_md + post + has_any_md*post, data=noMDchg_pnempen_sample)
noMDchg_pneumpen_ha_mort_did <- fixest::feols(heartattack_mortality ~ has_any_md + post + has_any_md*post, data=noMDchg_pnempen_sample)
etable(noMDchg_pneumpen_pnem_read_did, noMDchg_pneumpen_hf_read_did, noMDchg_pneumpen_ha_read_did, noMDchg_pneumpen_pnem_mort_did, noMDchg_pneumpen_hf_mort_did, noMDchg_pneumpen_ha_mort_did)

# 1c. penalized for heart failure
noMDchg_hfpen_pnem_read_did <- fixest::feols(pneum_readmission ~ has_any_md + post + has_any_md*post, data=noMDchg_hfpen_sample)
noMDchg_hfpen_hf_read_did <- fixest::feols(heartfailure_readmission ~ has_any_md + post + has_any_md*post, data=noMDchg_hfpen_sample)
noMDchg_hfpen_ha_read_did <- fixest::feols(heartattack_readmission ~ has_any_md + post + has_any_md*post, data=noMDchg_hfpen_sample)
noMDchg_hfpen_pnem_mort_did <- fixest::feols(pneum_mortality ~ has_any_md + post + has_any_md*post, data=noMDchg_hfpen_sample)
noMDchg_hfpen_hf_mort_did <- fixest::feols(heartfailure_mortality ~ has_any_md + post + has_any_md*post, data=noMDchg_hfpen_sample)
noMDchg_hfpen_ha_mort_did <- fixest::feols(heartattack_mortality ~ has_any_md + post + has_any_md*post, data=noMDchg_hfpen_sample)
etable(noMDchg_hfpen_pnem_read_did, noMDchg_hfpen_hf_read_did, noMDchg_hfpen_ha_read_did, noMDchg_hfpen_pnem_mort_did, noMDchg_hfpen_hf_mort_did, noMDchg_hfpen_ha_mort_did)

# 1d. penalized for heart attack
noMDchg_hapen_pnem_read_did <- fixest::feols(pneum_readmission ~ has_any_md + post + has_any_md*post, data=noMDchg_hapen_sample)
noMDchg_hapen_hf_read_did <- fixest::feols(heartfailure_readmission ~ has_any_md + post + has_any_md*post, data=noMDchg_hapen_sample)
noMDchg_hapen_ha_read_did <- fixest::feols(heartattack_readmission ~ has_any_md + post + has_any_md*post, data=noMDchg_hapen_sample)
noMDchg_hapen_pnem_mort_did <- fixest::feols(pneum_mortality ~ has_any_md + post + has_any_md*post, data=noMDchg_hapen_sample)
noMDchg_hapen_hf_mort_did <- fixest::feols(heartfailure_mortality ~ has_any_md + post + has_any_md*post, data=noMDchg_hapen_sample)
noMDchg_hapen_ha_mort_did <- fixest::feols(heartattack_mortality ~ has_any_md + post + has_any_md*post, data=noMDchg_hapen_sample)
etable(noMDchg_hapen_pnem_read_did, noMDchg_hapen_hf_read_did, noMDchg_hapen_ha_read_did, noMDchg_hapen_pnem_mort_did, noMDchg_hapen_hf_mort_did, noMDchg_hapen_ha_mort_did)

# 1d. bottom tercile
noMDchg_bottom_pnem_read_did <- fixest::feols(pneum_readmission ~ has_any_md + post + has_any_md*post, data=noMDchg_bottom_sample)
noMDchg_bottom_hf_read_did <- fixest::feols(heartfailure_readmission ~ has_any_md + post + has_any_md*post, data=noMDchg_bottom_sample)
noMDchg_bottom_ha_read_did <- fixest::feols(heartattack_readmission ~ has_any_md + post + has_any_md*post, data=noMDchg_bottom_sample)
noMDchg_bottom_pnem_mort_did <- fixest::feols(pneum_mortality ~ has_any_md + post + has_any_md*post, data=noMDchg_bottom_sample)
noMDchg_bottom_hf_mort_did <- fixest::feols(heartfailure_mortality ~ has_any_md + post + has_any_md*post, data=noMDchg_bottom_sample)
noMDchg_bottom_ha_mort_did <- fixest::feols(heartattack_mortality ~ has_any_md + post + has_any_md*post, data=noMDchg_bottom_sample)
etable(noMDchg_bottom_pnem_read_did, noMDchg_bottom_hf_read_did, noMDchg_bottom_ha_read_did, noMDchg_bottom_pnem_mort_did, noMDchg_bottom_hf_mort_did, noMDchg_bottom_ha_mort_did)

# 1e. middle tercile
noMDchg_middle_pnem_read_did <- fixest::feols(pneum_readmission ~ has_any_md + post + has_any_md*post, data=noMDchg_middle_sample)
noMDchg_middle_hf_read_did <- fixest::feols(heartfailure_readmission ~ has_any_md + post + has_any_md*post, data=noMDchg_middle_sample)
noMDchg_middle_ha_read_did <- fixest::feols(heartattack_readmission ~ has_any_md + post + has_any_md*post, data=noMDchg_middle_sample)
noMDchg_middle_pnem_mort_did <- fixest::feols(pneum_mortality ~ has_any_md + post + has_any_md*post, data=noMDchg_middle_sample)
noMDchg_middle_hf_mort_did <- fixest::feols(heartfailure_mortality ~ has_any_md + post + has_any_md*post, data=noMDchg_middle_sample)
noMDchg_middle_ha_mort_did <- fixest::feols(heartattack_mortality ~ has_any_md + post + has_any_md*post, data=noMDchg_middle_sample)
etable(noMDchg_middle_pnem_read_did, noMDchg_middle_hf_read_did, noMDchg_middle_ha_read_did, noMDchg_middle_pnem_mort_did, noMDchg_middle_hf_mort_did, noMDchg_middle_ha_mort_did)

# 1f. top tercile hospitals
noMDchg_top_pnem_read_did <- fixest::feols(pneum_readmission ~ has_any_md + post + has_any_md*post, data=noMDchg_top_sample)
noMDchg_top_hf_read_did <- fixest::feols(heartfailure_readmission ~ has_any_md + post + has_any_md*post, data=noMDchg_top_sample)
noMDchg_top_ha_read_did <- fixest::feols(heartattack_readmission ~ has_any_md + post + has_any_md*post, data=noMDchg_top_sample)
noMDchg_top_pnem_mort_did <- fixest::feols(pneum_mortality ~ has_any_md + post + has_any_md*post, data=noMDchg_top_sample)
noMDchg_top_hf_mort_did <- fixest::feols(heartfailure_mortality ~ has_any_md + post + has_any_md*post, data=noMDchg_top_sample)
noMDchg_top_ha_mort_did <- fixest::feols(heartattack_mortality ~ has_any_md + post + has_any_md*post, data=noMDchg_top_sample)
etable(noMDchg_top_pnem_read_did, noMDchg_top_hf_read_did, noMDchg_top_ha_read_did, noMDchg_top_pnem_mort_did, noMDchg_top_hf_mort_did, noMDchg_top_ha_mort_did)


# 2. Run event study on main penalty sample and for terciles
# 2a. main penalty full sample
noMDchg_pen_pneum_read_es = feols(pneum_readmission ~ has_any_md*yr2009 + has_any_md*yr2010 + has_any_md*yr2012 + 
                                    has_any_md*yr2013 + has_any_md*yr2014 + has_any_md*yr2015 | ID + year, data = noMDchg_pen_sample)
noMDchg_pen_hf_read_es = feols(heartfailure_readmission ~ has_any_md*yr2009 + has_any_md*yr2010 + has_any_md*yr2012 + 
                                    has_any_md*yr2013 + has_any_md*yr2014 + has_any_md*yr2015 | ID + year, data = noMDchg_pen_sample)
noMDchg_pen_ha_read_es = feols(heartattack_readmission ~ has_any_md*yr2009 + has_any_md*yr2010 + has_any_md*yr2012 + 
                                    has_any_md*yr2013 + has_any_md*yr2014 + has_any_md*yr2015 | ID + year, data = noMDchg_pen_sample)
noMDchg_pen_pneum_mort_es = feols(pneum_mortality ~ has_any_md*yr2009 + has_any_md*yr2010 + has_any_md*yr2012 + 
                                    has_any_md*yr2013 + has_any_md*yr2014 + has_any_md*yr2015 | ID + year, data = noMDchg_pen_sample)
noMDchg_pen_hf_mort_es = feols(heartfailure_mortality ~ has_any_md*yr2009 + has_any_md*yr2010 + has_any_md*yr2012 + 
                                 has_any_md*yr2013 + has_any_md*yr2014 + has_any_md*yr2015 | ID + year, data = noMDchg_pen_sample)
noMDchg_pen_ha_mort_es = feols(heartattack_mortality ~ has_any_md*yr2009 + has_any_md*yr2010 + has_any_md*yr2012 + 
                                 has_any_md*yr2013 + has_any_md*yr2014 + has_any_md*yr2015 | ID + year, data = noMDchg_pen_sample)
etable(noMDchg_pen_pneum_read_es, noMDchg_pen_hf_read_es, noMDchg_pen_ha_read_es, noMDchg_pen_pneum_mort_es, noMDchg_pen_hf_mort_es, noMDchg_pen_ha_mort_es)

# 2b. main penalty bottom tercile sample
noMDchg_bottom_pneum_read_es = feols(pneum_readmission ~ has_any_md*yr2009 + has_any_md*yr2010 + has_any_md*yr2012 + 
                                    has_any_md*yr2013 + has_any_md*yr2014 + has_any_md*yr2015 | ID + year, data = noMDchg_bottom_sample)
noMDchg_bottom_hf_read_es = feols(heartfailure_readmission ~ has_any_md*yr2009 + has_any_md*yr2010 + has_any_md*yr2012 + 
                                 has_any_md*yr2013 + has_any_md*yr2014 + has_any_md*yr2015 | ID + year, data = noMDchg_bottom_sample)
noMDchg_bottom_ha_read_es = feols(heartattack_readmission ~ has_any_md*yr2009 + has_any_md*yr2010 + has_any_md*yr2012 + 
                                 has_any_md*yr2013 + has_any_md*yr2014 + has_any_md*yr2015 | ID + year, data = noMDchg_bottom_sample)
noMDchg_bottom_pneum_mort_es = feols(pneum_mortality ~ has_any_md*yr2009 + has_any_md*yr2010 + has_any_md*yr2012 + 
                                    has_any_md*yr2013 + has_any_md*yr2014 + has_any_md*yr2015 | ID + year, data = noMDchg_bottom_sample)
noMDchg_bottom_hf_mort_es = feols(heartfailure_mortality ~ has_any_md*yr2009 + has_any_md*yr2010 + has_any_md*yr2012 + 
                                 has_any_md*yr2013 + has_any_md*yr2014 + has_any_md*yr2015 | ID + year, data = noMDchg_bottom_sample)
noMDchg_bottom_ha_mort_es = feols(heartattack_mortality ~ has_any_md*yr2009 + has_any_md*yr2010 + has_any_md*yr2012 + 
                                 has_any_md*yr2013 + has_any_md*yr2014 + has_any_md*yr2015 | ID + year, data = noMDchg_bottom_sample)
etable(noMDchg_bottom_pneum_read_es, noMDchg_bottom_hf_read_es, noMDchg_bottom_ha_read_es, noMDchg_bottom_pneum_mort_es, noMDchg_bottom_hf_mort_es, noMDchg_bottom_ha_mort_es)

# 2c. main penalty middle tercile sample
noMDchg_middle_pneum_read_es = feols(pneum_readmission ~ has_any_md*yr2009 + has_any_md*yr2010 + has_any_md*yr2012 + 
                                       has_any_md*yr2013 + has_any_md*yr2014 + has_any_md*yr2015 | ID + year, data = noMDchg_middle_sample)
noMDchg_middle_hf_read_es = feols(heartfailure_readmission ~ has_any_md*yr2009 + has_any_md*yr2010 + has_any_md*yr2012 + 
                                    has_any_md*yr2013 + has_any_md*yr2014 + has_any_md*yr2015 | ID + year, data = noMDchg_middle_sample)
noMDchg_middle_ha_read_es = feols(heartattack_readmission ~ has_any_md*yr2009 + has_any_md*yr2010 + has_any_md*yr2012 + 
                                    has_any_md*yr2013 + has_any_md*yr2014 + has_any_md*yr2015 | ID + year, data = noMDchg_middle_sample)
noMDchg_middle_pneum_mort_es = feols(pneum_mortality ~ has_any_md*yr2009 + has_any_md*yr2010 + has_any_md*yr2012 + 
                                       has_any_md*yr2013 + has_any_md*yr2014 + has_any_md*yr2015 | ID + year, data = noMDchg_middle_sample)
noMDchg_middle_hf_mort_es = feols(heartfailure_mortality ~ has_any_md*yr2009 + has_any_md*yr2010 + has_any_md*yr2012 + 
                                    has_any_md*yr2013 + has_any_md*yr2014 + has_any_md*yr2015 | ID + year, data = noMDchg_middle_sample)
noMDchg_middle_ha_mort_es = feols(heartattack_mortality ~ has_any_md*yr2009 + has_any_md*yr2010 + has_any_md*yr2012 + 
                                    has_any_md*yr2013 + has_any_md*yr2014 + has_any_md*yr2015 | ID + year, data = noMDchg_middle_sample)
etable(noMDchg_middle_pneum_read_es, noMDchg_middle_hf_read_es, noMDchg_middle_ha_read_es, noMDchg_middle_pneum_mort_es, noMDchg_middle_hf_mort_es, noMDchg_middle_ha_mort_es)

#2d. main penalty top tercile hospitals
noMDchg_top_pneum_read_es = feols(pneum_readmission ~ has_any_md*yr2009 + has_any_md*yr2010 + has_any_md*yr2012 + 
                                       has_any_md*yr2013 + has_any_md*yr2014 + has_any_md*yr2015 | ID + year, data = noMDchg_top_sample)
noMDchg_top_hf_read_es = feols(heartfailure_readmission ~ has_any_md*yr2009 + has_any_md*yr2010 + has_any_md*yr2012 + 
                                    has_any_md*yr2013 + has_any_md*yr2014 + has_any_md*yr2015 | ID + year, data = noMDchg_top_sample)
noMDchg_top_ha_read_es = feols(heartattack_readmission ~ has_any_md*yr2009 + has_any_md*yr2010 + has_any_md*yr2012 + 
                                    has_any_md*yr2013 + has_any_md*yr2014 + has_any_md*yr2015 | ID + year, data = noMDchg_top_sample)
noMDchg_top_pneum_mort_es = feols(pneum_mortality ~ has_any_md*yr2009 + has_any_md*yr2010 + has_any_md*yr2012 + 
                                       has_any_md*yr2013 + has_any_md*yr2014 + has_any_md*yr2015 | ID + year, data = noMDchg_top_sample)
noMDchg_top_hf_mort_es = feols(heartfailure_mortality ~ has_any_md*yr2009 + has_any_md*yr2010 + has_any_md*yr2012 + 
                                    has_any_md*yr2013 + has_any_md*yr2014 + has_any_md*yr2015 | ID + year, data = noMDchg_top_sample)
noMDchg_top_ha_mort_es = feols(heartattack_mortality ~ has_any_md*yr2009 + has_any_md*yr2010 + has_any_md*yr2012 + 
                                    has_any_md*yr2013 + has_any_md*yr2014 + has_any_md*yr2015 | ID + year, data = noMDchg_top_sample)
etable(noMDchg_top_pneum_read_es, noMDchg_top_hf_read_es, noMDchg_top_ha_read_es, noMDchg_top_pneum_mort_es, noMDchg_top_hf_mort_es, noMDchg_top_ha_mort_es)



