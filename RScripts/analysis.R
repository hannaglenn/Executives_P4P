library(readr)
library(dplyr)
library(fixest)
library(ggplot2)
library(ggpubr)
library(tidyr)
library(synthdid)
library(kableExtra)
options(knitr.kable.NA=" ")

# Comparisons I need: 
# 1. FP - NFP
# 2. FP - has MD
# 3. FP - no MD
# 4. has MD - no MD

# Methodologies:
# 1. full sample difference in difference
# 2. Matched difference in difference
# 3. Synthetic difference in difference

# Outcomes:
# 1. Readmission rates
# 2. Mortality Rates
# 3. Uncompensated Care and Case Mix Index

# Sample limitations:
# 1. dropping hospitals with leadership changes
# 2. not dropping hospitals with leadership changes

# limit to 2010 to 2014
hospital_data <- readRDS(paste0(created_data_path, "all_hospital_data.rds")) %>%
  filter(year>=2010 & year<=2014)

# drop hospitals with less than 10 beds
hospital_data <- hospital_data %>%
  group_by(ID) %>%
  mutate(maxbeds=max(beds, na.rm=T)) %>%
  ungroup() %>%
  filter(maxbeds>=10) %>%
  select(-maxbeds)

hospital_data$uncomp_care <- abs(hospital_data$uncomp_care)
hospital_data <- hospital_data %>%
  mutate(uncomp_care=uncomp_care/1000000) 

# fill ID
hospital_data <- hospital_data %>%
  group_by(MCRNUM) %>%
  fill(ID, .direction="downup") %>%
  ungroup() %>%
  filter(!is.na(ID)) 

# Remove hospitals that change from nonprofit to for-profit or vice versa
hospital_data <- hospital_data %>%
  mutate(post_2012=ifelse(year>2012, 1, 0),
         NFP = ifelse(profit_status=="nonprofit", 1, 0),
         FP = ifelse(profit_status=="forprofit",1,0)) %>%
  group_by(ID) %>%
  arrange(year) %>%
  mutate(lag_NFP = dplyr::lag(NFP)) %>%
  ungroup() %>%
  filter(NFP==lag_NFP | year==2010)

comp <- c("FP - NFP", "FP - has MD", "FP - no MD", "has MD - no MD")
method <- c("full", "matched", "synth")

### ANALYSIS WHERE I DROP HOSPITALS WITH LEADERSHIP CHANGES####

### OUTCOME: READMISSION ####

readmission_analysis <- lapply(comp, function(x){
  data <- hospital_data 
  
  # create did variable and limit samples accordingly
  if (x=="FP - NFP"){
    data <- data %>%
      mutate(did = post_2012*FP)
  }
  if (x=="FP - has MD"){
    data <- data %>%
      filter(!(NFP==1 & no_num_md_change_2010_2014==0)) %>%
      filter(!(NFP==1 & is.na(no_num_md_change_2010_2014))) %>%
      filter(!(profit_status=="nonprofit" & ever_has_md==0)) %>%
      mutate(did = post_2012*FP)
  }
  if (x=="FP - no MD"){
    data <- data %>%
      filter(!(NFP==1 & no_num_md_change_2010_2014==0)) %>%
      filter(!(NFP==1 & is.na(no_num_md_change_2010_2014))) %>%
      filter(!(profit_status=="nonprofit" & ever_has_md==1)) %>%
      mutate(did = post_2012*FP)
  }
  if (x=="has MD - no MD"){
    data <- data %>%
      filter(no_num_md_change_2010_2014==1) %>%
      filter(profit_status!="forprofit") %>%
      mutate(NFP_nomd = ifelse(profit_status=="nonprofit" & ever_has_md==0,1,0)) %>%
      mutate(did=post_2012*NFP_nomd)
  }
  
  methods <- lapply(method, function(y){
    if (y=="matched" & x=="FP - NFP"){
      data <- data %>%
        filter(matched_FP_NFP==1)
    }
    if (y=="matched" & x=="FP - has MD"){
      data <- data %>%
        filter(matched_FP_nfpMD==1)
    }
    if (y=="matched" & x=="FP - no MD"){
      data <- data %>%
        filter(matched_FP_nfpnoMD==1)
    }
    if (y=="matched" & x=="has MD - no MD"){
      data <- data %>%
        filter(matched_nfpMD_nfpnoMD==1)
    }
    
    # full and matched analysis
    if (y=="full" | y=="matched"){
      wa_did <- fixest::feols(weightedavg_read ~ did | ID + year, cluster = ~ID, data=data)
      pnem_did <- fixest::feols(rate_pneum_readmission ~ did | ID + year, cluster = ~ID,data=data)
      hf_did <- fixest::feols(rate_heartfailure_readmission ~ did | ID + year, cluster = ~ID,data=data)
      ha_did <- fixest::feols(rate_heartattack_readmission ~ did | ID + year, cluster = ~ID,data=data)
    }
    
    if (y=="synth"){
      wa_data <- data %>%
        select(ID, year, weightedavg_read, did) %>%
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
      wa_data <- panel.matrices(as.data.frame(wa_data))
      wa_did <- synthdid_estimate(wa_data$Y, wa_data$N0, wa_data$T0)
      
      pnem_data <- data %>%
        select(ID, year, rate_pneum_readmission, did) %>%
        na.omit() %>%
        mutate(count=1) %>%
        group_by(ID) %>%
        mutate(min=min(year[did==1])) %>%
        mutate(sum=sum(count)) %>%
        ungroup() %>%
        filter(sum==5) %>%
        filter(min!=2014) %>%
        select(-sum, -count, -min) 
      pnem_data <- panel.matrices(as.data.frame(pnem_data))
      pnem_did <- synthdid_estimate(pnem_data$Y, pnem_data$N0, pnem_data$T0)
      
      hf_data <- data %>%
        select(ID, year, rate_heartfailure_readmission, did) %>%
        na.omit() %>%
        mutate(count=1) %>%
        group_by(ID) %>%
        mutate(min=min(year[did==1])) %>%
        mutate(sum=sum(count)) %>%
        ungroup() %>%
        filter(sum==5) %>%
        filter(min!=2014) %>%
        select(-sum, -count, -min) 
      hf_data <- panel.matrices(as.data.frame(hf_data))
      hf_did <- synthdid_estimate(hf_data$Y, hf_data$N0, hf_data$T0)
      
      ha_data <- data %>%
        select(ID, year, rate_heartattack_readmission, did) %>%
        na.omit() %>%
        mutate(count=1) %>%
        group_by(ID) %>%
        mutate(min=min(year[did==1])) %>%
        mutate(sum=sum(count)) %>%
        ungroup() %>%
        filter(sum==5) %>%
        filter(min!=2014) %>%
        select(-sum, -count, -min) 
      ha_data <- panel.matrices(as.data.frame(ha_data))
      ha_did <- synthdid_estimate(ha_data$Y, ha_data$N0, ha_data$T0)
    }
    
    list(wa_did=wa_did, pnem_did=pnem_did, hf_did=hf_did, ha_did=ha_did)
  })
  
  return(methods)
})

## OUTCOME: MORTALITY ####
mortality_analysis <- lapply(comp, function(x){
  data <- hospital_data 
  
  # create did variable and limit samples accordingly
  if (x=="FP - NFP"){
    data <- data %>%
      mutate(did = post_2012*FP)
  }
  if (x=="FP - has MD"){
    data <- data %>%
      filter(!(NFP==1 & no_num_md_change_2010_2014==0)) %>%
      filter(!(NFP==1 & is.na(no_num_md_change_2010_2014))) %>%
      filter(!(profit_status=="nonprofit" & ever_has_md==0)) %>%
      mutate(did = post_2012*FP)
  }
  if (x=="FP - no MD"){
    data <- data %>%
      filter(!(NFP==1 & no_num_md_change_2010_2014==0)) %>%
      filter(!(NFP==1 & is.na(no_num_md_change_2010_2014))) %>%
      filter(!(profit_status=="nonprofit" & ever_has_md==1)) %>%
      mutate(did = post_2012*FP)
  }
  if (x=="has MD - no MD"){
    data <- data %>%
      filter(no_num_md_change_2010_2014==1) %>%
      filter(profit_status!="forprofit") %>%
      mutate(NFP_nomd = ifelse(profit_status=="nonprofit" & ever_has_md==0,1,0)) %>%
      mutate(did=post_2012*NFP_nomd)
  }
  
  methods <- lapply(method, function(y){
    if (y=="matched" & x=="FP - NFP"){
      data <- data %>%
        filter(matched_FP_NFP==1)
    }
    if (y=="matched" & x=="FP - has MD"){
      data <- data %>%
        filter(matched_FP_nfpMD==1)
    }
    if (y=="matched" & x=="FP - no MD"){
      data <- data %>%
        filter(matched_FP_nfpnoMD==1)
    }
    if (y=="matched" & x=="has MD - no MD"){
      data <- data %>%
        filter(matched_nfpMD_nfpnoMD==1)
    }
    
    # full and matched analysis
    if (y=="full" | y=="matched"){
      wa_did <- fixest::feols(weightedavg_mort ~ did | ID + year, cluster = ~ID, data=data)
      pnem_did <- fixest::feols(rate_pneum_mortality ~ did | ID + year, cluster = ~ID,data=data)
      hf_did <- fixest::feols(rate_heartfailure_mortality ~ did | ID + year, cluster = ~ID,data=data)
      ha_did <- fixest::feols(rate_heartattack_mortality ~ did | ID + year, cluster = ~ID,data=data)
    }
    
    if (y=="synth"){
      wa_data <- data %>%
        select(ID, year, weightedavg_mort, did) %>%
        na.omit() %>%
        mutate(count=1) %>%
        group_by(ID) %>%
        mutate(min=min(year[did==1])) %>%
        mutate(sum=sum(count)) %>%
        ungroup() %>%
        filter(sum==5) %>%
        filter(min!=2014) %>%
        select(-sum, -count, -min) 
      wa_data <- panel.matrices(as.data.frame(wa_data))
      wa_did <- synthdid_estimate(wa_data$Y, wa_data$N0, wa_data$T0)
      
      pnem_data <- data %>%
        select(ID, year, rate_pneum_mortality, did) %>%
        na.omit() %>%
        mutate(count=1) %>%
        group_by(ID) %>%
        mutate(min=min(year[did==1])) %>%
        mutate(sum=sum(count)) %>%
        ungroup() %>%
        filter(sum==5) %>%
        filter(min!=2014) %>%
        select(-sum, -count, -min) 
      pnem_data <- panel.matrices(as.data.frame(pnem_data))
      pnem_did <- synthdid_estimate(pnem_data$Y, pnem_data$N0, pnem_data$T0)
      
      hf_data <- data %>%
        select(ID, year, rate_heartfailure_mortality, did) %>%
        na.omit() %>%
        mutate(count=1) %>%
        group_by(ID) %>%
        mutate(min=min(year[did==1])) %>%
        mutate(sum=sum(count)) %>%
        ungroup() %>%
        filter(sum==5) %>%
        filter(min!=2014) %>%
        select(-sum, -count, -min) 
      hf_data <- panel.matrices(as.data.frame(hf_data))
      hf_did <- synthdid_estimate(hf_data$Y, hf_data$N0, hf_data$T0)
      
      ha_data <- data %>%
        select(ID, year, rate_heartattack_mortality, did) %>%
        na.omit() %>%
        mutate(count=1) %>%
        group_by(ID) %>%
        mutate(min=min(year[did==1])) %>%
        mutate(sum=sum(count)) %>%
        ungroup() %>%
        filter(sum==5) %>%
        filter(min!=2014) %>%
        select(-sum, -count, -min) 
      ha_data <- panel.matrices(as.data.frame(ha_data))
      ha_did <- synthdid_estimate(ha_data$Y, ha_data$N0, ha_data$T0)
    }
    
    list(wa_did=wa_did, pnem_did=pnem_did, hf_did=hf_did, ha_did=ha_did)
  })
  
  return(methods)
})

## OUTCOME: UNCOMPENSATED CARE and CMI ####
uncomp_cmi_analysis <- lapply(comp, function(x){
  data <- hospital_data
  
  # create did variable and limit samples accordingly
  if (x=="FP - NFP"){
    data <- data %>%
      mutate(did = post_2012*FP)
  }
  if (x=="FP - has MD"){
    data <- data %>%
      filter(!(NFP==1 & no_num_md_change_2010_2014==0)) %>%
      filter(!(NFP==1 & is.na(no_num_md_change_2010_2014))) %>%
      filter(!(profit_status=="nonprofit" & ever_has_md==0)) %>%
      mutate(did = post_2012*FP)
  }
  if (x=="FP - no MD"){
    data <- data %>%
      filter(!(NFP==1 & no_num_md_change_2010_2014==0)) %>%
      filter(!(NFP==1 & is.na(no_num_md_change_2010_2014))) %>%
      filter(!(profit_status=="nonprofit" & ever_has_md==1)) %>%
      mutate(did = post_2012*FP)
  }
  if (x=="has MD - no MD"){
    data <- data %>%
      filter(no_num_md_change_2010_2014==1) %>%
      filter(profit_status!="forprofit") %>%
      mutate(NFP_nomd = ifelse(profit_status=="nonprofit" & ever_has_md==0,1,0)) %>%
      mutate(did=post_2012*NFP_nomd)
  }
  
  methods <- lapply(method, function(y){
    if (y=="matched" & x=="FP - NFP"){
      data <- data %>%
        filter(matched_FP_NFP==1)
    }
    if (y=="matched" & x=="FP - has MD"){
      data <- data %>%
        filter(matched_FP_nfpMD==1)
    }
    if (y=="matched" & x=="FP - no MD"){
      data <- data %>%
        filter(matched_FP_nfpnoMD==1)
    }
    if (y=="matched" & x=="has MD - no MD"){
      data <- data %>%
        filter(matched_nfpMD_nfpnoMD==1)
    }
    
    # full and matched analysis
    if (y=="full" | y=="matched"){
      uncomp_did <- fixest::feols(uncomp_care ~ did | ID + year, cluster = ~ID, data=data)
      cmi_did <- fixest::feols(cmiv ~ did | ID + year, cluster = ~ID,data=data)
    }
    
    if (y=="synth"){
      uncomp_data <- data %>%
        distinct(ID, year, uncomp_care, did) %>%
        na.omit() %>%
        group_by(ID, year) %>%
        mutate(count=1) %>%
        mutate(sum=sum(count)) %>%
        ungroup() %>%
        filter(sum==1) %>%
        filter(uncomp_care!=0) %>%
        mutate(count=1) %>%
        group_by(ID) %>%
        mutate(sum=sum(count)) %>%
        mutate(min=min(year[did==1])) %>%
        ungroup() %>%
        filter(sum==5) %>%
        filter(min!=2014) %>%
        select(-sum, -count, -min)
      uncomp_data <- panel.matrices(as.data.frame(uncomp_data))
      uncomp_did <- synthdid_estimate(uncomp_data$Y, uncomp_data$N0, uncomp_data$T0)
      
      cmi_data <- data %>%
        select(ID, year, cmiv, did) %>%
        na.omit() %>%
        mutate(count=1) %>%
        group_by(ID) %>%
        mutate(min=min(year[did==1])) %>%
        mutate(sum=sum(count)) %>%
        ungroup() %>%
        filter(sum==5) %>%
        filter(min!=2014) %>%
        select(-sum, -count, -min) 
      cmi_data <- panel.matrices(as.data.frame(cmi_data))
      cmi_did <- synthdid_estimate(cmi_data$Y, cmi_data$N0, cmi_data$T0)
    }
    
    list(uncomp_did=uncomp_did, cmi_did=cmi_did)
  })
  
  return(methods)
})

## FULL SAMPLE TABLES ####

# make a table for weighted average readmissions and mortality comparing clinical NFP to non-clinical NFP for full sample and matched
tab1a <- etable(readmission_analysis[[4]][[1]][["wa_did"]], mortality_analysis[[4]][[1]][["wa_did"]], tex=TRUE,
                dict = c("weightedavg_read"="Weighted Avg. Readmission Rate", "weightedavg_mort"="Weighted Avg. Mortality Rate",
                         "did"="NP w/ MD Executive x Post Penalty","ID"="Hospital", "year"="Year"),
                style.tex = style.tex("aer"), fitstat = ~n,
                digits = "r2", title = "Comparison of MD vs. non-MD Nonprofit Readmission and Mortality Regression Results",
                extralines = list("\\textbf{Comparison Group:}" = c("", ""),
                                  "NP w/out MD Executive" = c("$\\checkmark$", "$\\checkmark$")),
                drop.section="fixef", label = "tab:MD_noMD_fullsample")
write(tab1,file="Tables/MD_noMD_readmort_fullsample.tex")

# table for weighted avg readmission and mortality for all for-profit comparisons
tab1b <- etable(readmission_analysis[[1]][[1]][["wa_did"]], readmission_analysis[[2]][[1]][["wa_did"]], readmission_analysis[[3]][[1]][["wa_did"]],
                mortality_analysis[[1]][[1]][["wa_did"]], mortality_analysis[[2]][[1]][["wa_did"]], mortality_analysis[[3]][[1]][["wa_did"]],
                tex=TRUE,
                dict = c("weightedavg_read"="Weighted Avg. Readmission Rate", "weightedavg_mort"="Weighted Avg. Mortality Rate",
                         "did"="For Profit x Post Penalty","ID"="Hospital", "year"="Year"),
                style.tex = style.tex("aer"), fitstat = ~n,
                digits = "r2", title = "Comparison to For-Profit Regression Results",
                extralines = list("\\textbf{Comparison Group:}" = c("", "", "" ,"", "", ""),
                                  "All Nonprofits" = c("$\\checkmark$", "", "", "$\\checkmark$", "", ""),
                                  "Nonprofits w/ MD" = c("", "$\\checkmark$", "", "", "$\\checkmark$", ""),
                                  "Nonprofits w/out MD" = c("", "", "$\\checkmark$", "", "", "$\\checkmark$")),
                drop.section="fixef", label = "tab:forprofit_fullsample")
write(tab1b,file="Tables/forprofit_readmort_fullsample.tex")


# table for clinical vs. nonclinical with uncompensated care and CMI as outcomes
tab1c <- etable(uncomp_cmi_analysis[[4]][[1]][["uncomp_did"]], uncomp_cmi_analysis[[4]][[1]][["cmi_did"]], tex=TRUE,
                dict = c("uncomp_care"="Uncomp. Care (Millions)", "cmiv"="Case Mix Index",
                         "did"="NP w/ MD Executive x Post Penalty","ID"="Hospital", "year"="Year"),
                style.tex = style.tex("aer"), fitstat = ~n,
                digits = "r2", title = "Comparison of MD vs. non-MD Nonprofit Uncompensated Care and CMI Regression Results",
                extralines = list("\\textbf{Comparison Group:}" = c("", ""),
                                  "NP w/out MD Executive" = c("$\\checkmark$", "$\\checkmark$")),
                drop.section="fixef", label = "tab:MD_noMD_uncomp_CMI_fullsample")
write(tab1c,file="Tables/MD_noMD_uncompCMI_fullsample.tex")

# table for uncomp care and CMI for all for-profit comparisons
tab1d <- etable(uncomp_cmi_analysis[[1]][[1]][["uncomp_did"]], uncomp_cmi_analysis[[2]][[1]][["uncomp_did"]], uncomp_cmi_analysis[[3]][[1]][["uncomp_did"]],
                uncomp_cmi_analysis[[1]][[1]][["cmi_did"]], uncomp_cmi_analysis[[2]][[1]][["cmi_did"]], uncomp_cmi_analysis[[3]][[1]][["cmi_did"]],
                tex=TRUE,
                dict = c("uncomp_care"="Uncomp. Care (Millions)", "cmiv"="Case Mix Index",
                         "did"="For Profit x Post Penalty","ID"="Hospital", "year"="Year"),
                style.tex = style.tex("aer"), fitstat = ~n,
                digits = "r2", title = "Comparison to For-Profit Uncompensated Care and CMI Regression Results",
                extralines = list("\\textbf{Comparison Group:}" = c("", "", "" ,"", "", ""),
                                  "All Nonprofits" = c("$\\checkmark$", "", "", "$\\checkmark$", "", ""),
                                  "Nonprofits w/ MD" = c("", "$\\checkmark$", "", "", "$\\checkmark$", ""),
                                  "Nonprofits w/out MD" = c("", "", "$\\checkmark$", "", "", "$\\checkmark$")),
                drop.section="fixef", label = "tab:forprofit_uncomp_CMI_fullsample")
write(tab1d,file="Tables/forprofit_uncompCMI_fullsample.tex")


## MATCHED SAMPLE TABLES ####
# make a table for weighted average readmissions and mortality comparing clinical NFP to non-clinical NFP for full sample and matched
tab2a <- etable(readmission_analysis[[4]][[2]][["wa_did"]], mortality_analysis[[4]][[2]][["wa_did"]], tex=TRUE,
                dict = c("weightedavg_read"="Weighted Avg. Readmission Rate", "weightedavg_mort"="Weighted Avg. Mortality Rate",
                         "did"="NP w/ MD Executive x Post Penalty","ID"="Hospital", "year"="Year"),
                style.tex = style.tex("aer"), fitstat = ~n,
                digits = "r2", title = "Comparison of MD vs. non-MD Nonprofit Readmission and Mortality Regression Results, Matched Sample",
                extralines = list("\\textbf{Comparison Group:}" = c("", ""),
                                  "NP w/out MD Executive" = c("$\\checkmark$", "$\\checkmark$")),
                drop.section="fixef", label = "tab:MD_noMD_matchsample")
write(tab2a,file="Tables/MD_noMD_readmort_matchsample.tex")

# table for weighted avg readmission and mortality for all for-profit comparisons
tab2b <- etable(readmission_analysis[[1]][[2]][["wa_did"]], readmission_analysis[[2]][[2]][["wa_did"]], readmission_analysis[[3]][[2]][["wa_did"]],
                mortality_analysis[[1]][[2]][["wa_did"]], mortality_analysis[[2]][[2]][["wa_did"]], mortality_analysis[[3]][[2]][["wa_did"]],
                tex=TRUE,
                dict = c("weightedavg_read"="Weighted Avg. Readmission Rate", "weightedavg_mort"="Weighted Avg. Mortality Rate",
                         "did"="For Profit x Post Penalty","ID"="Hospital", "year"="Year"),
                style.tex = style.tex("aer"), fitstat = ~n,
                digits = "r2", title = "Comparison to For-Profit Regression Results, Matched Sample",
                extralines = list("\\textbf{Comparison Group:}" = c("", "", "" ,"", "", ""),
                                  "All Nonprofits" = c("$\\checkmark$", "", "", "$\\checkmark$", "", ""),
                                  "Nonprofits w/ MD" = c("", "$\\checkmark$", "", "", "$\\checkmark$", ""),
                                  "Nonprofits w/out MD" = c("", "", "$\\checkmark$", "", "", "$\\checkmark$")),
                drop.section="fixef", label = "tab:forprofit_matchsample")
write(tab2b,file="Tables/forprofit_readmort_matchsample.tex")


# table for clinical vs. nonclinical with uncompensated care and CMI as outcomes
tab2c <- etable(uncomp_cmi_analysis[[4]][[2]][["uncomp_did"]], uncomp_cmi_analysis[[4]][[2]][["cmi_did"]], tex=TRUE,
                dict = c("uncomp_care"="Uncomp. Care (Millions)", "cmiv"="Case Mix Index",
                         "did"="NP w/ MD Executive x Post Penalty","ID"="Hospital", "year"="Year"),
                style.tex = style.tex("aer"), fitstat = ~n,
                digits = "r2", title = "Comparison of MD vs. non-MD Nonprofit Uncompensated Care and CMI Regression Results, Matched Sample",
                extralines = list("\\textbf{Comparison Group:}" = c("", ""),
                                  "NP w/out MD Executive" = c("$\\checkmark$", "$\\checkmark$")),
                drop.section="fixef", label = "tab:MD_noMD_uncomp_CMI_matchsample")
write(tab2c,file="Tables/MD_noMD_uncompCMI_matchsample.tex")

# table for uncomp care and CMI for all for-profit comparisons
tab2d <- etable(uncomp_cmi_analysis[[1]][[2]][["uncomp_did"]], uncomp_cmi_analysis[[2]][[2]][["uncomp_did"]], uncomp_cmi_analysis[[3]][[2]][["uncomp_did"]],
                uncomp_cmi_analysis[[1]][[2]][["cmi_did"]], uncomp_cmi_analysis[[2]][[2]][["cmi_did"]], uncomp_cmi_analysis[[3]][[2]][["cmi_did"]],
                tex=TRUE,
                dict = c("uncomp_care"="Uncomp. Care (Millions)", "cmiv"="Case Mix Index",
                         "did"="For Profit x Post Penalty","ID"="Hospital", "year"="Year"),
                style.tex = style.tex("aer"), fitstat = ~n,
                digits = "r2", title = "Comparison to For-Profit Uncompensated Care and CMI Regression Results, Matched Sample",
                extralines = list("\\textbf{Comparison Group:}" = c("", "", "" ,"", "", ""),
                                  "All Nonprofits" = c("$\\checkmark$", "", "", "$\\checkmark$", "", ""),
                                  "Nonprofits w/ MD" = c("", "$\\checkmark$", "", "", "$\\checkmark$", ""),
                                  "Nonprofits w/out MD" = c("", "", "$\\checkmark$", "", "", "$\\checkmark$")),
                drop.section="fixef", label = "tab:forprofit_uncomp_CMI_matchsample")
write(tab2d,file="Tables/forprofit_uncompCMI_matchsample.tex")





## SYNTHETIC DiD TABLES ####

# table for MD vs. non MD readmission and mortality
coef <- c(readmission_analysis[[4]][[3]][["wa_did"]], mortality_analysis[[4]][[3]][["wa_did"]])
se <- c(sqrt(vcov(readmission_analysis[[4]][[3]][["wa_did"]])), sqrt(vcov(mortality_analysis[[4]][[3]][["wa_did"]])))
var <- c("Weighted Avg. Readmission Rate", "Weighted Avg. Mortality Rate")
obs1 <- (summary(readmission_analysis[[4]][[3]][["wa_did"]])$dimensions[["N0"]]+summary(readmission_analysis[[4]][[3]][["wa_did"]])$dimensions[["N1"]])*5
obs2 <- (summary(mortality_analysis[[4]][[3]][["wa_did"]])$dimensions[["N0"]]+summary(mortality_analysis[[4]][[3]][["wa_did"]])$dimensions[["N1"]])*5

obs1 <- as.character(obs1)
obs2 <- as.character(obs2)

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
  mutate(x=c("NP w/ MD Executive x Post Penalty", "")) %>%
  select(x, V1, V2) %>%
  add_row(x="\\textbf{Comparison Group:}") %>%
  add_row(x="NP w/out MD Executive", V1="$\\checkmark$", V2="$\\checkmark$") %>%
  add_row(x="Observations", V1=obs1, V2=obs2)

tab3a <- knitr::kable(estimates, format="latex",
                      row.names = FALSE,
                      col.names = c("", "(1)", "(2)"),
                      booktabs = TRUE,
                      table.envir="table",
                      caption="\\label{tab:MD_noMD_synth}Comparison of MD vs. non-MD Nonprofit Readmissions and Mortality Synthetic DiD Results",
                      escape=F,
                      align=c("l","c","c","c"),
                      position="ht!") %>%
  add_header_above(c(" ", "Weighted Avg. Readmission Rate"=1, "Weighted Avg. Mortality Rate"=1)) %>%
  row_spec(row=0, underline = FALSE)
write(tab3a,file="Tables/MD_noMD_readmort_synth.tex")

# table for for-profit comparisons readmission and mortality
coef <- c(readmission_analysis[[1]][[3]][["wa_did"]], readmission_analysis[[2]][[3]][["wa_did"]], readmission_analysis[[3]][[3]][["wa_did"]], 
          mortality_analysis[[1]][[3]][["wa_did"]], mortality_analysis[[2]][[3]][["wa_did"]], mortality_analysis[[3]][[3]][["wa_did"]])
se <- c(sqrt(vcov(readmission_analysis[[1]][[3]][["wa_did"]])), sqrt(vcov(readmission_analysis[[2]][[3]][["wa_did"]])), sqrt(vcov(readmission_analysis[[3]][[3]][["wa_did"]])),
        sqrt(vcov(mortality_analysis[[1]][[3]][["wa_did"]])), sqrt(vcov(mortality_analysis[[2]][[3]][["wa_did"]])), sqrt(vcov(mortality_analysis[[3]][[3]][["wa_did"]])))

obs1a <- (summary(readmission_analysis[[1]][[3]][["wa_did"]])$dimensions[["N0"]]+summary(readmission_analysis[[1]][[3]][["wa_did"]])$dimensions[["N1"]])*5
obs1b <- (summary(readmission_analysis[[2]][[3]][["wa_did"]])$dimensions[["N0"]]+summary(readmission_analysis[[2]][[3]][["wa_did"]])$dimensions[["N1"]])*5
obs1c <- (summary(readmission_analysis[[3]][[3]][["wa_did"]])$dimensions[["N0"]]+summary(readmission_analysis[[3]][[3]][["wa_did"]])$dimensions[["N1"]])*5
obs2a <- (summary(mortality_analysis[[1]][[3]][["wa_did"]])$dimensions[["N0"]]+summary(mortality_analysis[[1]][[3]][["wa_did"]])$dimensions[["N1"]])*5
obs2b <- (summary(mortality_analysis[[2]][[3]][["wa_did"]])$dimensions[["N0"]]+summary(mortality_analysis[[2]][[3]][["wa_did"]])$dimensions[["N1"]])*5
obs2c <- (summary(mortality_analysis[[3]][[3]][["wa_did"]])$dimensions[["N0"]]+summary(mortality_analysis[[3]][[3]][["wa_did"]])$dimensions[["N1"]])*5

obs1a <- as.character(obs1a)
obs1b <- as.character(obs1b)
obs1c <- as.character(obs1c)
obs2a <- as.character(obs2a)
obs2b <- as.character(obs2b)
obs2c <- as.character(obs2c)

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
  mutate(x=c("For Profit x Post Penalty", "")) %>%
  select(x, V1, V2, V3, V4, V5, V6) %>%
  add_row(x="\\textbf{Comparison Group:}") %>%
  add_row(x="All Nonprofits", V1="$\\checkmark$", V4="$\\checkmark$") %>%
  add_row(x="Nonprofits w/ MD", V2="$\\checkmark$", V5="$\\checkmark$") %>%
  add_row(x="Nonprofits w/out MD", V3="$\\checkmark$", V6="$\\checkmark$") %>%
  add_row(x="Observations", V1=obs1a, V2=obs1b, V3=obs1c, V4=obs2a, V5=obs2b, V6=obs2c)

tab3b <- knitr::kable(estimates, format="latex",
                      row.names = FALSE,
                      col.names = c("", "(1)", "(2)", "(3)", "(1)", "(2)", "(3)"),
                      booktabs = TRUE,
                      table.envir="table",
                      caption="\\label{tab:forprofit_synth} Comparison to For-Profit Readmissions and Mortality Synthetic DiD Results",
                      escape=F,
                      align=c("l","c","c","c","c","c","c"),
                      position="ht!") %>%
  add_header_above(c(" ", "Weighted Avg. Readmission Rate"=3, "Weighted Avg. Mortality Rate"=3)) %>%
  row_spec(row=0, underline = FALSE)
write(tab3b,file="Tables/forprofit_readmort_synth.tex")


# table for MD vs. non MD uncompensated care and CMI
coef <- c(uncomp_cmi_analysis[[4]][[3]][["uncomp_did"]], uncomp_cmi_analysis[[4]][[3]][["cmi_did"]])
se <- c(sqrt(vcov(uncomp_cmi_analysis[[4]][[3]][["uncomp_did"]])), sqrt(vcov(uncomp_cmi_analysis[[4]][[3]][["cmi_did"]])))
obs1 <- (summary(uncomp_cmi_analysis[[4]][[3]][["uncomp_did"]])$dimensions[["N0"]]+summary(uncomp_cmi_analysis[[4]][[3]][["uncomp_did"]])$dimensions[["N1"]])*5
obs2 <- (summary(uncomp_cmi_analysis[[4]][[3]][["cmi_did"]])$dimensions[["N0"]]+summary(uncomp_cmi_analysis[[4]][[3]][["cmi_did"]])$dimensions[["N1"]])*5

obs1 <- as.character(obs1)
obs2 <- as.character(obs2)

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
  mutate(x=c("NP w/ MD Executive x Post Penalty", "")) %>%
  select(x, V1, V2) %>%
  add_row(x="\\textbf{Comparison Group:}") %>%
  add_row(x="NP w/out MD Executive", V1="$\\checkmark$", V2="$\\checkmark$") %>%
  add_row(x="Observations", V1=obs1, V2=obs2)

tab3c <- knitr::kable(estimates, format="latex",
                      row.names = FALSE,
                      col.names = c("", "(1)", "(2)"),
                      booktabs = TRUE,
                      table.envir="table",
                      caption="\\label{tab:MD_noMD_uncompCMI_synth}Comparison of MD vs. non-MD Nonprofit Uncompensated Care and CMI Synthetic DiD Results",
                      escape=F,
                      align=c("l","c","c","c"),
                      position="ht!") %>%
  add_header_above(c(" ", "Uncomp. Care (Millions)"=1, "Case Mix Index"=1)) %>%
  row_spec(row=0, underline = FALSE)
write(tab3c,file="Tables/MD_noMD_uncompCMI_synth.tex")

# table for for-profit comparisons uncompensated care and CMI
coef <- c(uncomp_cmi_analysis[[1]][[3]][["uncomp_did"]], uncomp_cmi_analysis[[2]][[3]][["uncomp_did"]], uncomp_cmi_analysis[[3]][[3]][["uncomp_did"]], 
          uncomp_cmi_analysis[[1]][[3]][["cmi_did"]], uncomp_cmi_analysis[[2]][[3]][["cmi_did"]], uncomp_cmi_analysis[[3]][[3]][["cmi_did"]])
se <- c(sqrt(vcov(uncomp_cmi_analysis[[1]][[3]][["uncomp_did"]])), sqrt(vcov(uncomp_cmi_analysis[[2]][[3]][["uncomp_did"]])), sqrt(vcov(uncomp_cmi_analysis[[3]][[3]][["uncomp_did"]])),
        sqrt(vcov(uncomp_cmi_analysis[[1]][[3]][["cmi_did"]])), sqrt(vcov(uncomp_cmi_analysis[[2]][[3]][["cmi_did"]])), sqrt(vcov(uncomp_cmi_analysis[[3]][[3]][["cmi_did"]])))

obs1a <- (summary(uncomp_cmi_analysis[[1]][[3]][["uncomp_did"]])$dimensions[["N0"]]+summary(uncomp_cmi_analysis[[1]][[3]][["uncomp_did"]])$dimensions[["N1"]])*5
obs1b <- (summary(uncomp_cmi_analysis[[2]][[3]][["uncomp_did"]])$dimensions[["N0"]]+summary(uncomp_cmi_analysis[[2]][[3]][["uncomp_did"]])$dimensions[["N1"]])*5
obs1c <- (summary(uncomp_cmi_analysis[[3]][[3]][["uncomp_did"]])$dimensions[["N0"]]+summary(uncomp_cmi_analysis[[3]][[3]][["uncomp_did"]])$dimensions[["N1"]])*5
obs2a <- (summary(uncomp_cmi_analysis[[1]][[3]][["cmi_did"]])$dimensions[["N0"]]+summary(uncomp_cmi_analysis[[1]][[3]][["cmi_did"]])$dimensions[["N1"]])*5
obs2b <- (summary(uncomp_cmi_analysis[[2]][[3]][["cmi_did"]])$dimensions[["N0"]]+summary(uncomp_cmi_analysis[[2]][[3]][["cmi_did"]])$dimensions[["N1"]])*5
obs2c <- (summary(uncomp_cmi_analysis[[3]][[3]][["cmi_did"]])$dimensions[["N0"]]+summary(uncomp_cmi_analysis[[3]][[3]][["cmi_did"]])$dimensions[["N1"]])*5

obs1a <- as.character(obs1a)
obs1b <- as.character(obs1b)
obs1c <- as.character(obs1c)
obs2a <- as.character(obs2a)
obs2b <- as.character(obs2b)
obs2c <- as.character(obs2c)

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
  mutate(x=c("For Profit x Post Penalty", "")) %>%
  select(x, V1, V2, V3, V4, V5, V6) %>%
  add_row(x="\\textbf{Comparison Group:}") %>%
  add_row(x="All Nonprofits", V1="$\\checkmark$", V4="$\\checkmark$") %>%
  add_row(x="Nonprofits w/ MD", V2="$\\checkmark$", V5="$\\checkmark$") %>%
  add_row(x="Nonprofits w/out MD", V3="$\\checkmark$", V6="$\\checkmark$") %>%
  add_row(x="Observations", V1=obs1a, V2=obs1b, V3=obs1c, V4=obs2a, V5=obs2b, V6=obs2c)

tab3d <- knitr::kable(estimates, format="latex",
                      row.names = FALSE,
                      col.names = c("", "(1)", "(2)", "(3)", "(1)", "(2)", "(3)"),
                      booktabs = TRUE,
                      table.envir="table",
                      caption="\\label{tab:forprofit_uncompCMI_synth} Comparison to For-Profit Uncompensated Care and CMI Synthetic DiD Results",
                      escape=F,
                      align=c("l","c","c","c","c","c","c"),
                      position="ht!") %>%
  add_header_above(c(" ", "Uncomp. Care (Millions)"=3, "Case Mix Index"=3)) %>%
  row_spec(row=0, underline = FALSE)
write(tab3d,file="Tables/forprofit_uncompCMI_synth.tex")






















### ANALYSIS WHERE I DON'T DROP HOSPITALS WITH LEADERSHIP CHANGES: EVER HAS MD####

### OUTCOME: READMISSION ####

readmission_analysis <- lapply(comp, function(x){
  data <- hospital_data 
  
  # create did variable and limit samples accordingly
  if (x=="FP - NFP"){
    data <- data %>%
      mutate(did = post_2012*FP)
  }
  if (x=="FP - has MD"){
    data <- data %>%
      filter(ever_has_md==1 | profit_status=="forprofit") %>%
      mutate(did = post_2012*FP)
  }
  if (x=="FP - no MD"){
    data <- data %>%
      filter(ever_has_md==0 | profit_status=="forprofit") %>%
      mutate(did = post_2012*FP)
  }
  if (x=="has MD - no MD"){
    data <- data %>%
      filter(!is.na(no_num_md_change_2010_2014)) %>%
      mutate(NFP_nomd = ifelse(ever_has_md==0,1,0)) %>%
      mutate(did=post_2012*NFP_nomd)
  }
  
  methods <- lapply(method, function(y){
    if (y=="matched" & x=="FP - NFP"){
      data <- data %>%
        filter(matched_FP_NFP==1)
    }
    if (y=="matched" & x=="FP - has MD"){
      data <- data %>%
        filter(matched_FP_nfpMD==1)
    }
    if (y=="matched" & x=="FP - no MD"){
      data <- data %>%
        filter(matched_FP_nfpnoMD==1)
    }
    if (y=="matched" & x=="has MD - no MD"){
      data <- data %>%
        filter(matched_nfpMD_nfpnoMD==1)
    }
    
    # full and matched analysis
    if (y=="full" | y=="matched"){
      wa_did <- fixest::feols(weightedavg_read ~ did | ID + year, cluster = ~ID, data=data)
      pnem_did <- fixest::feols(rate_pneum_readmission ~ did | ID + year, cluster = ~ID,data=data)
      hf_did <- fixest::feols(rate_heartfailure_readmission ~ did | ID + year, cluster = ~ID,data=data)
      ha_did <- fixest::feols(rate_heartattack_readmission ~ did | ID + year, cluster = ~ID,data=data)
    }
    
    if (y=="synth"){
      wa_data <- data %>%
        select(ID, year, weightedavg_read, did) %>%
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
      wa_data <- panel.matrices(as.data.frame(wa_data))
      wa_did <- synthdid_estimate(wa_data$Y, wa_data$N0, wa_data$T0)
      
      pnem_data <- data %>%
        select(ID, year, rate_pneum_readmission, did) %>%
        na.omit() %>%
        mutate(count=1) %>%
        group_by(ID) %>%
        mutate(min=min(year[did==1])) %>%
        mutate(sum=sum(count)) %>%
        ungroup() %>%
        filter(sum==5) %>%
        filter(min!=2014) %>%
        select(-sum, -count, -min) 
      pnem_data <- panel.matrices(as.data.frame(pnem_data))
      pnem_did <- synthdid_estimate(pnem_data$Y, pnem_data$N0, pnem_data$T0)
      
      hf_data <- data %>%
        select(ID, year, rate_heartfailure_readmission, did) %>%
        na.omit() %>%
        mutate(count=1) %>%
        group_by(ID) %>%
        mutate(min=min(year[did==1])) %>%
        mutate(sum=sum(count)) %>%
        ungroup() %>%
        filter(sum==5) %>%
        filter(min!=2014) %>%
        select(-sum, -count, -min) 
      hf_data <- panel.matrices(as.data.frame(hf_data))
      hf_did <- synthdid_estimate(hf_data$Y, hf_data$N0, hf_data$T0)
      
      ha_data <- data %>%
        select(ID, year, rate_heartattack_readmission, did) %>%
        na.omit() %>%
        mutate(count=1) %>%
        group_by(ID) %>%
        mutate(min=min(year[did==1])) %>%
        mutate(sum=sum(count)) %>%
        ungroup() %>%
        filter(sum==5) %>%
        filter(min!=2014) %>%
        select(-sum, -count, -min) 
      ha_data <- panel.matrices(as.data.frame(ha_data))
      ha_did <- synthdid_estimate(ha_data$Y, ha_data$N0, ha_data$T0)
    }
    
    list(wa_did=wa_did, pnem_did=pnem_did, hf_did=hf_did, ha_did=ha_did)
  })
  
  return(methods)
})

## OUTCOME: MORTALITY ####
mortality_analysis <- lapply(comp, function(x){
  data <- hospital_data 
  
  # create did variable and limit samples accordingly
  if (x=="FP - NFP"){
    data <- data %>%
      mutate(did = post_2012*FP)
  }
  if (x=="FP - has MD"){
    data <- data %>%
      filter(profit_status=="forprofit" | ever_has_md==1) %>%
      mutate(did = post_2012*FP)
  }
  if (x=="FP - no MD"){
    data <- data %>%
      filter(profit_status=="forprofit" | ever_has_md==0) %>%
      mutate(did = post_2012*FP)
  }
  if (x=="has MD - no MD"){
    data <- data %>%
      filter(!is.na(no_num_md_change_2010_2014)) %>%
      mutate(NFP_nomd = ifelse(profit_status=="nonprofit" & ever_has_md==0,1,0)) %>%
      mutate(did=post_2012*NFP_nomd)
  }
  
  methods <- lapply(method, function(y){
    if (y=="matched" & x=="FP - NFP"){
      data <- data %>%
        filter(matched_FP_NFP==1)
    }
    if (y=="matched" & x=="FP - has MD"){
      data <- data %>%
        filter(matched_FP_nfpMD==1)
    }
    if (y=="matched" & x=="FP - no MD"){
      data <- data %>%
        filter(matched_FP_nfpnoMD==1)
    }
    if (y=="matched" & x=="has MD - no MD"){
      data <- data %>%
        filter(matched_nfpMD_nfpnoMD==1)
    }
    
    # full and matched analysis
    if (y=="full" | y=="matched"){
      wa_did <- fixest::feols(weightedavg_mort ~ did | ID + year, cluster = ~ID, data=data)
      pnem_did <- fixest::feols(rate_pneum_mortality ~ did | ID + year, cluster = ~ID,data=data)
      hf_did <- fixest::feols(rate_heartfailure_mortality ~ did | ID + year, cluster = ~ID,data=data)
      ha_did <- fixest::feols(rate_heartattack_mortality ~ did | ID + year, cluster = ~ID,data=data)
    }
    
    if (y=="synth"){
      wa_data <- data %>%
        select(ID, year, weightedavg_mort, did) %>%
        na.omit() %>%
        mutate(count=1) %>%
        group_by(ID) %>%
        mutate(min=min(year[did==1])) %>%
        mutate(sum=sum(count)) %>%
        ungroup() %>%
        filter(sum==5) %>%
        filter(min!=2014) %>%
        select(-sum, -count, -min) 
      wa_data <- panel.matrices(as.data.frame(wa_data))
      wa_did <- synthdid_estimate(wa_data$Y, wa_data$N0, wa_data$T0)
      
      pnem_data <- data %>%
        select(ID, year, rate_pneum_mortality, did) %>%
        na.omit() %>%
        mutate(count=1) %>%
        group_by(ID) %>%
        mutate(min=min(year[did==1])) %>%
        mutate(sum=sum(count)) %>%
        ungroup() %>%
        filter(sum==5) %>%
        filter(min!=2014) %>%
        select(-sum, -count, -min) 
      pnem_data <- panel.matrices(as.data.frame(pnem_data))
      pnem_did <- synthdid_estimate(pnem_data$Y, pnem_data$N0, pnem_data$T0)
      
      hf_data <- data %>%
        select(ID, year, rate_heartfailure_mortality, did) %>%
        na.omit() %>%
        mutate(count=1) %>%
        group_by(ID) %>%
        mutate(min=min(year[did==1])) %>%
        mutate(sum=sum(count)) %>%
        ungroup() %>%
        filter(sum==5) %>%
        filter(min!=2014) %>%
        select(-sum, -count, -min) 
      hf_data <- panel.matrices(as.data.frame(hf_data))
      hf_did <- synthdid_estimate(hf_data$Y, hf_data$N0, hf_data$T0)
      
      ha_data <- data %>%
        select(ID, year, rate_heartattack_mortality, did) %>%
        na.omit() %>%
        mutate(count=1) %>%
        group_by(ID) %>%
        mutate(min=min(year[did==1])) %>%
        mutate(sum=sum(count)) %>%
        ungroup() %>%
        filter(sum==5) %>%
        filter(min!=2014) %>%
        select(-sum, -count, -min) 
      ha_data <- panel.matrices(as.data.frame(ha_data))
      ha_did <- synthdid_estimate(ha_data$Y, ha_data$N0, ha_data$T0)
    }
    
    list(wa_did=wa_did, pnem_did=pnem_did, hf_did=hf_did, ha_did=ha_did)
  })
  
  return(methods)
})

## OUTCOME: UNCOMPENSATED CARE and CMI ####
uncomp_cmi_analysis <- lapply(comp, function(x){
  data <- hospital_data
  
  # create did variable and limit samples accordingly
  if (x=="FP - NFP"){
    data <- data %>%
      mutate(did = post_2012*FP)
  }
  if (x=="FP - has MD"){
    data <- data %>%
      filter(profit_status=="forprofit" | ever_has_md==1) %>%
      mutate(did = post_2012*FP)
  }
  if (x=="FP - no MD"){
    data <- data %>%
      filter(profit_status=="forprofit" | ever_has_md==0) %>%
      mutate(did = post_2012*FP)
  }
  if (x=="has MD - no MD"){
    data <- data %>%
      filter(!is.na(no_num_md_change_2010_2014)) %>%
      mutate(NFP_nevermd = ifelse(profit_status=="nonprofit" & ever_has_md==0,1,0)) %>%
      mutate(did=post_2012*NFP_nevermd)
  }
  
  methods <- lapply(method, function(y){
    if (y=="matched" & x=="FP - NFP"){
      data <- data %>%
        filter(matched_FP_NFP==1)
    }
    if (y=="matched" & x=="FP - has MD"){
      data <- data %>%
        filter(matched_FP_nfpMD==1)
    }
    if (y=="matched" & x=="FP - no MD"){
      data <- data %>%
        filter(matched_FP_nfpnoMD==1)
    }
    if (y=="matched" & x=="has MD - no MD"){
      data <- data %>%
        filter(matched_nfpMD_nfpnoMD==1)
    }
    
    # full and matched analysis
    if (y=="full" | y=="matched"){
      uncomp_did <- fixest::feols(uncomp_care ~ did | ID + year, cluster = ~ID, data=data)
      cmi_did <- fixest::feols(cmiv ~ did | ID + year, cluster = ~ID,data=data)
    }
    
    if (y=="synth"){
      uncomp_data <- data %>%
        distinct(ID, year, uncomp_care, did) %>%
        na.omit() %>%
        group_by(ID, year) %>%
        mutate(count=1) %>%
        mutate(sum=sum(count)) %>%
        ungroup() %>%
        filter(sum==1) %>%
        filter(uncomp_care!=0) %>%
        mutate(count=1) %>%
        group_by(ID) %>%
        mutate(sum=sum(count)) %>%
        mutate(min=min(year[did==1])) %>%
        ungroup() %>%
        filter(sum==5) %>%
        filter(min!=2014) %>%
        select(-sum, -count, -min)
      uncomp_data <- panel.matrices(as.data.frame(uncomp_data))
      uncomp_did <- synthdid_estimate(uncomp_data$Y, uncomp_data$N0, uncomp_data$T0)
      
      cmi_data <- data %>%
        select(ID, year, cmiv, did) %>%
        na.omit() %>%
        mutate(count=1) %>%
        group_by(ID) %>%
        mutate(min=min(year[did==1])) %>%
        mutate(sum=sum(count)) %>%
        ungroup() %>%
        filter(sum==5) %>%
        filter(min!=2014) %>%
        select(-sum, -count, -min) 
      cmi_data <- panel.matrices(as.data.frame(cmi_data))
      cmi_did <- synthdid_estimate(cmi_data$Y, cmi_data$N0, cmi_data$T0)
    }
    
    list(uncomp_did=uncomp_did, cmi_did=cmi_did)
  })
  
  return(methods)
})



## FULL SAMPLE TABLES ####

# make a table for weighted average readmissions and mortality comparing clinical NFP to non-clinical NFP for full sample and matched
tab1a <- etable(readmission_analysis[[4]][[1]][["wa_did"]], mortality_analysis[[4]][[1]][["wa_did"]], tex=TRUE,
                dict = c("weightedavg_read"="Weighted Avg. Readmission Rate", "weightedavg_mort"="Weighted Avg. Mortality Rate",
                         "did"="NP w/ MD Executive x Post Penalty","ID"="Hospital", "year"="Year"),
                style.tex = style.tex("aer"), fitstat = ~n,
                digits = "r2", title = "Comparison of MD vs. non-MD Nonprofit Readmission and Mortality Regression Results",
                extralines = list("\\textbf{Comparison Group:}" = c("", ""),
                                  "NP w/out MD Executive" = c("$\\checkmark$", "$\\checkmark$")),
                drop.section="fixef", label = "tab:MD_noMD_fullsample")
write(tab1a,file="Tables/MD_noMD_readmort_fullsample_everMD.tex")

# table for weighted avg readmission and mortality for all for-profit comparisons
tab1b <- etable(readmission_analysis[[1]][[1]][["wa_did"]], readmission_analysis[[2]][[1]][["wa_did"]], readmission_analysis[[3]][[1]][["wa_did"]],
                mortality_analysis[[1]][[1]][["wa_did"]], mortality_analysis[[2]][[1]][["wa_did"]], mortality_analysis[[3]][[1]][["wa_did"]],
                tex=TRUE,
                dict = c("weightedavg_read"="Weighted Avg. Readmission Rate", "weightedavg_mort"="Weighted Avg. Mortality Rate",
                         "did"="For Profit x Post Penalty","ID"="Hospital", "year"="Year"),
                style.tex = style.tex("aer"), fitstat = ~n,
                digits = "r2", title = "Comparison to For-Profit Regression Results",
                extralines = list("\\textbf{Comparison Group:}" = c("", "", "" ,"", "", ""),
                                  "All Nonprofits" = c("$\\checkmark$", "", "", "$\\checkmark$", "", ""),
                                  "Nonprofits w/ MD" = c("", "$\\checkmark$", "", "", "$\\checkmark$", ""),
                                  "Nonprofits w/out MD" = c("", "", "$\\checkmark$", "", "", "$\\checkmark$")),
                drop.section="fixef", label = "tab:forprofit_fullsample")
write(tab1b,file="Tables/forprofit_readmort_fullsample_everMD.tex")


# table for clinical vs. nonclinical with uncompensated care and CMI as outcomes
tab1c <- etable(uncomp_cmi_analysis[[4]][[1]][["uncomp_did"]], uncomp_cmi_analysis[[4]][[1]][["cmi_did"]], tex=TRUE,
                dict = c("uncomp_care"="Uncomp. Care (Millions)", "cmiv"="Case Mix Index",
                         "did"="NP w/ MD Executive x Post Penalty","ID"="Hospital", "year"="Year"),
                style.tex = style.tex("aer"), fitstat = ~n,
                digits = "r2", title = "Comparison of MD vs. non-MD Nonprofit Uncompensated Care and CMI Regression Results",
                extralines = list("\\textbf{Comparison Group:}" = c("", ""),
                                  "NP w/out MD Executive" = c("$\\checkmark$", "$\\checkmark$")),
                drop.section="fixef", label = "tab:MD_noMD_uncomp_CMI_fullsample")
write(tab1c,file="Tables/MD_noMD_uncompCMI_fullsample_everMD.tex")

# table for uncomp care and CMI for all for-profit comparisons
tab1d <- etable(uncomp_cmi_analysis[[1]][[1]][["uncomp_did"]], uncomp_cmi_analysis[[2]][[1]][["uncomp_did"]], uncomp_cmi_analysis[[3]][[1]][["uncomp_did"]],
                uncomp_cmi_analysis[[1]][[1]][["cmi_did"]], uncomp_cmi_analysis[[2]][[1]][["cmi_did"]], uncomp_cmi_analysis[[3]][[1]][["cmi_did"]],
                tex=TRUE,
                dict = c("uncomp_care"="Uncomp. Care (Millions)", "cmiv"="Case Mix Index",
                         "did"="For Profit x Post Penalty","ID"="Hospital", "year"="Year"),
                style.tex = style.tex("aer"), fitstat = ~n,
                digits = "r2", title = "Comparison to For-Profit Uncompensated Care and CMI Regression Results",
                extralines = list("\\textbf{Comparison Group:}" = c("", "", "" ,"", "", ""),
                                  "All Nonprofits" = c("$\\checkmark$", "", "", "$\\checkmark$", "", ""),
                                  "Nonprofits w/ MD" = c("", "$\\checkmark$", "", "", "$\\checkmark$", ""),
                                  "Nonprofits w/out MD" = c("", "", "$\\checkmark$", "", "", "$\\checkmark$")),
                drop.section="fixef", label = "tab:forprofit_uncomp_CMI_fullsample")
write(tab1d,file="Tables/forprofit_uncompCMI_fullsample_everMD.tex")


## MATCHED SAMPLE TABLES ####
# make a table for weighted average readmissions and mortality comparing clinical NFP to non-clinical NFP for full sample and matched
tab2a <- etable(readmission_analysis[[4]][[2]][["wa_did"]], mortality_analysis[[4]][[2]][["wa_did"]], tex=TRUE,
                dict = c("weightedavg_read"="Weighted Avg. Readmission Rate", "weightedavg_mort"="Weighted Avg. Mortality Rate",
                         "did"="NP w/ MD Executive x Post Penalty","ID"="Hospital", "year"="Year"),
                style.tex = style.tex("aer"), fitstat = ~n,
                digits = "r2", title = "Comparison of MD vs. non-MD Nonprofit Readmission and Mortality Regression Results, Matched Sample",
                extralines = list("\\textbf{Comparison Group:}" = c("", ""),
                                  "NP w/out MD Executive" = c("$\\checkmark$", "$\\checkmark$")),
                drop.section="fixef", label = "tab:MD_noMD_matchsample")
write(tab2a,file="Tables/MD_noMD_readmort_matchsample_everMD.tex")

# table for weighted avg readmission and mortality for all for-profit comparisons
tab2b <- etable(readmission_analysis[[1]][[2]][["wa_did"]], readmission_analysis[[2]][[2]][["wa_did"]], readmission_analysis[[3]][[2]][["wa_did"]],
                mortality_analysis[[1]][[2]][["wa_did"]], mortality_analysis[[2]][[2]][["wa_did"]], mortality_analysis[[3]][[2]][["wa_did"]],
                tex=TRUE,
                dict = c("weightedavg_read"="Weighted Avg. Readmission Rate", "weightedavg_mort"="Weighted Avg. Mortality Rate",
                         "did"="For Profit x Post Penalty","ID"="Hospital", "year"="Year"),
                style.tex = style.tex("aer"), fitstat = ~n,
                digits = "r2", title = "Comparison to For-Profit Regression Results, Matched Sample",
                extralines = list("\\textbf{Comparison Group:}" = c("", "", "" ,"", "", ""),
                                  "All Nonprofits" = c("$\\checkmark$", "", "", "$\\checkmark$", "", ""),
                                  "Nonprofits w/ MD" = c("", "$\\checkmark$", "", "", "$\\checkmark$", ""),
                                  "Nonprofits w/out MD" = c("", "", "$\\checkmark$", "", "", "$\\checkmark$")),
                drop.section="fixef", label = "tab:forprofit_matchsample")
write(tab2b,file="Tables/forprofit_readmort_matchsample_everMD.tex")


# table for clinical vs. nonclinical with uncompensated care and CMI as outcomes
tab2c <- etable(uncomp_cmi_analysis[[4]][[2]][["uncomp_did"]], uncomp_cmi_analysis[[4]][[2]][["cmi_did"]], tex=TRUE,
                dict = c("uncomp_care"="Uncomp. Care (Millions)", "cmiv"="Case Mix Index",
                         "did"="NP w/ MD Executive x Post Penalty","ID"="Hospital", "year"="Year"),
                style.tex = style.tex("aer"), fitstat = ~n,
                digits = "r2", title = "Comparison of MD vs. non-MD Nonprofit Uncompensated Care and CMI Regression Results, Matched Sample",
                extralines = list("\\textbf{Comparison Group:}" = c("", ""),
                                  "NP w/out MD Executive" = c("$\\checkmark$", "$\\checkmark$")),
                drop.section="fixef", label = "tab:MD_noMD_uncomp_CMI_matchsample")
write(tab2c,file="Tables/MD_noMD_uncompCMI_matchsample_everMD.tex")

# table for uncomp care and CMI for all for-profit comparisons
tab2d <- etable(uncomp_cmi_analysis[[1]][[2]][["uncomp_did"]], uncomp_cmi_analysis[[2]][[2]][["uncomp_did"]], uncomp_cmi_analysis[[3]][[2]][["uncomp_did"]],
                uncomp_cmi_analysis[[1]][[2]][["cmi_did"]], uncomp_cmi_analysis[[2]][[2]][["cmi_did"]], uncomp_cmi_analysis[[3]][[2]][["cmi_did"]],
                tex=TRUE,
                dict = c("uncomp_care"="Uncomp. Care (Millions)", "cmiv"="Case Mix Index",
                         "did"="For Profit x Post Penalty","ID"="Hospital", "year"="Year"),
                style.tex = style.tex("aer"), fitstat = ~n,
                digits = "r2", title = "Comparison to For-Profit Uncompensated Care and CMI Regression Results, Matched Sample",
                extralines = list("\\textbf{Comparison Group:}" = c("", "", "" ,"", "", ""),
                                  "All Nonprofits" = c("$\\checkmark$", "", "", "$\\checkmark$", "", ""),
                                  "Nonprofits w/ MD" = c("", "$\\checkmark$", "", "", "$\\checkmark$", ""),
                                  "Nonprofits w/out MD" = c("", "", "$\\checkmark$", "", "", "$\\checkmark$")),
                drop.section="fixef", label = "tab:forprofit_uncomp_CMI_matchsample")
write(tab2d,file="Tables/forprofit_uncompCMI_matchsample_everMD.tex")





## SYNTHETIC DiD TABLES ####

# table for MD vs. non MD readmission and mortality
coef <- c(readmission_analysis[[4]][[3]][["wa_did"]], mortality_analysis[[4]][[3]][["wa_did"]])
se <- c(sqrt(vcov(readmission_analysis[[4]][[3]][["wa_did"]])), sqrt(vcov(mortality_analysis[[4]][[3]][["wa_did"]])))
var <- c("Weighted Avg. Readmission Rate", "Weighted Avg. Mortality Rate")
obs1 <- (summary(readmission_analysis[[4]][[3]][["wa_did"]])$dimensions[["N0"]]+summary(readmission_analysis[[4]][[3]][["wa_did"]])$dimensions[["N1"]])*5
obs2 <- (summary(mortality_analysis[[4]][[3]][["wa_did"]])$dimensions[["N0"]]+summary(mortality_analysis[[4]][[3]][["wa_did"]])$dimensions[["N1"]])*5

obs1 <- as.character(obs1)
obs2 <- as.character(obs2)

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
  mutate(x=c("NP w/ MD Executive x Post Penalty", "")) %>%
  select(x, V1, V2) %>%
  add_row(x="\\textbf{Comparison Group:}") %>%
  add_row(x="NP w/out MD Executive", V1="$\\checkmark$", V2="$\\checkmark$") %>%
  add_row(x="Observations", V1=obs1, V2=obs2)

tab3a <- knitr::kable(estimates, format="latex",
                      row.names = FALSE,
                      col.names = c("", "(1)", "(2)"),
                      booktabs = TRUE,
                      table.envir="table",
                      caption="\\label{tab:MD_noMD_synth}Comparison of MD vs. non-MD Nonprofit Readmissions and Mortality Synthetic DiD Results",
                      escape=F,
                      align=c("l","c","c","c"),
                      position="ht!") %>%
  add_header_above(c(" ", "Weighted Avg. Readmission Rate"=1, "Weighted Avg. Mortality Rate"=1)) %>%
  row_spec(row=0, underline = FALSE)
write(tab3a,file="Tables/MD_noMD_readmort_synth_everMD.tex")

# table for for-profit comparisons readmission and mortality
coef <- c(readmission_analysis[[1]][[3]][["wa_did"]], readmission_analysis[[2]][[3]][["wa_did"]], readmission_analysis[[3]][[3]][["wa_did"]], 
          mortality_analysis[[1]][[3]][["wa_did"]], mortality_analysis[[2]][[3]][["wa_did"]], mortality_analysis[[3]][[3]][["wa_did"]])
se <- c(sqrt(vcov(readmission_analysis[[1]][[3]][["wa_did"]])), sqrt(vcov(readmission_analysis[[2]][[3]][["wa_did"]])), sqrt(vcov(readmission_analysis[[3]][[3]][["wa_did"]])),
        sqrt(vcov(mortality_analysis[[1]][[3]][["wa_did"]])), sqrt(vcov(mortality_analysis[[2]][[3]][["wa_did"]])), sqrt(vcov(mortality_analysis[[3]][[3]][["wa_did"]])))

obs1a <- (summary(readmission_analysis[[1]][[3]][["wa_did"]])$dimensions[["N0"]]+summary(readmission_analysis[[1]][[3]][["wa_did"]])$dimensions[["N1"]])*5
obs1b <- (summary(readmission_analysis[[2]][[3]][["wa_did"]])$dimensions[["N0"]]+summary(readmission_analysis[[2]][[3]][["wa_did"]])$dimensions[["N1"]])*5
obs1c <- (summary(readmission_analysis[[3]][[3]][["wa_did"]])$dimensions[["N0"]]+summary(readmission_analysis[[3]][[3]][["wa_did"]])$dimensions[["N1"]])*5
obs2a <- (summary(mortality_analysis[[1]][[3]][["wa_did"]])$dimensions[["N0"]]+summary(mortality_analysis[[1]][[3]][["wa_did"]])$dimensions[["N1"]])*5
obs2b <- (summary(mortality_analysis[[2]][[3]][["wa_did"]])$dimensions[["N0"]]+summary(mortality_analysis[[2]][[3]][["wa_did"]])$dimensions[["N1"]])*5
obs2c <- (summary(mortality_analysis[[3]][[3]][["wa_did"]])$dimensions[["N0"]]+summary(mortality_analysis[[3]][[3]][["wa_did"]])$dimensions[["N1"]])*5

obs1a <- as.character(obs1a)
obs1b <- as.character(obs1b)
obs1c <- as.character(obs1c)
obs2a <- as.character(obs2a)
obs2b <- as.character(obs2b)
obs2c <- as.character(obs2c)

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
  mutate(x=c("For Profit x Post Penalty", "")) %>%
  select(x, V1, V2, V3, V4, V5, V6) %>%
  add_row(x="\\textbf{Comparison Group:}") %>%
  add_row(x="All Nonprofits", V1="$\\checkmark$", V4="$\\checkmark$") %>%
  add_row(x="Nonprofits w/ MD", V2="$\\checkmark$", V5="$\\checkmark$") %>%
  add_row(x="Nonprofits w/out MD", V3="$\\checkmark$", V6="$\\checkmark$") %>%
  add_row(x="Observations", V1=obs1a, V2=obs1b, V3=obs1c, V4=obs2a, V5=obs2b, V6=obs2c)

tab3b <- knitr::kable(estimates, format="latex",
                      row.names = FALSE,
                      col.names = c("", "(1)", "(2)", "(3)", "(1)", "(2)", "(3)"),
                      booktabs = TRUE,
                      table.envir="table",
                      caption="\\label{tab:forprofit_synth} Comparison to For-Profit Readmissions and Mortality Synthetic DiD Results",
                      escape=F,
                      align=c("l","c","c","c","c","c","c"),
                      position="ht!") %>%
  add_header_above(c(" ", "Weighted Avg. Readmission Rate"=3, "Weighted Avg. Mortality Rate"=3)) %>%
  row_spec(row=0, underline = FALSE)
write(tab3b,file="Tables/forprofit_readmort_synth_everMD.tex")


# table for MD vs. non MD uncompensated care and CMI
coef <- c(uncomp_cmi_analysis[[4]][[3]][["uncomp_did"]], uncomp_cmi_analysis[[4]][[3]][["cmi_did"]])
se <- c(sqrt(vcov(uncomp_cmi_analysis[[4]][[3]][["uncomp_did"]])), sqrt(vcov(uncomp_cmi_analysis[[4]][[3]][["cmi_did"]])))
obs1 <- (summary(uncomp_cmi_analysis[[4]][[3]][["uncomp_did"]])$dimensions[["N0"]]+summary(uncomp_cmi_analysis[[4]][[3]][["uncomp_did"]])$dimensions[["N1"]])*5
obs2 <- (summary(uncomp_cmi_analysis[[4]][[3]][["cmi_did"]])$dimensions[["N0"]]+summary(uncomp_cmi_analysis[[4]][[3]][["cmi_did"]])$dimensions[["N1"]])*5

obs1 <- as.character(obs1)
obs2 <- as.character(obs2)

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
  mutate(x=c("NP w/ MD Executive x Post Penalty", "")) %>%
  select(x, V1, V2) %>%
  add_row(x="\\textbf{Comparison Group:}") %>%
  add_row(x="NP w/out MD Executive", V1="$\\checkmark$", V2="$\\checkmark$") %>%
  add_row(x="Observations", V1=obs1, V2=obs2)

tab3c <- knitr::kable(estimates, format="latex",
                      row.names = FALSE,
                      col.names = c("", "(1)", "(2)"),
                      booktabs = TRUE,
                      table.envir="table",
                      caption="\\label{tab:MD_noMD_uncompCMI_synth}Comparison of MD vs. non-MD Nonprofit Uncompensated Care and CMI Synthetic DiD Results",
                      escape=F,
                      align=c("l","c","c","c"),
                      position="ht!") %>%
  add_header_above(c(" ", "Uncomp. Care (Millions)"=1, "Case Mix Index"=1)) %>%
  row_spec(row=0, underline = FALSE)
write(tab3c,file="Tables/MD_noMD_uncompCMI_synth_everMD.tex")

# table for for-profit comparisons uncompensated care and CMI
coef <- c(uncomp_cmi_analysis[[1]][[3]][["uncomp_did"]], uncomp_cmi_analysis[[2]][[3]][["uncomp_did"]], uncomp_cmi_analysis[[3]][[3]][["uncomp_did"]], 
          uncomp_cmi_analysis[[1]][[3]][["cmi_did"]], uncomp_cmi_analysis[[2]][[3]][["cmi_did"]], uncomp_cmi_analysis[[3]][[3]][["cmi_did"]])
se <- c(sqrt(vcov(uncomp_cmi_analysis[[1]][[3]][["uncomp_did"]])), sqrt(vcov(uncomp_cmi_analysis[[2]][[3]][["uncomp_did"]])), sqrt(vcov(uncomp_cmi_analysis[[3]][[3]][["uncomp_did"]])),
        sqrt(vcov(uncomp_cmi_analysis[[1]][[3]][["cmi_did"]])), sqrt(vcov(uncomp_cmi_analysis[[2]][[3]][["cmi_did"]])), sqrt(vcov(uncomp_cmi_analysis[[3]][[3]][["cmi_did"]])))

obs1a <- (summary(uncomp_cmi_analysis[[1]][[3]][["uncomp_did"]])$dimensions[["N0"]]+summary(uncomp_cmi_analysis[[1]][[3]][["uncomp_did"]])$dimensions[["N1"]])*5
obs1b <- (summary(uncomp_cmi_analysis[[2]][[3]][["uncomp_did"]])$dimensions[["N0"]]+summary(uncomp_cmi_analysis[[2]][[3]][["uncomp_did"]])$dimensions[["N1"]])*5
obs1c <- (summary(uncomp_cmi_analysis[[3]][[3]][["uncomp_did"]])$dimensions[["N0"]]+summary(uncomp_cmi_analysis[[3]][[3]][["uncomp_did"]])$dimensions[["N1"]])*5
obs2a <- (summary(uncomp_cmi_analysis[[1]][[3]][["cmi_did"]])$dimensions[["N0"]]+summary(uncomp_cmi_analysis[[1]][[3]][["cmi_did"]])$dimensions[["N1"]])*5
obs2b <- (summary(uncomp_cmi_analysis[[2]][[3]][["cmi_did"]])$dimensions[["N0"]]+summary(uncomp_cmi_analysis[[2]][[3]][["cmi_did"]])$dimensions[["N1"]])*5
obs2c <- (summary(uncomp_cmi_analysis[[3]][[3]][["cmi_did"]])$dimensions[["N0"]]+summary(uncomp_cmi_analysis[[3]][[3]][["cmi_did"]])$dimensions[["N1"]])*5

obs1a <- as.character(obs1a)
obs1b <- as.character(obs1b)
obs1c <- as.character(obs1c)
obs2a <- as.character(obs2a)
obs2b <- as.character(obs2b)
obs2c <- as.character(obs2c)

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
  mutate(x=c("For Profit x Post Penalty", "")) %>%
  select(x, V1, V2, V3, V4, V5, V6) %>%
  add_row(x="\\textbf{Comparison Group:}") %>%
  add_row(x="All Nonprofits", V1="$\\checkmark$", V4="$\\checkmark$") %>%
  add_row(x="Nonprofits w/ MD", V2="$\\checkmark$", V5="$\\checkmark$") %>%
  add_row(x="Nonprofits w/out MD", V3="$\\checkmark$", V6="$\\checkmark$") %>%
  add_row(x="Observations", V1=obs1a, V2=obs1b, V3=obs1c, V4=obs2a, V5=obs2b, V6=obs2c)

tab3d <- knitr::kable(estimates, format="latex",
                      row.names = FALSE,
                      col.names = c("", "(1)", "(2)", "(3)", "(1)", "(2)", "(3)"),
                      booktabs = TRUE,
                      table.envir="table",
                      caption="\\label{tab:forprofit_uncompCMI_synth} Comparison to For-Profit Uncompensated Care and CMI Synthetic DiD Results",
                      escape=F,
                      align=c("l","c","c","c","c","c","c"),
                      position="ht!") %>%
  add_header_above(c(" ", "Uncomp. Care (Millions)"=3, "Case Mix Index"=3)) %>%
  row_spec(row=0, underline = FALSE)
write(tab3d,file="Tables/forprofit_uncompCMI_synth_everMD.tex")



























### ANALYSIS WHERE I DON'T DROP HOSPITALS WITH LEADERSHIP CHANGES: HAS MD in 2012####

### OUTCOME: READMISSION ####
readmission_analysis <- lapply(comp, function(x){
  data <- hospital_data %>%
    mutate(has_md_2012 = ifelse(year==2012 & has_any_md==1,1,NA)) %>%
    group_by(ID) %>%
    fill(has_md_2012, .direction="downup") %>%
    ungroup() %>%
    mutate(has_md_2012=ifelse(is.na(has_md_2012) & !is.na(no_md_change_2010_2014),0,has_md_2012))
  
  # create did variable and limit samples accordingly
  if (x=="FP - NFP"){
    data <- data %>%
      mutate(did = post_2012*FP)
  }
  if (x=="FP - has MD"){
    data <- data %>%
      filter(has_md_2012==1 | profit_status=="forprofit") %>%
      mutate(did = post_2012*FP)
  }
  if (x=="FP - no MD"){
    data <- data %>%
      filter(has_md_2012==0 | profit_status=="forprofit") %>%
      mutate(did = post_2012*FP)
  }
  if (x=="has MD - no MD"){
    data <- data %>%
      filter(!is.na(no_num_md_change_2010_2014)) %>%
      mutate(NFP_nomd = ifelse(has_md_2012==0,1,0)) %>%
      mutate(did=post_2012*NFP_nomd)
  }
  
  methods <- lapply(method, function(y){
    if (y=="matched" & x=="FP - NFP"){
      data <- data %>%
        filter(matched_FP_NFP==1)
    }
    if (y=="matched" & x=="FP - has MD"){
      data <- data %>%
        filter(matched_FP_nfpMD==1)
    }
    if (y=="matched" & x=="FP - no MD"){
      data <- data %>%
        filter(matched_FP_nfpnoMD==1)
    }
    if (y=="matched" & x=="has MD - no MD"){
      data <- data %>%
        filter(matched_nfpMD_nfpnoMD==1)
    }
    
    # full and matched analysis
    if (y=="full" | y=="matched"){
      wa_did <- fixest::feols(weightedavg_read ~ did | ID + year, cluster = ~ID, data=data)
      pnem_did <- fixest::feols(rate_pneum_readmission ~ did | ID + year, cluster = ~ID,data=data)
      hf_did <- fixest::feols(rate_heartfailure_readmission ~ did | ID + year, cluster = ~ID,data=data)
      ha_did <- fixest::feols(rate_heartattack_readmission ~ did | ID + year, cluster = ~ID,data=data)
    }
    
    if (y=="synth"){
      wa_data <- data %>%
        select(ID, year, weightedavg_read, did) %>%
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
      wa_data <- panel.matrices(as.data.frame(wa_data))
      wa_did <- synthdid_estimate(wa_data$Y, wa_data$N0, wa_data$T0)
      
      pnem_data <- data %>%
        select(ID, year, rate_pneum_readmission, did) %>%
        na.omit() %>%
        mutate(count=1) %>%
        group_by(ID) %>%
        mutate(min=min(year[did==1])) %>%
        mutate(sum=sum(count)) %>%
        ungroup() %>%
        filter(sum==5) %>%
        filter(min!=2014) %>%
        select(-sum, -count, -min) 
      pnem_data <- panel.matrices(as.data.frame(pnem_data))
      pnem_did <- synthdid_estimate(pnem_data$Y, pnem_data$N0, pnem_data$T0)
      
      hf_data <- data %>%
        select(ID, year, rate_heartfailure_readmission, did) %>%
        na.omit() %>%
        mutate(count=1) %>%
        group_by(ID) %>%
        mutate(min=min(year[did==1])) %>%
        mutate(sum=sum(count)) %>%
        ungroup() %>%
        filter(sum==5) %>%
        filter(min!=2014) %>%
        select(-sum, -count, -min) 
      hf_data <- panel.matrices(as.data.frame(hf_data))
      hf_did <- synthdid_estimate(hf_data$Y, hf_data$N0, hf_data$T0)
      
      ha_data <- data %>%
        select(ID, year, rate_heartattack_readmission, did) %>%
        na.omit() %>%
        mutate(count=1) %>%
        group_by(ID) %>%
        mutate(min=min(year[did==1])) %>%
        mutate(sum=sum(count)) %>%
        ungroup() %>%
        filter(sum==5) %>%
        filter(min!=2014) %>%
        select(-sum, -count, -min) 
      ha_data <- panel.matrices(as.data.frame(ha_data))
      ha_did <- synthdid_estimate(ha_data$Y, ha_data$N0, ha_data$T0)
    }
    
    list(wa_did=wa_did, pnem_did=pnem_did, hf_did=hf_did, ha_did=ha_did)
  })
  
  return(methods)
})

## OUTCOME: MORTALITY ####
mortality_analysis <- lapply(comp, function(x){
  data <- hospital_data %>%
    mutate(has_md_2012 = ifelse(year==2012 & has_any_md==1,1,NA)) %>%
    group_by(ID) %>%
    fill(has_md_2012, .direction="downup") %>%
    ungroup() %>%
    mutate(has_md_2012=ifelse(is.na(has_md_2012) & !is.na(no_md_change_2010_2014),0,has_md_2012))
  
  # create did variable and limit samples accordingly
  if (x=="FP - NFP"){
    data <- data %>%
      mutate(did = post_2012*FP)
  }
  if (x=="FP - has MD"){
    data <- data %>%
      filter(profit_status=="forprofit" | has_md_2012==1) %>%
      mutate(did = post_2012*FP)
  }
  if (x=="FP - no MD"){
    data <- data %>%
      filter(profit_status=="forprofit" | has_md_2012==0) %>%
      mutate(did = post_2012*FP)
  }
  if (x=="has MD - no MD"){
    data <- data %>%
      filter(!is.na(no_num_md_change_2010_2014)) %>%
      mutate(NFP_nomd = ifelse(profit_status=="nonprofit" & has_md_2012==0,1,0)) %>%
      mutate(did=post_2012*NFP_nomd)
  }
  
  methods <- lapply(method, function(y){
    if (y=="matched" & x=="FP - NFP"){
      data <- data %>%
        filter(matched_FP_NFP==1)
    }
    if (y=="matched" & x=="FP - has MD"){
      data <- data %>%
        filter(matched_FP_nfpMD==1)
    }
    if (y=="matched" & x=="FP - no MD"){
      data <- data %>%
        filter(matched_FP_nfpnoMD==1)
    }
    if (y=="matched" & x=="has MD - no MD"){
      data <- data %>%
        filter(matched_nfpMD_nfpnoMD==1)
    }
    
    # full and matched analysis
    if (y=="full" | y=="matched"){
      wa_did <- fixest::feols(weightedavg_mort ~ did | ID + year, cluster = ~ID, data=data)
      pnem_did <- fixest::feols(rate_pneum_mortality ~ did | ID + year, cluster = ~ID,data=data)
      hf_did <- fixest::feols(rate_heartfailure_mortality ~ did | ID + year, cluster = ~ID,data=data)
      ha_did <- fixest::feols(rate_heartattack_mortality ~ did | ID + year, cluster = ~ID,data=data)
    }
    
    if (y=="synth"){
      wa_data <- data %>%
        select(ID, year, weightedavg_mort, did) %>%
        na.omit() %>%
        mutate(count=1) %>%
        group_by(ID) %>%
        mutate(min=min(year[did==1])) %>%
        mutate(sum=sum(count)) %>%
        ungroup() %>%
        filter(sum==5) %>%
        filter(min!=2014) %>%
        select(-sum, -count, -min) 
      wa_data <- panel.matrices(as.data.frame(wa_data))
      wa_did <- synthdid_estimate(wa_data$Y, wa_data$N0, wa_data$T0)
      
      pnem_data <- data %>%
        select(ID, year, rate_pneum_mortality, did) %>%
        na.omit() %>%
        mutate(count=1) %>%
        group_by(ID) %>%
        mutate(min=min(year[did==1])) %>%
        mutate(sum=sum(count)) %>%
        ungroup() %>%
        filter(sum==5) %>%
        filter(min!=2014) %>%
        select(-sum, -count, -min) 
      pnem_data <- panel.matrices(as.data.frame(pnem_data))
      pnem_did <- synthdid_estimate(pnem_data$Y, pnem_data$N0, pnem_data$T0)
      
      hf_data <- data %>%
        select(ID, year, rate_heartfailure_mortality, did) %>%
        na.omit() %>%
        mutate(count=1) %>%
        group_by(ID) %>%
        mutate(min=min(year[did==1])) %>%
        mutate(sum=sum(count)) %>%
        ungroup() %>%
        filter(sum==5) %>%
        filter(min!=2014) %>%
        select(-sum, -count, -min) 
      hf_data <- panel.matrices(as.data.frame(hf_data))
      hf_did <- synthdid_estimate(hf_data$Y, hf_data$N0, hf_data$T0)
      
      ha_data <- data %>%
        select(ID, year, rate_heartattack_mortality, did) %>%
        na.omit() %>%
        mutate(count=1) %>%
        group_by(ID) %>%
        mutate(min=min(year[did==1])) %>%
        mutate(sum=sum(count)) %>%
        ungroup() %>%
        filter(sum==5) %>%
        filter(min!=2014) %>%
        select(-sum, -count, -min) 
      ha_data <- panel.matrices(as.data.frame(ha_data))
      ha_did <- synthdid_estimate(ha_data$Y, ha_data$N0, ha_data$T0)
    }
    
    list(wa_did=wa_did, pnem_did=pnem_did, hf_did=hf_did, ha_did=ha_did)
  })
  
  return(methods)
})

## OUTCOME: UNCOMPENSATED CARE and CMI ####
uncomp_cmi_analysis <- lapply(comp, function(x){
  data <- hospital_data %>%
    mutate(has_md_2012 = ifelse(year==2012 & has_any_md==1,1,NA)) %>%
    group_by(ID) %>%
    fill(has_md_2012, .direction="downup") %>%
    ungroup() %>%
    mutate(has_md_2012=ifelse(is.na(has_md_2012) & !is.na(no_md_change_2010_2014),0,has_md_2012))
  
  # create did variable and limit samples accordingly
  if (x=="FP - NFP"){
    data <- data %>%
      mutate(did = post_2012*FP)
  }
  if (x=="FP - has MD"){
    data <- data %>%
      filter(profit_status=="forprofit" | has_md_2012==1) %>%
      mutate(did = post_2012*FP)
  }
  if (x=="FP - no MD"){
    data <- data %>%
      filter(profit_status=="forprofit" | has_md_2012==0) %>%
      mutate(did = post_2012*FP)
  }
  if (x=="has MD - no MD"){
    data <- data %>%
      filter(!is.na(no_num_md_change_2010_2014)) %>%
      mutate(NFP_nevermd = ifelse(profit_status=="nonprofit" & has_md_2012==0,1,0)) %>%
      mutate(did=post_2012*NFP_nevermd)
  }
  
  methods <- lapply(method, function(y){
    if (y=="matched" & x=="FP - NFP"){
      data <- data %>%
        filter(matched_FP_NFP==1)
    }
    if (y=="matched" & x=="FP - has MD"){
      data <- data %>%
        filter(matched_FP_nfpMD==1)
    }
    if (y=="matched" & x=="FP - no MD"){
      data <- data %>%
        filter(matched_FP_nfpnoMD==1)
    }
    if (y=="matched" & x=="has MD - no MD"){
      data <- data %>%
        filter(matched_nfpMD_nfpnoMD==1)
    }
    
    # full and matched analysis
    if (y=="full" | y=="matched"){
      uncomp_did <- fixest::feols(uncomp_care ~ did | ID + year, cluster = ~ID, data=data)
      cmi_did <- fixest::feols(cmiv ~ did | ID + year, cluster = ~ID,data=data)
    }
    
    if (y=="synth"){
      uncomp_data <- data %>%
        distinct(ID, year, uncomp_care, did) %>%
        na.omit() %>%
        group_by(ID, year) %>%
        mutate(count=1) %>%
        mutate(sum=sum(count)) %>%
        ungroup() %>%
        filter(sum==1) %>%
        filter(uncomp_care!=0) %>%
        mutate(count=1) %>%
        group_by(ID) %>%
        mutate(sum=sum(count)) %>%
        mutate(min=min(year[did==1])) %>%
        ungroup() %>%
        filter(sum==5) %>%
        filter(min!=2014) %>%
        select(-sum, -count, -min)
      uncomp_data <- panel.matrices(as.data.frame(uncomp_data))
      uncomp_did <- synthdid_estimate(uncomp_data$Y, uncomp_data$N0, uncomp_data$T0)
      
      cmi_data <- data %>%
        select(ID, year, cmiv, did) %>%
        na.omit() %>%
        mutate(count=1) %>%
        group_by(ID) %>%
        mutate(min=min(year[did==1])) %>%
        mutate(sum=sum(count)) %>%
        ungroup() %>%
        filter(sum==5) %>%
        filter(min!=2014) %>%
        select(-sum, -count, -min) 
      cmi_data <- panel.matrices(as.data.frame(cmi_data))
      cmi_did <- synthdid_estimate(cmi_data$Y, cmi_data$N0, cmi_data$T0)
    }
    
    list(uncomp_did=uncomp_did, cmi_did=cmi_did)
  })
  
  return(methods)
})



## FULL SAMPLE TABLES ####

# make a table for weighted average readmissions and mortality comparing clinical NFP to non-clinical NFP for full sample and matched
tab1a <- etable(readmission_analysis[[4]][[1]][["wa_did"]], mortality_analysis[[4]][[1]][["wa_did"]], tex=TRUE,
                dict = c("weightedavg_read"="Weighted Avg. Readmission Rate", "weightedavg_mort"="Weighted Avg. Mortality Rate",
                         "did"="NP w/ MD Executive x Post Penalty","ID"="Hospital", "year"="Year"),
                style.tex = style.tex("aer"), fitstat = ~n,
                digits = "r2", title = "Comparison of MD vs. non-MD Nonprofit Readmission and Mortality Regression Results",
                extralines = list("\\textbf{Comparison Group:}" = c("", ""),
                                  "NP w/out MD Executive" = c("$\\checkmark$", "$\\checkmark$")),
                drop.section="fixef", label = "tab:MD_noMD_fullsample")
write(tab1a,file="Tables/MD_noMD_readmort_fullsample_2012MD.tex")

# table for weighted avg readmission and mortality for all for-profit comparisons
tab1b <- etable(readmission_analysis[[1]][[1]][["wa_did"]], readmission_analysis[[2]][[1]][["wa_did"]], readmission_analysis[[3]][[1]][["wa_did"]],
                mortality_analysis[[1]][[1]][["wa_did"]], mortality_analysis[[2]][[1]][["wa_did"]], mortality_analysis[[3]][[1]][["wa_did"]],
                tex=TRUE,
                dict = c("weightedavg_read"="Weighted Avg. Readmission Rate", "weightedavg_mort"="Weighted Avg. Mortality Rate",
                         "did"="For Profit x Post Penalty","ID"="Hospital", "year"="Year"),
                style.tex = style.tex("aer"), fitstat = ~n,
                digits = "r2", title = "Comparison to For-Profit Regression Results",
                extralines = list("\\textbf{Comparison Group:}" = c("", "", "" ,"", "", ""),
                                  "All Nonprofits" = c("$\\checkmark$", "", "", "$\\checkmark$", "", ""),
                                  "Nonprofits w/ MD" = c("", "$\\checkmark$", "", "", "$\\checkmark$", ""),
                                  "Nonprofits w/out MD" = c("", "", "$\\checkmark$", "", "", "$\\checkmark$")),
                drop.section="fixef", label = "tab:forprofit_fullsample")
write(tab1b,file="Tables/forprofit_readmort_fullsample_2012MD.tex")


# table for clinical vs. nonclinical with uncompensated care and CMI as outcomes
tab1c <- etable(uncomp_cmi_analysis[[4]][[1]][["uncomp_did"]], uncomp_cmi_analysis[[4]][[1]][["cmi_did"]], tex=TRUE,
                dict = c("uncomp_care"="Uncomp. Care (Millions)", "cmiv"="Case Mix Index",
                         "did"="NP w/ MD Executive x Post Penalty","ID"="Hospital", "year"="Year"),
                style.tex = style.tex("aer"), fitstat = ~n,
                digits = "r2", title = "Comparison of MD vs. non-MD Nonprofit Uncompensated Care and CMI Regression Results",
                extralines = list("\\textbf{Comparison Group:}" = c("", ""),
                                  "NP w/out MD Executive" = c("$\\checkmark$", "$\\checkmark$")),
                drop.section="fixef", label = "tab:MD_noMD_uncomp_CMI_fullsample")
write(tab1c,file="Tables/MD_noMD_uncompCMI_fullsample_2012MD.tex")

# table for uncomp care and CMI for all for-profit comparisons
tab1d <- etable(uncomp_cmi_analysis[[1]][[1]][["uncomp_did"]], uncomp_cmi_analysis[[2]][[1]][["uncomp_did"]], uncomp_cmi_analysis[[3]][[1]][["uncomp_did"]],
                uncomp_cmi_analysis[[1]][[1]][["cmi_did"]], uncomp_cmi_analysis[[2]][[1]][["cmi_did"]], uncomp_cmi_analysis[[3]][[1]][["cmi_did"]],
                tex=TRUE,
                dict = c("uncomp_care"="Uncomp. Care (Millions)", "cmiv"="Case Mix Index",
                         "did"="For Profit x Post Penalty","ID"="Hospital", "year"="Year"),
                style.tex = style.tex("aer"), fitstat = ~n,
                digits = "r2", title = "Comparison to For-Profit Uncompensated Care and CMI Regression Results",
                extralines = list("\\textbf{Comparison Group:}" = c("", "", "" ,"", "", ""),
                                  "All Nonprofits" = c("$\\checkmark$", "", "", "$\\checkmark$", "", ""),
                                  "Nonprofits w/ MD" = c("", "$\\checkmark$", "", "", "$\\checkmark$", ""),
                                  "Nonprofits w/out MD" = c("", "", "$\\checkmark$", "", "", "$\\checkmark$")),
                drop.section="fixef", label = "tab:forprofit_uncomp_CMI_fullsample")
write(tab1d,file="Tables/forprofit_uncompCMI_fullsample_2012MD.tex")


## MATCHED SAMPLE TABLES ####
# make a table for weighted average readmissions and mortality comparing clinical NFP to non-clinical NFP for full sample and matched
tab2a <- etable(readmission_analysis[[4]][[2]][["wa_did"]], mortality_analysis[[4]][[2]][["wa_did"]], tex=TRUE,
                dict = c("weightedavg_read"="Weighted Avg. Readmission Rate", "weightedavg_mort"="Weighted Avg. Mortality Rate",
                         "did"="NP w/ MD Executive x Post Penalty","ID"="Hospital", "year"="Year"),
                style.tex = style.tex("aer"), fitstat = ~n,
                digits = "r2", title = "Comparison of MD vs. non-MD Nonprofit Readmission and Mortality Regression Results, Matched Sample",
                extralines = list("\\textbf{Comparison Group:}" = c("", ""),
                                  "NP w/out MD Executive" = c("$\\checkmark$", "$\\checkmark$")),
                drop.section="fixef", label = "tab:MD_noMD_matchsample")
write(tab2a,file="Tables/MD_noMD_readmort_matchsample_2012MD.tex")

# table for weighted avg readmission and mortality for all for-profit comparisons
tab2b <- etable(readmission_analysis[[1]][[2]][["wa_did"]], readmission_analysis[[2]][[2]][["wa_did"]], readmission_analysis[[3]][[2]][["wa_did"]],
                mortality_analysis[[1]][[2]][["wa_did"]], mortality_analysis[[2]][[2]][["wa_did"]], mortality_analysis[[3]][[2]][["wa_did"]],
                tex=TRUE,
                dict = c("weightedavg_read"="Weighted Avg. Readmission Rate", "weightedavg_mort"="Weighted Avg. Mortality Rate",
                         "did"="For Profit x Post Penalty","ID"="Hospital", "year"="Year"),
                style.tex = style.tex("aer"), fitstat = ~n,
                digits = "r2", title = "Comparison to For-Profit Regression Results, Matched Sample",
                extralines = list("\\textbf{Comparison Group:}" = c("", "", "" ,"", "", ""),
                                  "All Nonprofits" = c("$\\checkmark$", "", "", "$\\checkmark$", "", ""),
                                  "Nonprofits w/ MD" = c("", "$\\checkmark$", "", "", "$\\checkmark$", ""),
                                  "Nonprofits w/out MD" = c("", "", "$\\checkmark$", "", "", "$\\checkmark$")),
                drop.section="fixef", label = "tab:forprofit_matchsample")
write(tab2b,file="Tables/forprofit_readmort_matchsample_2012MD.tex")


# table for clinical vs. nonclinical with uncompensated care and CMI as outcomes
tab2c <- etable(uncomp_cmi_analysis[[4]][[2]][["uncomp_did"]], uncomp_cmi_analysis[[4]][[2]][["cmi_did"]], tex=TRUE,
                dict = c("uncomp_care"="Uncomp. Care (Millions)", "cmiv"="Case Mix Index",
                         "did"="NP w/ MD Executive x Post Penalty","ID"="Hospital", "year"="Year"),
                style.tex = style.tex("aer"), fitstat = ~n,
                digits = "r2", title = "Comparison of MD vs. non-MD Nonprofit Uncompensated Care and CMI Regression Results, Matched Sample",
                extralines = list("\\textbf{Comparison Group:}" = c("", ""),
                                  "NP w/out MD Executive" = c("$\\checkmark$", "$\\checkmark$")),
                drop.section="fixef", label = "tab:MD_noMD_uncomp_CMI_matchsample")
write(tab2c,file="Tables/MD_noMD_uncompCMI_matchsample_2012MD.tex")

# table for uncomp care and CMI for all for-profit comparisons
tab2d <- etable(uncomp_cmi_analysis[[1]][[2]][["uncomp_did"]], uncomp_cmi_analysis[[2]][[2]][["uncomp_did"]], uncomp_cmi_analysis[[3]][[2]][["uncomp_did"]],
                uncomp_cmi_analysis[[1]][[2]][["cmi_did"]], uncomp_cmi_analysis[[2]][[2]][["cmi_did"]], uncomp_cmi_analysis[[3]][[2]][["cmi_did"]],
                tex=TRUE,
                dict = c("uncomp_care"="Uncomp. Care (Millions)", "cmiv"="Case Mix Index",
                         "did"="For Profit x Post Penalty","ID"="Hospital", "year"="Year"),
                style.tex = style.tex("aer"), fitstat = ~n,
                digits = "r2", title = "Comparison to For-Profit Uncompensated Care and CMI Regression Results, Matched Sample",
                extralines = list("\\textbf{Comparison Group:}" = c("", "", "" ,"", "", ""),
                                  "All Nonprofits" = c("$\\checkmark$", "", "", "$\\checkmark$", "", ""),
                                  "Nonprofits w/ MD" = c("", "$\\checkmark$", "", "", "$\\checkmark$", ""),
                                  "Nonprofits w/out MD" = c("", "", "$\\checkmark$", "", "", "$\\checkmark$")),
                drop.section="fixef", label = "tab:forprofit_uncomp_CMI_matchsample")
write(tab2d,file="Tables/forprofit_uncompCMI_matchsample_2012MD.tex")





## SYNTHETIC DiD TABLES ####

# table for MD vs. non MD readmission and mortality
coef <- c(readmission_analysis[[4]][[3]][["wa_did"]], mortality_analysis[[4]][[3]][["wa_did"]])
se <- c(sqrt(vcov(readmission_analysis[[4]][[3]][["wa_did"]])), sqrt(vcov(mortality_analysis[[4]][[3]][["wa_did"]])))
var <- c("Weighted Avg. Readmission Rate", "Weighted Avg. Mortality Rate")
obs1 <- (summary(readmission_analysis[[4]][[3]][["wa_did"]])$dimensions[["N0"]]+summary(readmission_analysis[[4]][[3]][["wa_did"]])$dimensions[["N1"]])*5
obs2 <- (summary(mortality_analysis[[4]][[3]][["wa_did"]])$dimensions[["N0"]]+summary(mortality_analysis[[4]][[3]][["wa_did"]])$dimensions[["N1"]])*5

obs1 <- as.character(obs1)
obs2 <- as.character(obs2)

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
  mutate(x=c("NP w/ MD Executive x Post Penalty", "")) %>%
  select(x, V1, V2) %>%
  add_row(x="\\textbf{Comparison Group:}") %>%
  add_row(x="NP w/out MD Executive", V1="$\\checkmark$", V2="$\\checkmark$") %>%
  add_row(x="Observations", V1=obs1, V2=obs2)

tab3a <- knitr::kable(estimates, format="latex",
                      row.names = FALSE,
                      col.names = c("", "(1)", "(2)"),
                      booktabs = TRUE,
                      table.envir="table",
                      caption="\\label{tab:MD_noMD_synth}Comparison of MD vs. non-MD Nonprofit Readmissions and Mortality Synthetic DiD Results",
                      escape=F,
                      align=c("l","c","c","c"),
                      position="ht!") %>%
  add_header_above(c(" ", "Weighted Avg. Readmission Rate"=1, "Weighted Avg. Mortality Rate"=1)) %>%
  row_spec(row=0, underline = FALSE)
write(tab3a,file="Tables/MD_noMD_readmort_synth_2012MD.tex")

# table for for-profit comparisons readmission and mortality
coef <- c(readmission_analysis[[1]][[3]][["wa_did"]], readmission_analysis[[2]][[3]][["wa_did"]], readmission_analysis[[3]][[3]][["wa_did"]], 
          mortality_analysis[[1]][[3]][["wa_did"]], mortality_analysis[[2]][[3]][["wa_did"]], mortality_analysis[[3]][[3]][["wa_did"]])
se <- c(sqrt(vcov(readmission_analysis[[1]][[3]][["wa_did"]])), sqrt(vcov(readmission_analysis[[2]][[3]][["wa_did"]])), sqrt(vcov(readmission_analysis[[3]][[3]][["wa_did"]])),
        sqrt(vcov(mortality_analysis[[1]][[3]][["wa_did"]])), sqrt(vcov(mortality_analysis[[2]][[3]][["wa_did"]])), sqrt(vcov(mortality_analysis[[3]][[3]][["wa_did"]])))

obs1a <- (summary(readmission_analysis[[1]][[3]][["wa_did"]])$dimensions[["N0"]]+summary(readmission_analysis[[1]][[3]][["wa_did"]])$dimensions[["N1"]])*5
obs1b <- (summary(readmission_analysis[[2]][[3]][["wa_did"]])$dimensions[["N0"]]+summary(readmission_analysis[[2]][[3]][["wa_did"]])$dimensions[["N1"]])*5
obs1c <- (summary(readmission_analysis[[3]][[3]][["wa_did"]])$dimensions[["N0"]]+summary(readmission_analysis[[3]][[3]][["wa_did"]])$dimensions[["N1"]])*5
obs2a <- (summary(mortality_analysis[[1]][[3]][["wa_did"]])$dimensions[["N0"]]+summary(mortality_analysis[[1]][[3]][["wa_did"]])$dimensions[["N1"]])*5
obs2b <- (summary(mortality_analysis[[2]][[3]][["wa_did"]])$dimensions[["N0"]]+summary(mortality_analysis[[2]][[3]][["wa_did"]])$dimensions[["N1"]])*5
obs2c <- (summary(mortality_analysis[[3]][[3]][["wa_did"]])$dimensions[["N0"]]+summary(mortality_analysis[[3]][[3]][["wa_did"]])$dimensions[["N1"]])*5

obs1a <- as.character(obs1a)
obs1b <- as.character(obs1b)
obs1c <- as.character(obs1c)
obs2a <- as.character(obs2a)
obs2b <- as.character(obs2b)
obs2c <- as.character(obs2c)

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
  mutate(x=c("For Profit x Post Penalty", "")) %>%
  select(x, V1, V2, V3, V4, V5, V6) %>%
  add_row(x="\\textbf{Comparison Group:}") %>%
  add_row(x="All Nonprofits", V1="$\\checkmark$", V4="$\\checkmark$") %>%
  add_row(x="Nonprofits w/ MD", V2="$\\checkmark$", V5="$\\checkmark$") %>%
  add_row(x="Nonprofits w/out MD", V3="$\\checkmark$", V6="$\\checkmark$") %>%
  add_row(x="Observations", V1=obs1a, V2=obs1b, V3=obs1c, V4=obs2a, V5=obs2b, V6=obs2c)

tab3b <- knitr::kable(estimates, format="latex",
                      row.names = FALSE,
                      col.names = c("", "(1)", "(2)", "(3)", "(1)", "(2)", "(3)"),
                      booktabs = TRUE,
                      table.envir="table",
                      caption="\\label{tab:forprofit_synth} Comparison to For-Profit Readmissions and Mortality Synthetic DiD Results",
                      escape=F,
                      align=c("l","c","c","c","c","c","c"),
                      position="ht!") %>%
  add_header_above(c(" ", "Weighted Avg. Readmission Rate"=3, "Weighted Avg. Mortality Rate"=3)) %>%
  row_spec(row=0, underline = FALSE)
write(tab3b,file="Tables/forprofit_readmort_synth_2012MD.tex")


# table for MD vs. non MD uncompensated care and CMI
coef <- c(uncomp_cmi_analysis[[4]][[3]][["uncomp_did"]], uncomp_cmi_analysis[[4]][[3]][["cmi_did"]])
se <- c(sqrt(vcov(uncomp_cmi_analysis[[4]][[3]][["uncomp_did"]])), sqrt(vcov(uncomp_cmi_analysis[[4]][[3]][["cmi_did"]])))
obs1 <- (summary(uncomp_cmi_analysis[[4]][[3]][["uncomp_did"]])$dimensions[["N0"]]+summary(uncomp_cmi_analysis[[4]][[3]][["uncomp_did"]])$dimensions[["N1"]])*5
obs2 <- (summary(uncomp_cmi_analysis[[4]][[3]][["cmi_did"]])$dimensions[["N0"]]+summary(uncomp_cmi_analysis[[4]][[3]][["cmi_did"]])$dimensions[["N1"]])*5

obs1 <- as.character(obs1)
obs2 <- as.character(obs2)

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
  mutate(x=c("NP w/ MD Executive x Post Penalty", "")) %>%
  select(x, V1, V2) %>%
  add_row(x="\\textbf{Comparison Group:}") %>%
  add_row(x="NP w/out MD Executive", V1="$\\checkmark$", V2="$\\checkmark$") %>%
  add_row(x="Observations", V1=obs1, V2=obs2)

tab3c <- knitr::kable(estimates, format="latex",
                      row.names = FALSE,
                      col.names = c("", "(1)", "(2)"),
                      booktabs = TRUE,
                      table.envir="table",
                      caption="\\label{tab:MD_noMD_uncompCMI_synth}Comparison of MD vs. non-MD Nonprofit Uncompensated Care and CMI Synthetic DiD Results",
                      escape=F,
                      align=c("l","c","c","c"),
                      position="ht!") %>%
  add_header_above(c(" ", "Uncomp. Care (Millions)"=1, "Case Mix Index"=1)) %>%
  row_spec(row=0, underline = FALSE)
write(tab3c,file="Tables/MD_noMD_uncompCMI_synth_2012MD.tex")

# table for for-profit comparisons uncompensated care and CMI
coef <- c(uncomp_cmi_analysis[[1]][[3]][["uncomp_did"]], uncomp_cmi_analysis[[2]][[3]][["uncomp_did"]], uncomp_cmi_analysis[[3]][[3]][["uncomp_did"]], 
          uncomp_cmi_analysis[[1]][[3]][["cmi_did"]], uncomp_cmi_analysis[[2]][[3]][["cmi_did"]], uncomp_cmi_analysis[[3]][[3]][["cmi_did"]])
se <- c(sqrt(vcov(uncomp_cmi_analysis[[1]][[3]][["uncomp_did"]])), sqrt(vcov(uncomp_cmi_analysis[[2]][[3]][["uncomp_did"]])), sqrt(vcov(uncomp_cmi_analysis[[3]][[3]][["uncomp_did"]])),
        sqrt(vcov(uncomp_cmi_analysis[[1]][[3]][["cmi_did"]])), sqrt(vcov(uncomp_cmi_analysis[[2]][[3]][["cmi_did"]])), sqrt(vcov(uncomp_cmi_analysis[[3]][[3]][["cmi_did"]])))

obs1a <- (summary(uncomp_cmi_analysis[[1]][[3]][["uncomp_did"]])$dimensions[["N0"]]+summary(uncomp_cmi_analysis[[1]][[3]][["uncomp_did"]])$dimensions[["N1"]])*5
obs1b <- (summary(uncomp_cmi_analysis[[2]][[3]][["uncomp_did"]])$dimensions[["N0"]]+summary(uncomp_cmi_analysis[[2]][[3]][["uncomp_did"]])$dimensions[["N1"]])*5
obs1c <- (summary(uncomp_cmi_analysis[[3]][[3]][["uncomp_did"]])$dimensions[["N0"]]+summary(uncomp_cmi_analysis[[3]][[3]][["uncomp_did"]])$dimensions[["N1"]])*5
obs2a <- (summary(uncomp_cmi_analysis[[1]][[3]][["cmi_did"]])$dimensions[["N0"]]+summary(uncomp_cmi_analysis[[1]][[3]][["cmi_did"]])$dimensions[["N1"]])*5
obs2b <- (summary(uncomp_cmi_analysis[[2]][[3]][["cmi_did"]])$dimensions[["N0"]]+summary(uncomp_cmi_analysis[[2]][[3]][["cmi_did"]])$dimensions[["N1"]])*5
obs2c <- (summary(uncomp_cmi_analysis[[3]][[3]][["cmi_did"]])$dimensions[["N0"]]+summary(uncomp_cmi_analysis[[3]][[3]][["cmi_did"]])$dimensions[["N1"]])*5

obs1a <- as.character(obs1a)
obs1b <- as.character(obs1b)
obs1c <- as.character(obs1c)
obs2a <- as.character(obs2a)
obs2b <- as.character(obs2b)
obs2c <- as.character(obs2c)

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
  mutate(x=c("For Profit x Post Penalty", "")) %>%
  select(x, V1, V2, V3, V4, V5, V6) %>%
  add_row(x="\\textbf{Comparison Group:}") %>%
  add_row(x="All Nonprofits", V1="$\\checkmark$", V4="$\\checkmark$") %>%
  add_row(x="Nonprofits w/ MD", V2="$\\checkmark$", V5="$\\checkmark$") %>%
  add_row(x="Nonprofits w/out MD", V3="$\\checkmark$", V6="$\\checkmark$") %>%
  add_row(x="Observations", V1=obs1a, V2=obs1b, V3=obs1c, V4=obs2a, V5=obs2b, V6=obs2c)

tab3d <- knitr::kable(estimates, format="latex",
                      row.names = FALSE,
                      col.names = c("", "(1)", "(2)", "(3)", "(1)", "(2)", "(3)"),
                      booktabs = TRUE,
                      table.envir="table",
                      caption="\\label{tab:forprofit_uncompCMI_synth} Comparison to For-Profit Uncompensated Care and CMI Synthetic DiD Results",
                      escape=F,
                      align=c("l","c","c","c","c","c","c"),
                      position="ht!") %>%
  add_header_above(c(" ", "Uncomp. Care (Millions)"=3, "Case Mix Index"=3)) %>%
  row_spec(row=0, underline = FALSE)
write(tab3d,file="Tables/forprofit_uncompCMI_synth_2012MD.tex")

































# effect of the policy on MD change ####
change_data <- hospital_data %>%
  mutate(yr2010=ifelse(year==2010,1,0),
         yr2011=ifelse(year==2011,1,0),
         yr2012=ifelse(year==2012,1,0),
         yr2013=ifelse(year==2013,1,0),
         yr2014=ifelse(year==2014,1,0)) %>%
  filter(!is.na(no_num_md_change_2010_2014)) %>%
  mutate(post_2011=ifelse(year>=2011,1,0)) %>%
  mutate(did=post_2011*ever_penalized)

# 1. Are changes concentrated around 2011-2012? 
md_change_year_did <- fixest::feols(md_change ~ yr2011 + yr2012 + yr2013 + yr2014 | ID, cluster = ~ID, data=change_data)
num_md_change_year_did <- fixest::feols(num_doc_change ~ yr2011 + yr2012 + yr2013 + yr2014 | ID, cluster = ~ID, data=change_data)

md_change_did <- fixest::feols(md_change ~ did | ID + year, cluster = ~ID, data=change_data)
num_md_change_did <- fixest::feols(num_doc_change ~ did | ID + year, cluster = ~ID, data=change_data)


change_analysis_tab <- etable(md_change_year_did, md_change_did, num_md_change_year_did, num_md_change_did, tex=TRUE,
                              dict = c("yr2011"="2011", "yr2012"="2012", "yr2013"="2013", "yr2014"="2014", "did"="Ever Penalized x Post 2011",
                                       "ID"="Hospital", "year"="Year", "md_change"="Change in Any MD", "num_doc_change"="Change in Num. MDs"),
                              style.tex = style.tex("aer"), fitstat = ~n,
                              digits = "r2", title = "Leadership Change Correlation with Penalty",
                              drop.section="fixef", label = "tab:change_analysis")
write(change_analysis_tab, "Tables/change_analysis.tex")