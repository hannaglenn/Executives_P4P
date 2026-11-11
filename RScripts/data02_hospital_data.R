library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(purrr)

source("./RScripts/paths.R")
source("./RScripts/00_functions.R")      # pad_ccn() and the other shared functions
source("./RScripts/stable_sample.R")

### Script 2: build the hospital-year data set
### Leadership data (script 1) + AHA + HCRIS + Hospital Compare / HCAHPS outcomes + HRRP penalty status.
### Outputs: hospital_data.rds (hospital-year panel) and hosp_key.rds (ID / EIN / CCN by year).

# CMS provider numbers (CCNs) are 6 characters with leading zeros. data04 now pads them, so every other source is padded the same way
# (pad_ccn, from 00_functions.R) before joining: a numeric or unpadded id would silently fail to match.

# read in hospital exec data
# NOTE: this is the EXEC file only. Board variables (board_size, board_perc_md, ...) live in hospital_leader_data.rds
# (exec + board joined, saved at the end of data01). Read that file instead if you want them in hospital_data.
hospital_leader_data <- readRDS(paste0(created_data_path, "/hospital_exec_data.rds"))

# read in cw
cw <- readRDS(paste0(created_data_path, "/updated_ein_aha_cw.rds"))

hospital_data <- hospital_leader_data %>% left_join(cw)

# one row per hospital (EIN) per year is assumed everywhere below.
# If the crosswalk maps one EIN to several AHA ids, this shows it -- fix the crosswalk before going on.
dup_ein_years <- hospital_data %>% count(Filer.EIN, TaxYr) %>% filter(n > 1)
if (nrow(dup_ein_years) > 0) {
  warning(nrow(dup_ein_years), " EIN-years appear more than once after joining the AHA crosswalk")
  print(head(dup_ein_years))
}

hospital_data <- hospital_data %>%
  mutate(any_clinician_vp = ifelse(exec_perc_clinical_with_vp>0,1,0),
         any_clinician_no_vp = ifelse(exec_perc_clinical_no_vp>0,1,0),
         any_md_vp = ifelse(exec_perc_md_with_vp>0,1,0),
         any_md_no_vp = ifelse(exec_perc_md_no_vp>0,1,0))

##### read in AHA data and create AHA variables ####
AHA <- read_csv(paste0(raw_data_path, "/AHAdata_20052023.csv"))

# CNTRL = AHA control (ownership) code, used for profit_status below
base_vars <- c("YEAR", "ID", "SERV", "MAPP3", "MAPP5", "MAPP8", "BDTOT", "SYSID", "MCRNUM", "CNTRL")
aha_new <- list(
  beds        = c("MSICBD", "CICBD", "NICBD", "PEDICBD", "SPCICBD", "PSYBD", "REHABBD", "OBBD"),
  volumes     = c("SUROPIP", "SUROPOP", "VEM", "ADMTOT", "IPDTOT", "BIRTHS", "VOTH"),
  cardiac     = c("ACLABHOS", "ICLABHOS", "ADTCHOS", "PELABHOS", "ACARDHOS"),   # sum into one cardiac-services count after the merge
  service     = c("ROBOHOS", "ONCOLHOS", "TRAUMHOS", "OBHOS", "PSYHOS", "MRIHOS", "PETHOS", "RADTHHOS"),
  physicians  = c("FTMDTF", "FTRES", "PTRES", "IPAHOS", "ISMHOS", "EQMODHOS", "FOUNDHOS")   # IPAHOS / ISMHOS are missing in 2018; do not use across that year
)
want <- c(base_vars, unname(unlist(aha_new)))
missing_vars <- setdiff(want, names(AHA))
if (length(missing_vars) > 0) message("AHA variables not found (check names): ", paste(missing_vars, collapse = ", "))

AHA_hosp <- AHA %>%
  select(any_of(want)) %>%
  distinct() %>%
  mutate(independent = ifelse(is.na(SYSID), 1, 0),
         MCRNUM = pad_ccn(MCRNUM),
         cardiac_services = rowSums(across(any_of(aha_new$cardiac)), na.rm = FALSE))   # NA if any component is missing

stopifnot(!anyDuplicated(AHA_hosp[, c("ID", "YEAR")]))

AHA_hosp <- AHA_hosp %>%
  mutate(
    # hosptype1: equivalent to type1_aha
    hosptype1 = dplyr::case_when(
      SERV == 10 ~ 1L,
      SERV %in% c(11, 12, 13, 22, 33, 41, 42, 44, 45, 46, 47, 48, 49) ~ 2L,
      SERV == 18 ~ 3L,
      SERV == 50 ~ 4L,
      SERV %in% c(51, 52, 53, 55, 56, 57, 58, 59, 90, 91) ~ 5L,
      SERV %in% c(62, 80, 82) ~ 6L,
      TRUE ~ NA_integer_
    ),
    hosptype1 = factor(
      hosptype1, levels = 1:6,
      labels = c(
        "General medical/surgical",
        "Specialty",
        "Rural",
        "Children's general medical/surgical",
        "Children's specialty",
        "Other"
      )
    ),
    #hosptype2: equivalent to type2_aha, based on hosptype1
    hosptype2 = dplyr::case_when(
      hosptype1 %in% c("General medical/surgical",
                       "Children's general medical/surgical") ~ 1L,
      hosptype1 %in% c("Specialty",
                       "Children's specialty") ~ 2L,
      hosptype1 %in% c("Rural", "Other") ~ 3L,
      TRUE ~ NA_integer_
    ),
    hosptype2 = factor(
      hosptype2, levels = 1:3,
      labels = c(
        "General medical/surgical",
        "Specialty",
        "Other"
      )
    )
  )

# create teaching hospital indicators
AHA_hosp <- AHA_hosp %>%
  mutate(
    # teaching1_aha: start at 3 ("Not Teaching"), then update
    teaching1_aha = 3L,
    teaching1_aha = ifelse(MAPP8 == 1, 1L, teaching1_aha),
    teaching1_aha = ifelse(
      teaching1_aha == 3L & (MAPP3 == 1 | MAPP5 == 1),
      2L,
      teaching1_aha
    ),
    teaching1_aha = factor(
      teaching1_aha,
      levels = c(1L, 2L, 3L),
      labels = c("Major Teaching", "Minor Teaching", "Not Teaching")
    ),
    
    # teaching2_aha: 0/1 then factor with labels
    teaching2_aha = 0L,
    teaching2_aha = ifelse(
      teaching1_aha %in% c("Major Teaching", "Minor Teaching"),
      1L,
      teaching2_aha
    ),
    teaching2_aha = factor(
      teaching2_aha,
      levels = c(0L, 1L),
      labels = c("Not Teaching", "Teaching")
    )
  )

# ownership / profit status from the AHA control code
# CHECK these codes against your AHA codebook (written from memory of the standard CNTRL scheme)
AHA_hosp <- AHA_hosp %>%
  mutate(profit_status = case_when(CNTRL %in% 31:33      ~ "forprofit",   # investor-owned: individual / partnership / corporation
                                   CNTRL %in% c(21, 23)  ~ "nonprofit",   # church-operated / other not-for-profit
                                   CNTRL %in% 12:16      ~ "government",  # state / county / city / city-county / district
                                   CNTRL %in% 41:48      ~ "federal",
                                   TRUE                  ~ NA_character_))
AHA_hosp %>% count(CNTRL, profit_status) %>% print(n = 40)

AHA_hosp <- AHA_hosp %>%
  select(YEAR, ID, independent, SYSID, hosptype2, hosptype1, teaching2_aha, BDTOT, MCRNUM, CNTRL, profit_status,
         cardiac_services, any_of(unname(unlist(aha_new[c("beds", "volumes", "service", "physicians")]))))

hospital_data <- hospital_data %>%
  left_join(AHA_hosp, by=c("ID", "TaxYr"="YEAR"))

##### Append for-profit hospitals (not 990 filers, so they have no leadership data) ####
# AHA hospital-years that are for-profit and not already matched to a 990 filer are added as extra rows.
# Every leadership column is NA on these rows (including exec_team_size): use in_leadership_data, not "== 0" or "is.na".
# This must come BEFORE the HCRIS / outcome / HRRP joins so these rows pick them up through MCRNUM.
forprofit_rows <- AHA_hosp %>%
  filter(profit_status == "forprofit",
         YEAR >= min(hospital_data$TaxYr, na.rm = TRUE),
         YEAR <= max(hospital_data$TaxYr, na.rm = TRUE)) %>%
  mutate(ID = as.character(ID)) %>%
  anti_join(hospital_data %>% filter(!is.na(ID)) %>% transmute(ID = as.character(ID), YEAR = TaxYr),
            by = c("ID", "YEAR")) %>%
  rename(TaxYr = YEAR) %>%
  mutate(Filer.EIN = NA_character_)

hospital_data <- bind_rows(hospital_data %>% mutate(ID = as.character(ID)), forprofit_rows) %>%
  mutate(in_leadership_data = !is.na(Filer.EIN))

hospital_data %>% count(TaxYr, in_leadership_data, profit_status) %>% print(n = 100)

##### Read in HCRIS data and join ####
HCRIS <- read_csv(paste0(raw_data_path, "/final_HCRIS_data.csv"))
HCRIS <- HCRIS %>%
  mutate(provider_number = pad_ccn(provider_number)) %>%
  select(provider_number, year, beds, hrrp_payment, hvbp_payment, tot_discharges, mcare_discharges, mcaid_discharges, labor_costs, movableequipment_purch,
         fixedequipment_purch, build_purch, land_purch, landimpr_purch, tot_operating_exp, tot_pat_rev,
         any_of(c("net_pat_rev", "allowance", "obs_days_tot", "obs_days_mcare", "inpt_days_tot", "inpt_days_mcare")))   # added 6 Nov
if (!"net_pat_rev" %in% names(HCRIS)) stop("net_pat_rev not in final_HCRIS_data.csv: keep it in the script that combines the v1996 / v2010 files")
miss_obs <- setdiff(c("obs_days_tot", "obs_days_mcare", "inpt_days_tot", "inpt_days_mcare"), names(HCRIS))
if (length(miss_obs)) message("Not in final_HCRIS_data.csv yet (re-run the HCRIS extracts and the combine step): ", paste(miss_obs, collapse = ", "))

hospital_data <- hospital_data %>%
  left_join(HCRIS, by=c("TaxYr"="year", "MCRNUM"="provider_number"))

# 6 Nov: tot_pat_rev is GROSS patient revenue (G-3 line 1, charges), so the old margin was a charge-to-cost markup (~0.6).
# Operating (patient) margin = (net patient revenue - operating expenses) / net patient revenue (G-3 lines 3 and 4).
hospital_data <- hospital_data %>%
  mutate(oper_margin   = if_else(net_pat_rev > 0, (net_pat_rev - tot_operating_exp) / net_pat_rev, NA_real_),
         markup_ratio  = (tot_pat_rev - tot_operating_exp) / tot_pat_rev)                                   # the old definition, kept for reference
cat("\nOperating margin (net patient revenue) - should centre near 0:\n"); print(summary(hospital_data$oper_margin))

#### Read in hospital outcomes data created in other scripts
# data04_HospCompare_v2.R keys the outcomes to the 36-month MEASUREMENT WINDOW, not the release year in the file name.
#   `year` in this file = the calendar year the window ENDS (June), so year Y covers Jul (Y-3) - Jun Y.
#   Consecutive years share 2 of 3 years of discharges, so effects of a leadership change show up slowly and are smeared
#   over about 3 years. Joining on the window END is the no-look-ahead choice: the outcome in year t never contains care
#   delivered after mid-year t. To centre the window on the care it measures instead, set outcome_year_key to "year_mid".
outcome_year_key <- "year"          # "year" (window end) or "year_mid" (centre of the window)

read_mort <- read_rds(paste0(created_data_path, "/mort_read_data_windowed.rds")) %>%
  filter(!is.na(provider_id)) %>%
  mutate(provider_id = pad_ccn(provider_id),
         outcome_year = .data[[outcome_year_key]]) %>%
  select(provider_id, outcome_year,
         ami_mortality, hf_mortality, pneumonia_mortality,
         ami_readmission, hf_readmission, pneumonia_readmission,
         copd_readmission, stk_readmission, hip_knee_readmission, cabg_readmission, hosp_wide_readmission,
         copd_mortality, stk_mortality, cabg_mortality,
         mort_3, mort_hp, mort_ap, read_3, read_hp, read_ap,
         pn_cohort_expanded, any_readm_bridged, win_start, win_end,
         any_of(c("ami_readmission_n", "hf_readmission_n", "pneumonia_readmission_n")))

# with "year_mid" two windows can share a mid-year; keep the later-ending one
# the window dates are KEPT (as outcome_win_start / outcome_win_end) so the timing of every outcome stays visible in the data
read_mort <- read_mort %>%
  arrange(provider_id, outcome_year, desc(win_end)) %>%
  distinct(provider_id, outcome_year, .keep_all = TRUE) %>%
  rename(outcome_win_start = win_start, outcome_win_end = win_end)

hcahps <- read_rds(paste0(created_data_path, "/hcahps_data.rds")) %>%
  filter(!is.na(provider_id)) %>%
  mutate(provider_id = pad_ccn(provider_id))
# hcahps_data.rds (data05) is keyed on the year the survey's discharge period ENDS, like the mortality/readmission panel above,
# so both outcomes line up on the same year.

hospital_data <- hospital_data %>%
  left_join(read_mort, by=c("TaxYr"="outcome_year", "MCRNUM"="provider_id")) %>%
  left_join(hcahps, by=c("TaxYr"="year", "MCRNUM"="provider_id"))

# how much of each outcome window falls after the ACA was signed (Mar 2010) and after HRRP penalties began (Oct 2012)
# 0 = window entirely before, 1 = entirely after. Use these (not the calendar year) to decide what counts as a pre / post window.
share_after <- function(win_start, win_end, date) {
  pmax(0, pmin(1, as.numeric(win_end - as.Date(date)) / as.numeric(win_end - win_start)))
}
hospital_data <- hospital_data %>%
  mutate(outcome_share_post_aca  = share_after(outcome_win_start, outcome_win_end, "2010-03-23"),
         outcome_share_post_hrrp = share_after(outcome_win_start, outcome_win_end, "2012-10-01"))

hospital_data <- hospital_data %>%
  mutate(bed_num = ifelse(!is.na(BDTOT), BDTOT, beds),
         general = ifelse(hosptype2=="General medical/surgical",1,0),
         teaching = ifelse(teaching2_aha=="Teaching",1,0))

# Fixed-composition averages built in data04 (NA unless all listed conditions are reported):
#   mort_3 / read_3 : AMI + HF + pneumonia
#   mort_ap / read_ap: AMI + HF only -- avoids the level break when CMS widened the pneumonia cohort (windows ending Jun 2015+).
#   mort_hp / read_hp: HF + pneumonia -- AMI is only reported for ~50-60% of hospitals (low volumes are suppressed), so dropping it
#                       raises coverage to ~75-85%.
outcome_set <- "hp"                 # "3" (AMI+HF+PN), "ap" (AMI+HF) or "hp" (HF+PN)
hospital_data <- hospital_data %>%
  mutate(all_mort = case_when(outcome_set == "3" ~ mort_3, outcome_set == "ap" ~ mort_ap, outcome_set == "hp" ~ mort_hp),
         all_read = case_when(outcome_set == "3" ~ read_3, outcome_set == "ap" ~ read_ap, outcome_set == "hp" ~ read_hp))

# Hospital Compare readmission is missing for the window ending 2016 (data04 leaves it out rather than bridge it from HRRP).
# Readmission is therefore only used up to this year; set to NULL once more files are added.
readm_last_year <- NULL
if (!is.null(readm_last_year)) {
  hospital_data <- hospital_data %>%
    mutate(across(c(ami_readmission, hf_readmission, pneumonia_readmission, read_3, read_hp, read_ap, all_read),
                  ~ if_else(TaxYr > readm_last_year, NA_real_, .x)))
}

#### HRRP penalty status by condition (HRRP files read in data04 / data00) ####
# TIMING: an HRRP file window ending June Y sets the penalty for federal fiscal year Y+2 (Oct Y+1 - Sep Y+2).
#   e.g. Jul 2008 - Jun 2011 -> FY2013 (the first penalties).  TaxYr is the 990 fiscal year, labelled by its majority-days
#   year, so this match is only APPROXIMATE: a calendar-year hospital's TaxYr 2012 already contains Oct-Dec 2012 (penalised),
#   and its TaxYr 2013 mixes 9 months of FY2013 with 3 months of FY2014. hrrp_payment (HCRIS) is the better measure of the
#   year in which the payment cut actually applied; the flags below say what was determined by past performance.
# So on a row with TaxYr = t, hrrp_*_pen is the readmission performance in the window ending t-2, whereas read_* on the
# same row is the window ending t. They are NOT the same period.
hrrp_long <- read_rds(paste0(created_data_path, "/hrrp_err_long.rds"))

hrrp_cond_year <- hrrp_long %>%
  filter(condition %in% c("ami", "hf", "pneumonia")) %>%
  mutate(provider_id  = pad_ccn(provider_id),
         penalty_year = as.integer(format(win_end, "%Y")) + 2L) %>%
  filter(penalty_year >= 2013)          # HRRP penalties start in FY2013

dup_hrrp <- hrrp_cond_year %>% count(provider_id, condition, penalty_year) %>% filter(n > 1)
if (nrow(dup_hrrp) > 0) warning(nrow(dup_hrrp), " provider/condition/penalty-year rows have more than one HRRP window (latest release kept)")

hrrp_wide <- hrrp_cond_year %>%
  arrange(provider_id, condition, penalty_year, desc(release_year)) %>%
  distinct(provider_id, condition, penalty_year, .keep_all = TRUE) %>%
  transmute(provider_id, condition, penalty_year, err,
            disch = discharges,
            pen = as.integer(err > 1)) %>%          # NA when err is NA (too few cases)
  pivot_wider(names_from = condition, values_from = c(err, pen, disch),
              names_glue = "hrrp_{condition}_{.value}") %>%
  mutate(n_cond_measured  = rowSums(!is.na(across(c(hrrp_ami_pen, hrrp_hf_pen, hrrp_pneumonia_pen)))),
         n_cond_penalized = rowSums(across(c(hrrp_ami_pen, hrrp_hf_pen, hrrp_pneumonia_pen)), na.rm = TRUE),
         hrrp_penalized_any = if_else(n_cond_measured == 0, NA_integer_, as.integer(n_cond_penalized > 0)),
         hrrp_in_program = 1L)

hospital_data <- hospital_data %>%
  left_join(hrrp_wide, by = c("TaxYr" = "penalty_year", "MCRNUM" = "provider_id")) %>%
  # not in the HRRP file in a penalty year = not subject to HRRP (critical access, Maryland, ...), which is different from
  # "not penalized". Needs a CCN to say anything, so rows without MCRNUM stay NA.
  mutate(hrrp_in_program = if_else(TaxYr >= 2013 & !is.na(MCRNUM), coalesce(hrrp_in_program, 0L), NA_integer_))

# coverage check: which penalty years do your HRRP files actually cover? (a year with in_program near 0 is a missing file)
hospital_data %>% filter(TaxYr >= 2013) %>%
  group_by(TaxYr) %>%
  summarise(in_program = mean(hrrp_in_program, na.rm = TRUE), pen_any = mean(hrrp_penalized_any, na.rm = TRUE),
            pen_ami = mean(hrrp_ami_pen, na.rm = TRUE), pen_hf = mean(hrrp_hf_pen, na.rm = TRUE),
            pen_pn = mean(hrrp_pneumonia_pen, na.rm = TRUE)) %>% print()

# PRE-POLICY exposure (time-invariant): ERR for Jul 2008 - Jun 2011, the data behind the first (FY2013) penalties.
# Fixed over time, so it is the natural treatment-intensity measure for a DiD / event study. CAUTION: it is NOT free of response
# to the policy. The window ends Jun 2011 but the ACA (which created HRRP) was signed in Mar 2010, so about 40% of the window
# (Mar 2010 - Jun 2011) is care delivered after hospitals knew HRRP was coming. Check pre-trends, or use hrrp_pre_* only as a
# measure of how exposed a hospital was going in. The year-specific hrrp_*_pen above are partly the hospital's own response.
# Also: penalties from FY2015 on also cover COPD and hip/knee (added by CMS), which are not in the 3-condition flags here.
hrrp_pre <- read_rds(paste0(created_data_path, "/hrrp_pre_policy_exposure.rds")) %>%
  mutate(provider_id = pad_ccn(provider_id),
         hrrp_pre_measured = 1L) %>%
  distinct(provider_id, .keep_all = TRUE)

hospital_data <- hospital_data %>%
  left_join(hrrp_pre, by = c("MCRNUM" = "provider_id")) %>%
  mutate(hrrp_pre_measured = if_else(!is.na(MCRNUM), coalesce(hrrp_pre_measured, 0L), NA_integer_))

# ---- Standard key names for the physician-hospital project (see 00_functions.R) -----------------
# the original TaxYr / Filer.EIN / ID columns are kept, so the analysis scripts keep working
hospital_data <- hospital_data %>%
  mutate(ID   = as.character(ID),
         ein  = Filer.EIN,
         year = as.integer(TaxYr))

# one row per hospital-year, on both identifiers (for-profit rows have no EIN, so the EIN check is on filers only)
assert_unique(filter(hospital_data, !is.na(Filer.EIN)), Filer.EIN, TaxYr)
assert_unique(filter(hospital_data, !is.na(ID)), ID, TaxYr)

# ---- Key table used to link CMS CCNs to hospitals (script 4) ------------------------------------
hosp_key <- hospital_data %>%
  filter(!is.na(ID), !is.na(MCRNUM)) %>%
  distinct(year, MCRNUM, ID, ein, hosptype1, hosptype2, independent, SYSID)

# a CCN should belong to one hospital in a given year; if not, script 4 needs a rule for it
dup_ccn <- hosp_key %>% count(year, MCRNUM) %>% filter(n > 1)
if (nrow(dup_ccn) > 0) {
  warning(nrow(dup_ccn), " CCN-years map to more than one hospital in hosp_key")
  print(head(dup_ccn))
}

#### Fiscal-year timing for 990 

pad_ein  <- function(x) str_pad(str_trim(as.character(x)), 9, pad = "0")
month_end   <- function(y, m) as.Date(sprintf("%d-%02d-01", ifelse(m == 12, y + 1, y), ifelse(m == 12, 1, m + 1))) - 1
month_start <- function(y, m) as.Date(sprintf("%d-%02d-01", y, m))

fy_obs <- read_rds(paste0(created_data_path, "/xml_ein_data_new(with_ind).rds")) %>%
  transmute(ein9 = pad_ein(ein), fy_begin = as.Date(beg_dt), fy_end = as.Date(end_dt)) %>% filter(!is.na(fy_begin), !is.na(fy_end)) %>%
  mutate(end_y = as.integer(format(fy_end, "%Y")), days_all = as.numeric(fy_end - fy_begin) + 1,
         days_end = as.numeric(fy_end - pmax(fy_begin, as.Date(paste0(end_y, "-01-01")))) + 1,
         TaxYr = if_else(days_end >= days_all - days_end, end_y, end_y - 1L)) %>%            # same labelling rule as data01
  group_by(ein9, TaxYr) %>% slice_max(days_all, n = 1, with_ties = FALSE) %>% ungroup() %>%  # full-year filing, not a short stub
  select(ein9, TaxYr, fy_begin, fy_end)
fye_usual <- fy_obs %>% mutate(m = as.integer(format(fy_end, "%m"))) %>% count(ein9, m) %>%
  group_by(ein9) %>% slice_max(n, n = 1, with_ties = FALSE) %>% ungroup() %>% select(ein9, fye_month = m)

hospital_data <- hospital_data %>%
  mutate(ein9 = if_else(in_leadership_data, pad_ein(Filer.EIN), NA_character_)) %>%
  left_join(fy_obs, by = c("ein9", "TaxYr")) %>%
  left_join(fye_usual, by = "ein9") %>%
  mutate(fye_month = coalesce(as.integer(format(fy_end, "%m")), fye_month),
         fy_dates_imputed = as.integer(is.na(fy_begin) & !is.na(fye_month)),
         # a fiscal year ending in month m of year E has its majority in E if m >= 7, otherwise in E-1; invert that to get E from TaxYr
         .E = TaxYr + if_else(fye_month >= 7, 0L, 1L),
         fy_end   = if_else(is.na(fy_end)   & !is.na(fye_month), month_end(.E, fye_month), fy_end),
         fy_begin = if_else(is.na(fy_begin) & !is.na(fye_month), if_else(fye_month == 12, month_start(.E, 1L), month_start(.E - 1L, fye_month + 1L)), fy_begin),
         fy_share_post_aca     = share_after(fy_begin, fy_end, "2010-03-23"),
         fy_share_post_rule    = share_after(fy_begin, fy_end, "2011-08-01"),
         fy_share_post_penalty = share_after(fy_begin, fy_end, "2012-10-01"),
         fy_post_rule     = as.integer(fy_begin >= as.Date("2011-08-01")),      # MAIN: first fiscal year that BEGINS after the final rule
         fy_post_penalty  = as.integer(fy_begin >= as.Date("2012-10-01")),      # robustness: first fiscal year beginning after penalties start
         fy_straddle_rule = as.integer(fy_begin < as.Date("2011-08-01") & fy_end >= as.Date("2011-08-01"))) %>%   # the partly treated year
  select(-.E, -ein9)

# checks: fiscal dates coverage, year-end mix, and how the main definition compares with the TaxYr >= 2012 rule used so far
hospital_data %>% filter(in_leadership_data) %>% summarise(hosp_years = n(), has_dates = mean(!is.na(fy_begin)), imputed = mean(fy_dates_imputed %in% 1)) %>% print()
hospital_data %>% filter(in_leadership_data) %>% count(fye_month) %>% mutate(share = round(n / sum(n), 3)) %>% print()
hospital_data %>% filter(in_leadership_data, !is.na(fy_post_rule)) %>% count(taxyr_post = as.integer(TaxYr >= 2012), fy_post_rule) %>% print()
hospital_data %>% filter(in_leadership_data, fy_straddle_rule == 1) %>% group_by(fye_month) %>%
  summarise(n = n(), months_after_rule = round(12 * mean(fy_share_post_rule), 1)) %>% print()

#### HIMSS leadership (paste into data02 AFTER the fiscal-timing block, before saveRDS(hospital_data, ...)) ####
# Built by himss_build.R (run it with RUN_DIAGNOSTICS = TRUE so ceo_same_990 is saved). HIMSS year = TaxYr; general hospitals; 2008-2016.
# All HIMSS variables are prefixed h_.  Rule (DECISIONS.md): analyses that POOL nonprofits and for-profits use the h_ variables for everyone;
# nonprofit-only analyses use the 990 variables (ceo_md, ceo_entry, n_md_exec, ...) with the h_ versions as robustness R6.
#   h_ceo_md           physician CEO (HIMSS MD/DO credential, or linked to a 990-verified doctor)
#   h_ceo_change       CEO differs from last year's (spelling-only id changes excluded); NA if either year is unobserved
#   h_has_md_exec      physician among CEO/CFO/COO/CIO/CNO (weak proxy for has_md: catches ~1 in 4 of the 990's)
#   h_has_md_exec_cmo  + anyone titled CMO / VP medical affairs (catches ~half of the 990's has_md)
#   h_cmo_title        someone titled CMO / VP medical affairs
#   h_ceo_same_990     HIMSS and the 990 name the same CEO this year (validation subsample)
#   *_r5               robustness R5: for June fiscal year-end hospitals take the HIMSS value of TaxYr + 1
# Not run when written (no R in the sandbox).

himss_raw <- read_rds(paste0(created_data_path, "/himss_hospital_year.rds"))
if (!"ceo_same_990" %in% names(himss_raw)) { warning("ceo_same_990 missing: rerun himss_build.R with RUN_DIAGNOSTICS = TRUE"); himss_raw$ceo_same_990 <- NA }
if (!"ceo_clinical_himss" %in% names(himss_raw)) { warning("ceo_clinical_himss missing: rerun himss_build.R"); himss_raw$ceo_clinical_himss <- NA }
himss_vars <- himss_raw %>%
  transmute(ccn, himss_year,
            h_ceo_pid = ceo_pid, h_ceo_md = as.integer(ceo_md_himss), h_ceo_clinical = as.integer(ceo_clinical_himss), h_ceo_change = ceo_change_himss,
            h_has_md_exec = as.integer(has_md_exec), h_has_md_exec_cmo = as.integer(has_md_exec_cmo), h_cmo_title = as.integer(cmo_title),
            h_n_exec_slots = n_exec_slots, h_ceo_same_990 = as.integer(ceo_same_990)) %>%
  distinct(ccn, himss_year, .keep_all = TRUE)
himss_next <- himss_vars %>% transmute(ccn, himss_year = himss_year - 1, h_ceo_change_next = h_ceo_change, h_ceo_md_next = h_ceo_md)   # TaxYr + 1 values

hospital_data <- hospital_data %>%
  mutate(.ccn = pad_ccn(MCRNUM)) %>%
  left_join(himss_vars, by = c(".ccn" = "ccn", "TaxYr" = "himss_year")) %>%
  left_join(himss_next, by = c(".ccn" = "ccn", "TaxYr" = "himss_year")) %>%
  mutate(in_himss        = !is.na(h_n_exec_slots),
         h_ceo_change_r5 = if_else(fye_month %in% 6, h_ceo_change_next, h_ceo_change),
         h_ceo_md_r5     = if_else(fye_month %in% 6, h_ceo_md_next, h_ceo_md)) %>%
  select(-.ccn, -h_ceo_change_next, -h_ceo_md_next)

# checks
hospital_data %>% filter(general %in% 1, TaxYr %in% 2008:2016) %>%
  group_by(source = if_else(in_leadership_data, "990 filer", "for-profit")) %>%
  summarise(hosp_years = n(), in_himss = round(mean(in_himss), 3), ceo_md = round(mean(h_ceo_md, na.rm = TRUE), 3),
            ceo_change = round(mean(h_ceo_change, na.rm = TRUE), 3), same_ceo_as_990 = round(mean(h_ceo_same_990, na.rm = TRUE), 3)) %>% print()
# clinical CEO: HIMSS (physician or nursing credential) vs 990 ceo_clinical, same hospital-years (are nurse CEOs credentialed in HIMSS?)
if ("ceo_clinical" %in% names(hospital_data)) hospital_data %>% filter(general %in% 1, in_leadership_data, !is.na(h_ceo_clinical), !is.na(ceo_clinical)) %>%
  summarise(n = n(), clinical_990 = round(mean(ceo_clinical), 3), clinical_himss = round(mean(h_ceo_clinical), 3),
            agree = round(mean(ceo_clinical == h_ceo_clinical), 3), himss_given_990 = round(mean(h_ceo_clinical[ceo_clinical == 1]), 3)) %>% print()

#### combined CEO measure (himss_build.R PART 7; paste right after the HIMSS block above) ####
#   c_ceo_md        physician CEO: HIMSS says WHO the CEO is; physician status from the validated 990 when that person is on a 990, else HIMSS credential
#   c_ceo_clinical  same, physician or nurse (nurse from EITHER source: HIMSS nursing credential or 990 nurse flag; 990 nurse status is not verified)
#   c_ceo_specialty physician specialty from your 990 validation (person the HIMSS CEO links to; NA if not on a 990); c_ceo_specialty_src where it
#                   came from (npi_data / review_note); c_ceo_npi likewise
#   c_ceo_md_nofb / c_ceo_clinical_nofb  same without the 990 fallback (pre-specified version)
#   c_ceo_source    0 hand-check (coded in PART 7) | 1 990: same CEO | 2 990: other position | 3 HIMSS credential | 4 990 fallback (no HIMSS CEO that year)
f_comb <- paste0(created_data_path, "/himss_ceo_combined.rds")
if (file.exists(f_comb)) {
  ceo_comb <- read_rds(f_comb) %>% distinct(ccn, himss_year, .keep_all = TRUE) %>%
    transmute(ccn, TaxYr = himss_year, c_ceo_md, c_ceo_clinical, c_ceo_specialty, c_ceo_specialty_src, c_ceo_npi, c_ceo_source = c_source)
  hospital_data <- hospital_data %>% select(-any_of(c("c_ceo_md", "c_ceo_clinical", "c_ceo_specialty", "c_ceo_specialty_src", "c_ceo_npi", "c_ceo_source"))) %>%
    mutate(.ccn = pad_ccn(MCRNUM)) %>% left_join(ceo_comb, by = c(".ccn" = "ccn", "TaxYr")) %>% select(-.ccn)
  # 990 fallback (3 Nov, decided after seeing the first combined results): when HIMSS has no CEO for a 990 filer in that year, keep the
  # 990 measure so the nonprofit sample does not shrink. The pre-specified version without the fallback is kept as *_nofb (robustness;
  # also used for annual leadership changes in analysis05, so a switch between sources is never counted as a change in the CEO).
  hospital_data <- hospital_data %>% select(-any_of(c("c_ceo_md_nofb", "c_ceo_clinical_nofb"))) %>%
    mutate(c_ceo_md_nofb = c_ceo_md, c_ceo_clinical_nofb = c_ceo_clinical,
           .fb = is.na(c_ceo_md) & in_leadership_data %in% TRUE & !is.na(ceo_md),
           c_ceo_md       = if_else(.fb, as.integer(ceo_md), c_ceo_md),
           c_ceo_clinical = if_else(.fb, as.integer(coalesce(ceo_clinical, ceo_md)), c_ceo_clinical),
           c_ceo_source   = if_else(.fb, "4 990 fallback (no HIMSS CEO)", c_ceo_source)) %>% select(-.fb)
  hospital_data %>% filter(general %in% 1, TaxYr == 2010) %>% group_by(source = if_else(in_leadership_data, "990 filer", "for-profit")) %>%
    summarise(n = n(), has_combined = round(mean(!is.na(c_ceo_md)), 3), c_ceo_md = round(mean(c_ceo_md, na.rm = TRUE), 3),
              c_ceo_clinical = round(mean(c_ceo_clinical, na.rm = TRUE), 3)) %>% print()
  hospital_data %>% filter(general %in% 1, TaxYr == 2010, !is.na(c_ceo_source)) %>% count(c_ceo_source) %>% print()   # hand-check rows should show 6
} else warning("himss_ceo_combined.rds missing: run himss_build.R PART 7")

#### system CEO (himss_build.R PART 8; paste right after the combined CEO block) ####
#   s_member        hospital has a HIMSS parent entity (system member) this year
#   s_ceo_md        the system CEO (CEO of the highest HIMSS parent that lists one) is a physician: 990 doctor flag if on a system hospital's 990, else HIMSS credential
#   s_ceo_clinical  same, physician or nurse (either source)
#   s_type          HIMSS parent type: "IDS/RHA" (multi-hospital system) or "Single Hospital Health System"
#   s_same_hosp     the system CEO is the hospital's own CEO;  s_uid / s_name  HIMSS system id and name;  s_src  where the status comes from
f_sys <- paste0(created_data_path, "/himss_system_ceo.rds")
if (file.exists(f_sys)) {
  sys_ceo <- read_rds(f_sys) %>% distinct(ccn, himss_year, .keep_all = TRUE) %>%
    transmute(ccn, TaxYr = himss_year, s_member = as.integer(s_member), s_uid, s_name, s_type, s_ceo_md, s_ceo_clinical, s_same_hosp, s_src)
  hospital_data <- hospital_data %>% select(-any_of(c("s_member", "s_uid", "s_name", "s_type", "s_ceo_md", "s_ceo_clinical", "s_same_hosp", "s_src"))) %>%
    mutate(.ccn = pad_ccn(MCRNUM)) %>% left_join(sys_ceo, by = c(".ccn" = "ccn", "TaxYr")) %>% select(-.ccn)
  hospital_data %>% filter(general %in% 1, TaxYr == 2010) %>% group_by(source = if_else(in_leadership_data, "990 filer", "for-profit")) %>%
    summarise(n = n(), in_himss_chain = round(mean(!is.na(s_member)), 3), member = round(mean(s_member, na.rm = TRUE), 3),
              sys_ceo_found = round(mean(!is.na(s_ceo_md[s_member %in% 1])), 3), sys_ceo_md = round(mean(s_ceo_md, na.rm = TRUE), 3)) %>% print()
} else warning("himss_system_ceo.rds missing: run himss_build.R PART 8")

#### why HIMSS and the 990 name different CEOs (himss_build.R PART 8; for the bridge in analysis02) ####
#   d_class   0 same CEO | 1 name_variant | 2 himss_lags | 3 himss_leads | 4 other_role | 5 in_990_other | 6 system_level | 7 unexplained (NA = no usable 990 CEO name)
#   d_is_sys  the 990's assigned CEO is the HIMSS system CEO
f_dcl <- paste0(created_data_path, "/himss_ceo_disagreement.rds")
if (file.exists(f_dcl)) {
  dcl <- read_rds(f_dcl) %>% distinct(ccn, himss_year, .keep_all = TRUE) %>% transmute(ccn, TaxYr = himss_year, d_class, d_is_sys)
  hospital_data <- hospital_data %>% select(-any_of(c("d_class", "d_is_sys"))) %>%
    mutate(.ccn = pad_ccn(MCRNUM)) %>% left_join(dcl, by = c(".ccn" = "ccn", "TaxYr")) %>% select(-.ccn)
  hospital_data %>% filter(general %in% 1, in_leadership_data, TaxYr == 2010) %>% count(d_class, d_is_sys) %>% print()
} else warning("himss_ceo_disagreement.rds missing: run himss_build.R (PART 5 and PART 8 in one session)")

#### System membership changes

sysd <- hospital_data %>% filter(!is.na(ID)) %>% mutate(ID = as.character(ID)) %>%
  distinct(ID, TaxYr, SYSID) %>% group_by(ID, TaxYr) %>% slice(1) %>% ungroup() %>%
  mutate(sys = coalesce(as.character(SYSID), "none")) %>%
  arrange(ID, TaxYr) %>% group_by(ID) %>%
  mutate(prev_ok = !is.na(lag(TaxYr)) & lag(TaxYr) == TaxYr - 1, next_ok = !is.na(lead(TaxYr)) & lead(TaxYr) == TaxYr + 1,
         sys_prev = if_else(prev_ok, lag(sys), NA_character_), sys_next = if_else(next_ok, lead(sys), NA_character_),
         raw   = !is.na(sys_prev) & sys != sys_prev,
         blip  = raw & !is.na(sys_next) & sys_next == sys_prev,                 # A -> B in t, back to A in t+1
         back  = raw & coalesce(lag(blip), FALSE),                              # the return leg of a blip
         sys_change = as.integer(raw & !blip & !back),
         sys_change_type = case_when(sys_change == 0 ~ NA_character_, sys_prev == "none" ~ "join", sys == "none" ~ "leave", TRUE ~ "switch"),
         post_sys_change = as.integer(cumsum(sys_change) > 0),
         ever_sys_change_0916 = as.integer(any(sys_change == 1 & TaxYr %in% 2009:2016)),
         sys_member0 = as.integer(any(sys != "none" & TaxYr %in% 2009:2011))) %>%
  ungroup()

cat("System-status changes (after removing one-year blips):", sum(sysd$sys_change), "| blips removed:", sum(sysd$blip), "\n")
print(sysd %>% filter(sys_change == 1) %>% count(TaxYr, sys_change_type) %>% tidyr::pivot_wider(names_from = sys_change_type, values_from = n, values_fill = 0))

hospital_data <- hospital_data %>% mutate(.ID = as.character(ID)) %>%
  left_join(sysd %>% select(ID, TaxYr, sys_change, sys_change_type, post_sys_change, ever_sys_change_0916, sys_member0), by = c(".ID" = "ID", "TaxYr")) %>%
  select(-.ID)
hospital_data %>% filter(general %in% 1, TaxYr == 2012) %>% group_by(in_leadership_data) %>%
  summarise(hospitals = n(), share_ever_change_0916 = round(mean(ever_sys_change_0916, na.rm = TRUE), 3), share_system0 = round(mean(sys_member0, na.rm = TRUE), 3)) %>% print()

#### ACA Medicaid expansion by state (paste into data02 AFTER the share_after() block, before saveRDS(hospital_data, ...)) ####
# Adds:
#   state           hospital's state: AHA MSTATE if it is in hospital_data, else the first two digits of the CCN (CMS state code),
#                   filled within AHA ID so rows without a CCN pick it up
#   mcaid_exp_date  effective date of the state's ACA Medicaid expansion (KFF); NA = not expanded by the end of 2017
#   mcaid_exp       for fiscal / calendar-year outcomes (HCRIS, AHA, 990, HCAHPS): 1 from the calendar year of the effective date if it fell
#                   Jan-Jun, otherwise from the next year
#   mcaid_exp_win   for Hospital Compare window outcomes: share of the 36-month window (outcome_win_start - outcome_win_end) after the
#                   effective date, 0-1 (same logic as outcome_share_post_aca); falls back to mcaid_exp when the window dates are missing
# Early partial expansions (CA, CT, DC, MN, NJ, WA in 2010-11) are not coded; the state x year FE robustness absorbs them.
# Not run when written (no R in the sandbox).

ccn_state <- c("01"="AL","02"="AK","03"="AZ","04"="AR","05"="CA","06"="CO","07"="CT","08"="DE","09"="DC","10"="FL","11"="GA","12"="HI",
               "13"="ID","14"="IL","15"="IN","16"="IA","17"="KS","18"="KY","19"="LA","20"="ME","21"="MD","22"="MA","23"="MI","24"="MN",
               "25"="MS","26"="MO","27"="MT","28"="NE","29"="NV","30"="NH","31"="NJ","32"="NM","33"="NY","34"="NC","35"="ND","36"="OH",
               "37"="OK","38"="OR","39"="PA","40"="PR","41"="RI","42"="SC","43"="SD","44"="TN","45"="TX","46"="UT","47"="VT","48"="VI",
               "49"="VA","50"="WA","51"="WV","52"="WI","53"="WY","55"="CA","67"="TX")
mcaid_dates <- tribble(~state, ~mcaid_exp_date,
                       "AZ","2014-01-01", "AR","2014-01-01", "CA","2014-01-01", "CO","2014-01-01", "CT","2014-01-01", "DE","2014-01-01", "DC","2014-01-01",
                       "HI","2014-01-01", "IL","2014-01-01", "IA","2014-01-01", "KY","2014-01-01", "MD","2014-01-01", "MA","2014-01-01", "MN","2014-01-01",
                       "NV","2014-01-01", "NJ","2014-01-01", "NM","2014-01-01", "NY","2014-01-01", "ND","2014-01-01", "OH","2014-01-01", "OR","2014-01-01",
                       "RI","2014-01-01", "VT","2014-01-01", "WA","2014-01-01", "WV","2014-01-01", "MI","2014-04-01", "NH","2014-08-15", "PA","2015-01-01",
                       "IN","2015-02-01", "AK","2015-09-01", "MT","2016-01-01", "LA","2016-07-01") %>%
  mutate(mcaid_exp_date = as.Date(mcaid_exp_date),
         mcaid_exp_year = as.integer(format(mcaid_exp_date, "%Y")) + as.integer(as.integer(format(mcaid_exp_date, "%m")) > 6))

hospital_data <- hospital_data %>%
  mutate(ccn_chr   = if (is.numeric(MCRNUM)) sprintf("%06d", as.integer(MCRNUM)) else str_pad(str_trim(as.character(MCRNUM)), 6, "left", "0"),
         state_ccn = unname(ccn_state[str_sub(ccn_chr, 1, 2)]),
         state_aha = if ("MSTATE" %in% names(.)) str_to_upper(str_trim(as.character(MSTATE))) else NA_character_,
         state     = coalesce(if_else(state_aha %in% c(state.abb, "DC"), state_aha, NA_character_), state_ccn)) %>%
  group_by(ID) %>% mutate(state = if (is.na(ID[1])) state else coalesce(state, na.omit(state)[1])) %>% ungroup() %>%   # rows without an AHA ID are not filled
  select(-any_of(c("mcaid_exp_date", "mcaid_exp_year", "mcaid_exp", "mcaid_exp_win"))) %>%
  left_join(mcaid_dates, by = "state") %>%
  mutate(mcaid_exp     = if_else(is.na(state), NA_integer_, as.integer(!is.na(mcaid_exp_year) & TaxYr >= mcaid_exp_year)),
         mcaid_exp_win = case_when(is.na(state) ~ NA_real_, is.na(mcaid_exp_date) ~ 0,
                                   !is.na(outcome_win_start) & !is.na(outcome_win_end) ~ share_after(outcome_win_start, outcome_win_end, mcaid_exp_date),
                                   TRUE ~ as.numeric(mcaid_exp))) %>%
  select(-ccn_chr, -state_ccn, -state_aha, -mcaid_exp_year)

cat("Rows with a state:", round(mean(!is.na(hospital_data$state)), 3), "\n")
print(hospital_data %>% filter(!is.na(state)) %>% group_by(TaxYr) %>%
        summarise(share_expanded = mean(mcaid_exp), mean_window_share = mean(mcaid_exp_win, na.rm = TRUE), .groups = "drop"), n = 20)

#### CMS IPPS impact files: case mix index and HRRP / VBP payment factors (paste into data02 before saveRDS(hospital_data, ...)) ####
# Source: raw_data/HospCaseMix/impactYYYY.csv (NBER copies of the CMS final-rule impact files; FY = fyear).
# Timing (from my knowledge of the IPPS rules - the check below tests it against HCRIS Medicare discharges):
#   - the case mix variables in the FY Y file are computed from MedPAR claims for FY Y-2 (e.g. impact2014 = discharges Oct 2011 - Sep 2012)
#     -> assigned to year = fyear - CMI_LAG, so the post-rule years start with FY2012 discharges (from Oct 2011, after the Aug 2011 rule);
#   - the payment factors in the FY Y file apply to FY Y discharges -> assigned to year = fyear. They are the values published with the
#     final rule; CMS sometimes revised the HRRP factors after publication (FY2013 especially).
# Adds (by CCN x year; NA when the hospital is not in that impact file - e.g. critical access hospitals, Maryland):
#   cmi              transfer-adjusted case mix index under the file's own (current) MS-DRG grouper (tacmiv)
#   cmi_cases        transfer-adjusted Medicare cases behind it (caseta)
#   cmi_chain        same-grouper chained CMI: year-to-year growth measured under ONE grouper version (the FY Y file reports the FY Y-2 claims
#                    under both the new and the previous grouper), so grouper revisions do not move it; anchored at the observed CMI in the first year
#                    and re-anchored at the observed CMI after a gap or a grouper mismatch
#   imp_readaf       HRRP payment adjustment factor (readaf; readmaj in the FY2014 file); 1 = no penalty; set to 1 for FY < 2013 (no programme)
#   imp_vbpaf        Hospital VBP adjustment factor (hospvbpadj FY2013-14, vbpaf FY2015+); set to 1 for FY < 2013
# Not run when written (no R in the sandbox).

CMI_LAG <- 2
imp_dir   <- file.path(raw_data_path, "HospCaseMix")
imp_files <- list.files(imp_dir, pattern = "^impact\\d{4}\\.csv$", full.names = TRUE)
num <- function(x) suppressWarnings(as.numeric(str_remove_all(x, "[,$ ]")))
pick <- function(d, cands) { v <- intersect(cands, names(d)); if (length(v)) num(d[[v[1]]]) else rep(NA_real_, nrow(d)) }

imp <- map_dfr(imp_files, function(f) {
  d  <- read_csv(f, col_types = cols(.default = col_character()), show_col_types = FALSE)
  fy <- as.integer(str_extract(basename(f), "\\d{4}"))
  v  <- sort(as.integer(str_match(names(d), "^tacmiv(\\d+)$")[, 2]), na.last = NA)     # grouper versions in this file: old, current
  tibble(ccn = str_pad(str_trim(d$provider), 6, "left", "0"), fyear = fy,
         cmi = pick(d, "tacmiv"), cmi_cases = pick(d, "caseta"), bills = pick(d, "bills"),
         cmi_oldgrp = if (length(v) >= 2) num(d[[paste0("tacmiv", min(v))]]) else NA_real_,
         grp_cur = if (length(v)) max(v) else NA_integer_, grp_old = if (length(v) >= 2) min(v) else NA_integer_,
         imp_readaf = pick(d, c("readaf", "readmaj")), imp_vbpaf = pick(d, c("vbpaf", "hospvbpadj")))
}) %>% filter(!is.na(ccn), ccn != "000000") %>% distinct(ccn, fyear, .keep_all = TRUE)

cat("Impact files read:", paste(range(imp$fyear), collapse = "-"), "| rows:", nrow(imp), "\n")
print(imp %>% group_by(fyear) %>% summarise(hospitals = n(), grp_old = grp_old[1], grp_cur = grp_cur[1],
                                            cmi_median = median(cmi, na.rm = TRUE), readaf_nonmiss = mean(!is.na(imp_readaf)),
                                            readaf_below1 = mean(imp_readaf < 1, na.rm = TRUE), vbpaf_nonmiss = mean(!is.na(imp_vbpaf)), .groups = "drop"), n = 30)

# ---- timing check: which HCRIS year do the impact-file Medicare bills line up with? (year-to-year changes, within hospital) ----
hcris_mc <- hospital_data %>% filter(!is.na(MCRNUM), mcare_discharges > 0) %>%
  transmute(ccn = str_pad(str_trim(as.character(MCRNUM)), 6, "left", "0"), year = TaxYr, lmc = log(mcare_discharges)) %>%
  distinct(ccn, year, .keep_all = TRUE) %>% arrange(ccn, year) %>% group_by(ccn) %>%
  mutate(dlmc = if_else(lag(year) == year - 1, lmc - lag(lmc), NA_real_)) %>% ungroup()
imp_d <- imp %>% filter(bills > 0) %>% arrange(ccn, fyear) %>% group_by(ccn) %>%
  mutate(dlb = if_else(lag(fyear) == fyear - 1, log(bills) - lag(log(bills)), NA_real_)) %>% ungroup()
lag_check <- map_dfr(0:3, function(L) imp_d %>% mutate(year = fyear - L) %>% inner_join(hcris_mc, by = c("ccn", "year")) %>%
                       summarise(lag = L, n = sum(!is.na(dlb) & !is.na(dlmc)), cor_changes = cor(dlb, dlmc, use = "complete.obs")))
cat("\nCorrelation of year-to-year changes: impact-file Medicare bills (FY Y file) vs HCRIS Medicare discharges in year Y - lag\n"); print(lag_check)
best_lag <- lag_check$lag[which.max(lag_check$cor_changes)]
if (best_lag != CMI_LAG) warning("Impact-file case data line up best with lag ", best_lag, ", not CMI_LAG = ", CMI_LAG, ". Check before using cmi.")

# ---- case mix by discharge year, with the same-grouper chain ---------------------------------------------------
cmi_y <- imp %>% mutate(year = fyear - CMI_LAG) %>% arrange(ccn, year) %>% group_by(ccn) %>%
  mutate(consec = !is.na(lag(year)) & lag(year) == year - 1,
         # growth from year-1 to year under the grouper that was current in the previous file (= this file's old grouper)
         g = if_else(consec & grp_old == lag(grp_cur), log(cmi_oldgrp) - log(lag(cmi)), NA_real_),
         spell = cumsum(!consec | is.na(g))) %>%
  group_by(ccn, spell) %>% mutate(cmi_chain = exp(log(cmi[1]) + cumsum(if_else(row_number() == 1, 0, g)))) %>% ungroup() %>%
  select(ccn, year, cmi, cmi_cases, cmi_chain)
fac_y <- imp %>% transmute(ccn, year = fyear, imp_readaf = if_else(fyear < 2013, 1, imp_readaf), imp_vbpaf = if_else(fyear < 2013, 1, imp_vbpaf))

hospital_data <- hospital_data %>% select(-any_of(c("cmi", "cmi_cases", "cmi_chain", "imp_readaf", "imp_vbpaf"))) %>%
  mutate(.ccn = str_pad(str_trim(as.character(MCRNUM)), 6, "left", "0")) %>%
  left_join(cmi_y, by = c(".ccn" = "ccn", "TaxYr" = "year")) %>%
  left_join(fac_y, by = c(".ccn" = "ccn", "TaxYr" = "year")) %>% select(-.ccn)

cat("\nCoverage among general hospitals with a CCN:\n")
print(hospital_data %>% filter(general %in% 1, !is.na(MCRNUM), TaxYr %in% 2009:2017) %>% group_by(TaxYr) %>%
        summarise(rows = n(), has_cmi = round(mean(!is.na(cmi)), 3), has_chain = round(mean(!is.na(cmi_chain)), 3),
                  has_readaf = round(mean(!is.na(imp_readaf)), 3), cmi_mean = round(mean(cmi, na.rm = TRUE), 3),
                  chain_mean = round(mean(cmi_chain, na.rm = TRUE), 3), .groups = "drop"), n = 20)

#### Hospital Compare process-of-care measures (data06_process_of_care.R) ####
# hf1 (HF discharge instructions), hf2, hf3, hf1_minus_hf2, hf1_minus_hf3, hf1_noceil; 12-month windows keyed to the window END year
# (poc_win_start / poc_win_end kept, as for the readmission windows). Releases 2008-2014 only (measures retired after).
f_poc <- paste0(created_data_path, "/process_of_care.rds")
if (file.exists(f_poc)) {
  poc <- read_rds(f_poc) %>% mutate(provider_id = pad_ccn(provider_id))
  hospital_data <- hospital_data %>% select(-any_of(setdiff(names(poc), c("provider_id", "year")))) %>%
    left_join(poc, by = c("MCRNUM" = "provider_id", "TaxYr" = "year"))
  hospital_data %>% filter(general %in% 1, TaxYr %in% 2008:2014) %>% group_by(TaxYr) %>%
    summarise(rows = n(), has_hf1 = round(mean(!is.na(hf1)), 3), hf1 = round(mean(hf1, na.rm = TRUE), 1), .groups = "drop") %>% print()
} else warning("process_of_care.rds missing: run data06_process_of_care.R")

#### Market concentration (item J; pre-specified 6 Nov before results; DECISIONS.md) ####
# Baseline (2009-11) HHI of admissions within the hospital's Dartmouth HRR and HSA, from ALL AHA general medical & surgical hospitals
# (SERV 10, ADMTOT > 0), not only the analysis sample. Firm = AHA system (SYSID), else the hospital: same-system hospitals are one firm.
# HHI averaged over 2009-11 (0-1 scale); n_firms = number of firms in the market. The hospital's market = its most frequent HRR / HSA in
# its 2009-11 AHA records. Column names for HRR / HSA differ by AHA release: picked by pattern below and printed - check them.
pick_geo <- function(nm, pre) { h <- grep(paste0("^", pre), nm, value = TRUE, ignore.case = TRUE); h <- h[!grepl("NAME", h, ignore.case = TRUE)]
if (!length(h)) stop("No ", pre, " column in the AHA file: set it by hand"); h[1] }
hrr_var <- pick_geo(names(AHA), "HRR"); hsa_var <- pick_geo(names(AHA), "HSA")
message("Market columns used: HRR = ", hrr_var, ", HSA = ", hsa_var)
aha_mkt <- AHA %>% filter(YEAR %in% 2009:2011) %>%
  transmute(ID, YEAR, SERV, ADMTOT, firm = if_else(is.na(SYSID), paste0("H", ID), paste0("S", SYSID)),
            hrr = na_if(as.character(.data[[hrr_var]]), ""), hsa = na_if(as.character(.data[[hsa_var]]), ""))
mode_chr <- function(x) { x <- x[!is.na(x)]; if (!length(x)) NA_character_ else names(sort(table(x), decreasing = TRUE))[1] }
hhi_of <- function(geo) aha_mkt %>% filter(SERV == 10, ADMTOT > 0, !is.na(.data[[geo]])) %>%
  group_by(YEAR, mk = .data[[geo]], firm) %>% summarise(a = sum(ADMTOT), .groups = "drop_last") %>%
  summarise(hhi = sum((a / sum(a))^2), n_firms = n(), .groups = "drop") %>%
  group_by(mk) %>% summarise(hhi = mean(hhi), n_firms = mean(n_firms), .groups = "drop")
hosp_mkt <- aha_mkt %>% group_by(ID) %>% summarise(mkt_hrr = mode_chr(hrr), mkt_hsa = mode_chr(hsa), .groups = "drop") %>%
  left_join(hhi_of("hrr") %>% rename(mkt_hrr = mk, hhi_hrr0 = hhi, nfirms_hrr0 = n_firms), by = "mkt_hrr") %>%
  left_join(hhi_of("hsa") %>% rename(mkt_hsa = mk, hhi_hsa0 = hhi, nfirms_hsa0 = n_firms), by = "mkt_hsa")
cat("\nMarket concentration (hospital level, all AHA hospitals with a market):\n")
print(summary(hosp_mkt %>% select(hhi_hrr0, hhi_hsa0, nfirms_hrr0, nfirms_hsa0)))
cat("Share of hospitals alone in their HSA market (HHI = 1):", round(mean(hosp_mkt$hhi_hsa0 >= 0.9999, na.rm = TRUE), 3), "\n")
hospital_data <- hospital_data %>% select(-any_of(setdiff(names(hosp_mkt), "ID"))) %>% left_join(hosp_mkt, by = "ID")

saveRDS(hospital_data, paste0(created_data_path, "/hospital_data.rds"))
saveRDS(hosp_key,      paste0(created_data_path, "/hosp_key.rds"))