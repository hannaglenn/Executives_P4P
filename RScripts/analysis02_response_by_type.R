library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(purrr)
library(tibble)
library(ggplot2)
library(fixest)
library(knitr)
library(WeightIt)

source("./RScripts/paths.R")

### Do hospitals with different (pre-policy) leadership types change readmissions - and mortality - differently after HRRP?   DECISIONS.md (2 Nov).
###   Two main outcomes, each with the full robustness set: all_read (main from 3 Nov; hf_readmission until then) and all_mort (added 2 Nov after the mechanisms
###   results; condition-specific mortality stays in analysis03).
###   y_it = b (type_i x post_t) + post_sys_change + hospital FE + year FE, entropy-balancing weights (type 0 reweighted to look like type 1),
###   SEs clustered by hospital.  b = how the type-1 hospitals' readmission trend diverges from type-0 hospitals' after the policy.
###   ACA Medicaid expansion (added 2 Nov): every specification also controls for mcaid_exp_win (share of the 36-month window after the
###   hospital's state expanded Medicaid; data02_aca_block.R). A0 drops that control; A1 replaces year FE with state x year FE.
###   Hospitals without a state are dropped everywhere.
###   Panel A: nonprofits, 990 types (has_md, ceo_md).   Panel B: all general hospitals incl. for-profits, HIMSS type (h_ceo_md).
### Not run when written (no R in the sandbox).

# ---- settings ---------------------------------------------------------------------------------------------------
RULE_DATE  <- as.Date("2011-08-01")
YEARS      <- 2009:2017            # years the 36-month readmission windows end (June). 2008 window removed from all estimations (6 Nov).
REF_YEAR   <- 2011                 # last window entirely before the rule
TYPE_YEARS <- c(2010, 2009)        # type measured in 2010, else 2009 (3 Nov; was 2009 then 2010)
# Balancing covariates (main weights; same as analysis03). Restored 11 Nov: these two settings were used below but no longer defined in
# the script, so it only ran when a previous session had left them in memory.
COVS        <- c("lbeds0", "teaching0", "indep0", "margin_r", "mcare_r", "cmi0")   # baseline 2009-2011
EXTRA_MATCH <- character(0)   # extra balancing covariates for the main weights: none (pre-period outcome levels are the separate W2 check)
# physician executive split by role (5 Nov, item I; mutually exclusive, Panel A). Groups x / y are reported but not estimated.
RS_GRP <- c("0 no physician executive", "1 physician CEO", "2 physician exec (not CEO), CMO in place", "3 physician exec (not CEO), no CMO",
            "x physician CEO, no 990 physician executive", "y physician executive, CEO or CMO status unknown")
MAIN_OUTCOMES <- tribble(~y, ~to, ~ylab,
                         "all_read", 2014, "Readmission, all conditions (all_read, pp)",       # main from 3 Nov (was hf_readmission)
                         "all_mort", 2014, "Mortality, all conditions (all_mort, pp)")
# all_read / all_mort follow outcome_set in data02 ("hp" = HF + pneumonia; "3" adds AMI but AMI is suppressed for ~40-50% of hospitals;
# "ap" = AMI + HF). With pneumonia included they stop at windows ending 2014 (CMS widened the pneumonia cohort from windows ending 2015);
# set `to` to 2017 if outcome_set = "ap".
ALT_OUTCOMES <- tribble(~main, ~y, ~spec, ~to,                          # O-rows: alternative versions of each main outcome
                        "all_read", "read_ap",        "O1 read_ap (AMI+HF), to 2017",                  2017,
                        "all_read", "hf_readmission", "O2 HF readmission (main before 3 Nov), to 2017", 2017,
                        "all_mort", "mort_ap",        "O1 mort_ap (AMI+HF), to 2017",                  2017)

hd <- read_rds(paste0(created_data_path, "/hospital_data.rds"))
need <- c("Filer.EIN", "TaxYr", "in_leadership_data", "MCRNUM", "general", "hrrp_pre_measured", "err_wmean_pre", "n_cond_err_above1_pre", "bed_num", "teaching", "independent",
          "oper_margin", "mcare_discharges", "tot_discharges", "n_md_exec", "ceo_md", "h_ceo_md", "ceo_clinical", "h_ceo_clinical", "has_cmo", "exec_perc_clinical_no_vp", "h_cmo_title", "post_sys_change", "ever_sys_change_0916",
          "hf_readmission", "read_ap", "all_read", "all_mort", "mort_ap", "outcome_win_start", "outcome_win_end", "any_readm_bridged", "state", "mcaid_exp_win", "mcaid_exp_date", "cmi", "h_ceo_same_990", "c_ceo_md", "c_ceo_clinical", "c_ceo_md_nofb", "c_ceo_clinical_nofb", "SYSID",
          "s_member", "s_uid", "s_type", "s_name", "s_ceo_md", "s_ceo_clinical", "s_same_hosp", "d_class", "d_is_sys", "ceo_entry", "h_ceo_change",
          "hhi_hrr0", "hhi_hsa0", "nfirms_hrr0", "nfirms_hsa0")   # market concentration (data02, item J)
if (length(miss <- setdiff(need, names(hd)))) stop("Not in hospital_data: ", paste(miss, collapse = ", "))
first_nn <- function(x) { x <- x[!is.na(x)]; if (length(x)) x[[1]] else NA }
role_split <- function(has_md, ceo_md, cmo) case_when(
  has_md %in% 0 & !ceo_md %in% 1              ~ RS_GRP[1],
  has_md %in% 1 & ceo_md %in% 1               ~ RS_GRP[2],
  has_md %in% 1 & ceo_md %in% 0 & cmo %in% 1  ~ RS_GRP[3],
  has_md %in% 1 & ceo_md %in% 0 & cmo %in% 0  ~ RS_GRP[4],
  has_md %in% 0 & ceo_md %in% 1               ~ RS_GRP[5],
  has_md %in% 1                               ~ RS_GRP[6],
  TRUE ~ NA_character_)

# managing vs organisational type (item C, pre-specified 3 Nov; coded 6 Nov before results). Annual combined CEO status WITHOUT the 990
# fallback (HIMSS identity), leadership years 2008-2016 (leadership data only; outcomes still start in 2009).
#   AT = type 1 in 2012 (2011 if 2012 missing); BEFORE = type 1 in some year 2008-11 but not AT; AFTER = type 1 only from 2013 on;
#   NEVER = type 0 in every observed year 2008-2016 (reference). NA when status in 2012 and 2011 is unknown.
MVT_LEVELS <- c("NEVER", "AT", "BEFORE", "AFTER")
# pooled version (6 Nov, user decision AFTER the four-group results): BY = clinical leader at or before the policy (AT + BEFORE)
MVT_POOL <- c(NEVER = "NEVER", AT = "BY", BEFORE = "BY", AFTER = "AFTER")
mvt_spec <- function(pooled) if (pooled) list(lv = c("NEVER", "BY", "AFTER"), contr = list(c("BY", "AFTER"))) else
  list(lv = MVT_LEVELS, contr = list(c("AT", "BEFORE"), c("AT", "AFTER"), c("BEFORE", "AFTER")))
mvt_group <- function(x, yr) {
  at <- first_nn(c(x[yr == 2012], x[yr == 2011]))
  if (is.na(at)) return(NA_character_)
  if (at %in% 1) return("AT")
  ok <- !is.na(x) & yr %in% 2008:2016
  if (any(x[ok & yr %in% 2008:2011] == 1)) return("BEFORE")
  if (any(x[ok & yr %in% 2013:2016] == 1)) return("AFTER")
  "NEVER"
}

# ---- build a panel: one row per hospital-year, baseline covariates, fixed type ---------------------------------------
build <- function(df) {
  df <- df %>% filter(TaxYr %in% c(2008:max(YEARS))) %>%
    mutate(has_md = as.integer(n_md_exec > 0),
           post = as.integer(TaxYr >= 2012),
           share_post = pmax(0, pmin(1, as.numeric(outcome_win_end - RULE_DATE) / as.numeric(outcome_win_end - outcome_win_start))),
           post_sys_change = coalesce(post_sys_change, 0L))
  b <- df %>% group_by(id) %>% summarise(
    hrrp     = any(hrrp_pre_measured %in% TRUE),
    general0 = any(general[TaxYr %in% 2009:2011] %in% 1),
    lbeds0   = { v <- mean(bed_num[TaxYr %in% 2009:2011], na.rm = TRUE); if (is.finite(v) && v > 0) log(v) else NA_real_ },
    teaching0 = { x <- as.numeric(teaching[TaxYr %in% 2009:2011]); x <- x[!is.na(x)]; if (length(x)) max(x) else NA_real_ },
    indep0   = { x <- as.numeric(independent[TaxYr %in% 2009:2011]); x <- x[!is.na(x)]; if (length(x)) max(x) else NA_real_ },
    margin0  = { x <- oper_margin[which(TaxYr %in% 2009:2011 & is.finite(oper_margin))]; if (length(x)) mean(x) else NA_real_ },
    cmi0     = { x <- cmi[TaxYr %in% 2009:2011]; x <- x[!is.na(x)]; if (length(x)) mean(x) else NA_real_ },
    mcare0   = { x <- (mcare_discharges / tot_discharges)[which(TaxYr %in% 2009:2011 & tot_discharges > 0)]; x <- x[is.finite(x)]; if (length(x)) mean(x) else NA_real_ },
    exposure = { x <- pmax(err_wmean_pre - 1, 0); first_nn(x) },
    pen0     = first_nn(as.integer(n_cond_err_above1_pre > 0)),
    ceo_same0 = first_nn(c(h_ceo_same_990[TaxYr == TYPE_YEARS[1]], h_ceo_same_990[TaxYr == TYPE_YEARS[2]])),   # V1: 990 CEO = HIMSS CEO     # would be penalised in FY2013 (any condition ERR > 1); added 3 Nov
    all_read0     = { x <- all_read[TaxYr %in% 2009:2010]; x <- x[!is.na(x)]; if (length(x)) mean(x) else NA_real_ },
    all_read_2011 = first_nn(all_read[TaxYr == 2011]),
    ever_chg = any(ever_sys_change_0916 %in% 1),
    ceo_chg1112 = any((ceo_entry %in% 1 | h_ceo_change %in% 1) & TaxYr %in% 2011:2012),   # new CEO in 2011 or 2012, either source (T4)
    has_md0  = first_nn(c(has_md[TaxYr == TYPE_YEARS[1]], has_md[TaxYr == TYPE_YEARS[2]])),
    ceo_md0  = first_nn(c(ceo_md[TaxYr == TYPE_YEARS[1]], ceo_md[TaxYr == TYPE_YEARS[2]])),
    h_ceo_md0 = first_nn(c(h_ceo_md[TaxYr == TYPE_YEARS[1]], h_ceo_md[TaxYr == TYPE_YEARS[2]])),
    ceo_clinical0   = first_nn(c(ceo_clinical[TaxYr == TYPE_YEARS[1]], ceo_clinical[TaxYr == TYPE_YEARS[2]])),
    h_ceo_clinical0 = first_nn(c(h_ceo_clinical[TaxYr == TYPE_YEARS[1]], h_ceo_clinical[TaxYr == TYPE_YEARS[2]])),
    c_ceo_md0       = first_nn(c(c_ceo_md[TaxYr == TYPE_YEARS[1]], c_ceo_md[TaxYr == TYPE_YEARS[2]])),               # combined measure (3 Nov)
    c_ceo_clinical0 = first_nn(c(c_ceo_clinical[TaxYr == TYPE_YEARS[1]], c_ceo_clinical[TaxYr == TYPE_YEARS[2]])),
    c_ceo_md_nofb0       = first_nn(c(c_ceo_md_nofb[TaxYr == TYPE_YEARS[1]], c_ceo_md_nofb[TaxYr == TYPE_YEARS[2]])),       # without the 990 fallback
    c_ceo_clinical_nofb0 = first_nn(c(c_ceo_clinical_nofb[TaxYr == TYPE_YEARS[1]], c_ceo_clinical_nofb[TaxYr == TYPE_YEARS[2]])),
    hhi_hrr0 = first_nn(hhi_hrr0), hhi_hsa0 = first_nn(hhi_hsa0), nfirms_hrr0 = first_nn(nfirms_hrr0), nfirms_hsa0 = first_nn(nfirms_hsa0),
    mvt_md0   = mvt_group(c_ceo_md_nofb, TaxYr),         # item C: AT / BEFORE / AFTER / NEVER (physician CEO) - combined (Panel B)
    mvt_v_md0   = mvt_group(ifelse(h_ceo_same_990 %in% 1, ceo_md, NA), TaxYr),         # main (agreement) definition, by year (Panel A, 6 Nov)
    mvt_v_clin0 = mvt_group(ifelse(h_ceo_same_990 %in% 1, ceo_clinical, NA), TaxYr),
    mvt_clin0 = mvt_group(c_ceo_clinical_nofb, TaxYr),   # item C (clinical CEO: physician or nurse)
    has_cmo0  = first_nn(c(has_cmo[TaxYr == TYPE_YEARS[1]], has_cmo[TaxYr == TYPE_YEARS[2]])),
    h_cmo0    = first_nn(c(h_cmo_title[TaxYr == TYPE_YEARS[1]], h_cmo_title[TaxYr == TYPE_YEARS[2]])),
    clin_share_raw0 = first_nn(c(exec_perc_clinical_no_vp[TaxYr == TYPE_YEARS[1]], exec_perc_clinical_no_vp[TaxYr == TYPE_YEARS[2]])),
    fp       = first_nn(fp),
    st       = first_nn(c(state[TaxYr %in% 2009:2011], state)),
    sys0     = first_nn(as.character(SYSID[TaxYr %in% 2009:2011])),                  # AHA system id at baseline (NA = independent)
    # HIMSS system CEO (PART 8), type years as for the other types
    s_member0       = first_nn(c(s_member[TaxYr == TYPE_YEARS[1]], s_member[TaxYr == TYPE_YEARS[2]])),
    s_uid0          = first_nn(c(s_uid[TaxYr == TYPE_YEARS[1]], s_uid[TaxYr == TYPE_YEARS[2]])),
    s_ceo_md0       = first_nn(c(s_ceo_md[TaxYr == TYPE_YEARS[1]], s_ceo_md[TaxYr == TYPE_YEARS[2]])),
    s_ceo_clinical0 = first_nn(c(s_ceo_clinical[TaxYr == TYPE_YEARS[1]], s_ceo_clinical[TaxYr == TYPE_YEARS[2]])),
    s_same0         = first_nn(c(s_same_hosp[TaxYr == TYPE_YEARS[1]], s_same_hosp[TaxYr == TYPE_YEARS[2]])),
    d_class0        = first_nn(c(d_class[TaxYr == TYPE_YEARS[1]], d_class[TaxYr == TYPE_YEARS[2]])),      # why 990 and HIMSS CEOs differ (bridge)
    d_is_sys0       = first_nn(c(d_is_sys[TaxYr == TYPE_YEARS[1]], d_is_sys[TaxYr == TYPE_YEARS[2]])),
    s_name0         = first_nn(c(s_name[TaxYr == TYPE_YEARS[1]], s_name[TaxYr == TYPE_YEARS[2]])),
    ccn0            = first_nn(as.character(MCRNUM)),
    s_ids0          = first_nn(c(as.integer(s_type == "IDS/RHA")[TaxYr == TYPE_YEARS[1]], as.integer(s_type == "IDS/RHA")[TaxYr == TYPE_YEARS[2]])),  # multi-hospital system
    exp_by2014 = first_nn(as.integer(!is.na(mcaid_exp_date) & mcaid_exp_date < as.Date("2015-01-01"))),
    .groups = "drop") %>%
    mutate(margin_r = percent_rank(margin0), mcare_r = percent_rank(mcare0))
  b <- b %>% filter(hrrp, general0, if_all(all_of(COVS), ~ !is.na(.x)))
  cat("Hospitals dropped for missing state:", sum(is.na(b$st)), "of", nrow(b), "\n"); b <- b %>% filter(!is.na(st))
  b <- b %>% mutate(clin_share_hi0 = as.integer(clin_share_raw0 > median(clin_share_raw0, na.rm = TRUE)),
                    clus_sys = coalesce(sys0, id),                                     # cluster = system (independents are their own cluster)
                    clus_hs  = coalesce(if_else(s_member0 %in% 1, s_uid0, NA_character_), id),   # cluster = HIMSS system (PART 8)
                    # MAIN CEO DEFINITION from 6 Nov (user, after results): 990 CEO status on hospitals where the 990-assigned CEO is the HIMSS
                    # CEO in the type year (= the former V1); NA elsewhere, so those hospitals leave the sample for these types. Nonprofits only.
                    v_ceo_md0       = ifelse(ceo_same0 %in% 1, ceo_md0, NA),
                    v_ceo_clinical0 = ifelse(ceo_same0 %in% 1, ceo_clinical0, NA),
                    # role split on the main definition (6 Nov): agreement hospitals only
                    role_grp0 = if_else(ceo_same0 %in% 1, role_split(has_md0, v_ceo_md0, has_cmo0), NA_character_),
                    # item J: high competition = HHI below the panel median; continuous HHI per SD (higher = MORE concentrated)
                    comp_hi_hrr0 = as.integer(hhi_hrr0 < median(hhi_hrr0, na.rm = TRUE)), comp_hi_hsa0 = as.integer(hhi_hsa0 < median(hhi_hsa0, na.rm = TRUE)),
                    hhi_hrr_sd0 = (hhi_hrr0 - mean(hhi_hrr0, na.rm = TRUE)) / sd(hhi_hrr0, na.rm = TRUE),
                    hhi_hsa_sd0 = (hhi_hsa0 - mean(hhi_hsa0, na.rm = TRUE)) / sd(hhi_hsa0, na.rm = TRUE),
                    # margin heterogeneity (6 Nov): low margin = baseline margin rank below the panel median; continuous = margin rank per SD
                    margin_lo0 = as.integer(margin_r < median(margin_r, na.rm = TRUE)),
                    margin_rsd0 = (margin_r - mean(margin_r, na.rm = TRUE)) / sd(margin_r, na.rm = TRUE))
  b <- b %>% mutate(rs_ceo0    = case_when(role_grp0 == RS_GRP[1] ~ 0L, role_grp0 == RS_GRP[2] ~ 1L),             # each group vs no physician exec
                    rs_nonceo0 = case_when(role_grp0 == RS_GRP[1] ~ 0L, role_grp0 %in% RS_GRP[3:4] ~ 1L),
                    rs_cmo0    = case_when(role_grp0 == RS_GRP[1] ~ 0L, role_grp0 == RS_GRP[3] ~ 1L),
                    rs_oth0    = case_when(role_grp0 == RS_GRP[1] ~ 0L, role_grp0 == RS_GRP[4] ~ 1L),
                    rs_md_core0 = if_else(role_grp0 %in% RS_GRP[1:4], as.integer(has_md0), NA_integer_),         # has_md0 without groups x / y
                    rs_ceo_vs_nonceo0 = case_when(role_grp0 == RS_GRP[2] ~ 1L, role_grp0 %in% RS_GRP[3:4] ~ 0L))  # among hospitals with a physician exec
  b$exposure_sd <- (b$exposure - mean(b$exposure, na.rm = TRUE)) / sd(b$exposure, na.rm = TRUE)
  b$cmi0_sd <- (b$cmi0 - mean(b$cmi0)) / sd(b$cmi0)                       # moderators (3 Nov): baseline case mix per SD, and above the median
  b$cmi_hi0 <- as.integer(b$cmi0 > median(b$cmi0))
  df %>% filter(TaxYr %in% YEARS) %>% select(-any_of(c("fp", setdiff(intersect(names(df), names(b)), "id")))) %>% inner_join(b, by = "id")   # b's hospital-level versions win (6 Nov)
}
A <- hd %>% filter(in_leadership_data, !is.na(Filer.EIN)) %>% mutate(id = as.character(Filer.EIN), fp = 0L) %>% build()
B <- hd %>% filter(!is.na(MCRNUM)) %>% group_by(MCRNUM, TaxYr) %>% slice(1) %>% ungroup() %>%
  mutate(id = as.character(MCRNUM), fp = as.integer(!in_leadership_data)) %>%
  group_by(id) %>% mutate(fp = as.integer(mean(fp) > 0.5)) %>% ungroup() %>% build()

cat("\nHospitals with each main outcome, by window-end year (Panel A / B):\n")
print(bind_rows(A %>% mutate(panel = "A"), B %>% mutate(panel = "B")) %>% group_by(panel, TaxYr) %>%
        summarise(all_read = sum(!is.na(all_read)), all_mort = sum(!is.na(all_mort)), .groups = "drop") %>%
        pivot_wider(names_from = panel, values_from = c(all_read, all_mort)), n = 20)

# CEO types = COMBINED measure (3 Nov, DECISIONS.md): who the CEO is from HIMSS, physician status from the validated 990 when that person is
# on a 990, else the HIMSS credential; clinical = physician or nurse in either source. Team types stay from the 990s.
# The 990-measure CEO types are kept as a side-by-side comparison (tables only). HIMSS-only types are superseded (uncomment to restore).
TYPES <- tribble(~panel, ~type, ~label,
                 # MAIN (6 Nov): agreement definition - 990 CEO status where the 990 and HIMSS name the same CEO in the type year
                 "A", "v_ceo_md0",       "Physician CEO (agreement)",
                 "A", "v_ceo_clinical0", "Clinical CEO: physician or nurse (agreement)",
                 "A", "has_md0",         "Physician executive (990)",
                 # comparison from 6 Nov (main 3-6 Nov): combined measure; Panel B (incl. for-profits) has no 990, so it stays combined
                 "A", "c_ceo_md0",       "Physician CEO (combined)",
                 "B", "c_ceo_md0",       "Physician CEO (combined, incl. for-profits)",
                 "A", "c_ceo_clinical0", "Clinical CEO: physician or nurse (combined)",
                 "B", "c_ceo_clinical0", "Clinical CEO: physician or nurse (combined, incl. for-profits)",
                 # comparison: the 990 measure used before 3 Nov (algorithm-assigned CEO)
                 "A", "ceo_md0",         "Physician CEO (990 measure)",
                 "A", "ceo_clinical0",   "Clinical CEO: physician or nurse (990 measure)",
                 # comparison: combined measure without the 990 fallback (the pre-specified version, 3 Nov)
                 "A", "c_ceo_md_nofb0",       "Physician CEO (combined, no 990 fallback)",
                 "A", "c_ceo_clinical_nofb0", "Clinical CEO: physician or nurse (combined, no 990 fallback)",
                 # "B", "h_ceo_md0",       "Physician CEO (HIMSS, incl. for-profits)",
                 # "B", "h_ceo_clinical0", "Clinical CEO: physician or nurse (HIMSS, incl. for-profits)",
                 # other leadership dimensions (added 2 Nov; rationale in DECISIONS.md)
                 "A", "has_cmo0",        "CMO in place (990)",
                 "B", "h_cmo0",          "CMO-titled executive (HIMSS, incl. for-profits)",
                 "A", "clin_share_hi0",  "Clinical share of executives above median (990)")
# CMO and clinical-share types (3 Nov): still estimated and saved (appendix), but left out of the figures; also used by analysis03-05
APPENDIX_TYPES <- c("has_cmo0", "h_cmo0", "clin_share_hi0", "ceo_md0", "ceo_clinical0", "c_ceo_md_nofb0", "c_ceo_clinical_nofb0")   # + the 990-measure CEO types (comparison, tables only)
FIG_LABELS <- TYPES$label[!TYPES$type %in% APPENDIX_TYPES &
                            !TYPES$label %in% c("Physician CEO (combined)", "Clinical CEO: physician or nurse (combined)")]   # 6 Nov: combined (A) = comparison
MVT_TYPES <- tribble(~panel, ~gvar, ~label,                                         # item C (also used by analysis04)
                     "A", "mvt_v_md0",   "Physician CEO (agreement)",          # 6 Nov: main definition, year by year (990 status in years
                     "A", "mvt_v_clin0", "Clinical CEO (agreement)",           #   where the 990 and HIMSS name the same CEO; NA otherwise)
                     "B", "mvt_md0",   "Physician CEO (combined, no fallback, incl. for-profits)",
                     "B", "mvt_clin0", "Clinical CEO (combined, no fallback, incl. for-profits)")
cat("Hospitals by panel and type (type measured", paste(TYPE_YEARS, collapse = "/"), "):\n")
print(pmap_dfr(TYPES, function(panel, type, label) {
  d <- if (panel == "A") A else B
  d %>% distinct(id, t = .data[[type]], fp) %>% filter(!is.na(t)) %>% group_by(t) %>%
    summarise(hospitals = n(), for_profit = sum(fp == 1), .groups = "drop") %>% mutate(panel = panel, type = label)
}))

# ---- weights (entropy balancing, ATT: type 0 reweighted to type 1) ---------------------------------------------------
get_w <- function(d, type, covs) {
  h <- d %>% distinct(id, .keep_all = TRUE) %>% select(id, t = all_of(type), all_of(covs)) %>% filter(!is.na(t), if_all(all_of(covs), ~ !is.na(.x)))
  if (length(unique(h$t)) < 2 || min(table(h$t)) < 10) return(NULL)
  W <- try(weightit(as.formula(paste("t ~", paste(covs, collapse = " + "))), data = h, method = "ebal", estimand = "ATT"), silent = TRUE)
  if (inherits(W, "try-error")) return(NULL)
  cw <- W$weights[h$t == 0]
  tibble(id = h$id, w = W$weights) %>% structure(ess = sum(cw)^2 / sum(cw^2), n0 = sum(h$t == 0), n1 = sum(h$t == 1))
}
pre_levels <- function(d, y) d %>% filter(TaxYr %in% 2009:2010) %>% select(id, TaxYr, v = all_of(y)) %>%
  pivot_wider(names_from = TaxYr, values_from = v, names_prefix = "pre_")
covs_for <- function(panel) c(COVS, EXTRA_MATCH, if (panel == "B") "fp")   # Panel B also balances ownership
BUILD_DONE <- TRUE   # analysis04 runs this script up to here (panels, types, weights) and stops; keep this line after the weights section

# ---- one estimate -----------------------------------------------------------------------------------------------
fit <- function(d, type, y, spec, wcovs = COVS, postv = "post", weighted = TRUE, extra_rhs = "", key = "t:pv", from = min(YEARS), to = max(YEARS),
                aca = TRUE, fe = "id + TaxYr", clus = ~id) {
  dd <- d %>% filter(TaxYr >= from, TaxYr <= to, !is.na(.data[[y]]), !is.na(.data[[type]])) %>%
    mutate(yv = .data[[y]], t = .data[[type]], pv = .data[[postv]]) %>% balance_ids()
  if (any(str_detect(wcovs, "^pre_"))) dd <- dd %>% left_join(pre_levels(d, y), by = "id")
  w <- NULL
  if (weighted) { w <- get_w(dd, "t", wcovs); if (is.null(w)) return(NULL); dd <- dd %>% inner_join(w, by = "id") } else dd$w <- 1
  ucov <- wcovs[!str_detect(wcovs, "^pre_")]
  rhs <- paste0("t:pv + post_sys_change", if (aca) " + mcaid_exp_win", if (!weighted && length(ucov)) paste0(" + pv:(", paste(ucov, collapse = " + "), ")") else "", extra_rhs)
  m <- try(feols(as.formula(paste0("yv ~ ", rhs, " | ", fe)), data = dd, weights = ~w, cluster = clus), silent = TRUE)
  if (inherits(m, "try-error")) return(NULL)
  ct <- coeftable(m); if (!key %in% rownames(ct)) return(NULL)
  r <- ct[key, ]; pre <- dd %>% filter(TaxYr < 2012); mp <- weighted.mean(pre$yv, pre$w); sdp <- sd(pre$yv)
  tibble(spec = spec, outcome = y, est = r[[1]], se = r[[2]], p = r[[4]], ci_lo = r[[1]] - 1.96 * r[[2]], ci_hi = r[[1]] + 1.96 * r[[2]],
         mean_pre = mp, sd_pre = sdp,
         ci_sd  = sprintf("[%.2f, %.2f]", (r[[1]] - 1.96 * r[[2]]) / sdp, (r[[1]] + 1.96 * r[[2]]) / sdp),
         ci_pct = sprintf("[%.1f%%, %.1f%%]", 100 * (r[[1]] - 1.96 * r[[2]]) / mp, 100 * (r[[1]] + 1.96 * r[[2]]) / mp),
         n_type1 = n_distinct(dd$id[dd$t == 1]), n_type0 = n_distinct(dd$id[dd$t == 0]),
         ess_type0 = if (weighted) round(attr(w, "ess"), 1) else NA_real_, N = nobs(m))
}

# ---- all pre-specified estimates -------------------------------------------------------------------------------
# share of hospitals in states that expanded Medicaid in 2014: type 1 vs type 0, raw and reweighted
cat("\nShare of hospitals in 2014 Medicaid-expansion states:\n")
print(pmap_dfr(TYPES, function(panel, type, label) {
  d <- if (panel == "A") A else B; w <- get_w(d, type, covs_for(panel))
  h <- d %>% distinct(id, .keep_all = TRUE) %>% mutate(t = .data[[type]]) %>% filter(!is.na(t)); if (!is.null(w)) h <- h %>% left_join(w, by = "id")
  tibble(type = label, type1 = mean(h$exp_by2014[h$t == 1]), type0_raw = mean(h$exp_by2014[h$t == 0]),
         type0_weighted = if (is.null(w)) NA_real_ else weighted.mean(h$exp_by2014[h$t == 0], h$w[h$t == 0], na.rm = TRUE))
}) %>% mutate(across(where(is.numeric), ~ round(.x, 3))), width = Inf)

# ---- baseline case mix diagnostic (uses no type x post estimate) ------------------------------------------------------
# (1) how different are the types in baseline CMI, raw and after weighting on the OTHER covariates (standardised difference);
# (2) does baseline CMI predict each main outcome's path, all hospitals pooled, ignoring leadership type (event study on cmi0 in SD).
cat("\n================ Baseline case mix: standardised difference type 1 - type 0 ================\n")
print(pmap_dfr(TYPES, function(panel, type, label) {
  d <- if (panel == "A") A else B; w <- get_w(d, type, setdiff(covs_for(panel), "cmi0"))
  h <- d %>% distinct(id, .keep_all = TRUE) %>% mutate(t = .data[[type]]) %>% filter(!is.na(t)); if (!is.null(w)) h <- h %>% left_join(w, by = "id")
  s <- sd(h$cmi0)
  tibble(type = label, cmi0_type1 = mean(h$cmi0[h$t == 1]), cmi0_type0 = mean(h$cmi0[h$t == 0]),
         std_diff_raw = (mean(h$cmi0[h$t == 1]) - mean(h$cmi0[h$t == 0])) / s,
         std_diff_after_other_covs = if (is.null(w)) NA_real_ else (mean(h$cmi0[h$t == 1]) - weighted.mean(h$cmi0[h$t == 0], h$w[h$t == 0])) / s)
}) %>% mutate(across(where(is.numeric), ~ round(.x, 3))), width = Inf)
for (P in c("A", "B")) for (i in seq_len(nrow(MAIN_OUTCOMES))) {
  y <- MAIN_OUTCOMES$y[i]
  d <- (if (P == "A") A else B) %>% filter(!is.na(.data[[y]]), TaxYr <= MAIN_OUTCOMES$to[i]) %>% mutate(yv = .data[[y]], cmi0_z = (cmi0 - mean(cmi0)) / sd(cmi0))
  m <- feols(yv ~ i(TaxYr, cmi0_z, ref = REF_YEAR) + post_sys_change + mcaid_exp_win | id + TaxYr, data = d, cluster = ~id)
  ct <- coeftable(m); ct <- ct[str_detect(rownames(ct), "cmi0_z"), , drop = FALSE]
  cat("\nPanel", P, "|", y, ": path by 1 SD of baseline CMI, relative to", REF_YEAR, "(all hospitals pooled)\n"); print(round(ct, 3))
}

run_type <- function(panel, type, label, y, to) {
  d <- if (panel == "A") A else B
  wc <- covs_for(panel)
  rows <- list(
    fit(d, type, y, "MAIN", wcovs = wc, to = to),
    fit(d, type, y, "R0 raw: unweighted, no covariates", wcovs = character(0), weighted = FALSE, to = to),
    fit(d, type, y, "A0 no Medicaid expansion control", wcovs = wc, aca = FALSE, to = to),
    fit(d, type, y, "A1 state x year FE", wcovs = wc, aca = FALSE, fe = "id + st^TaxYr", to = to),
    fit(d, type, y, "C0 weights without baseline case mix", wcovs = setdiff(wc, "cmi0"), to = to),
    fit(d, type, y, "W0 unweighted, covariates x post", wcovs = wc, weighted = FALSE, to = to),
    fit(d, type, y, "W2 weights incl. 2009-10 outcome", wcovs = c(wc, "pre_2009", "pre_2010"), to = to),
    fit(d, type, y, "T1 share of window after rule", wcovs = wc, postv = "share_post", to = to),
    fit(d %>% filter(TaxYr != 2012), type, y, "T2 drop window ending 2012", wcovs = wc, to = to),
    if (to >= 2016 && str_detect(y, "read")) fit(d %>% filter(!(TaxYr == 2016 & any_readm_bridged %in% TRUE)), type, y, "T3 drop bridged 2016 window", wcovs = wc, to = to),
    fit(d %>% filter(!ever_chg), type, y, "(a) no system change 2009-16", wcovs = wc, to = to),
    # T4 (3 Nov): stable leadership - drop hospitals (type 1 and type 0) with a new CEO in 2011 or 2012 in either source, so the CEO typed in
    # 2010 is the one in post when the rule and the first penalties arrived. Same hospitals for every measure.
    fit(d %>% filter(!ceo_chg1112), type, y, paste0("T4 no CEO change in 2011-12 (", n_distinct(d$id[!d$ceo_chg1112 & d[[type]] %in% 1]), " type-1)"), wcovs = wc, to = to),
    fit(d %>% filter(!ceo_chg1112, !is.na(exposure_sd)), type, y, "T4 + E1 triple: no CEO change in 2011-12", wcovs = wc,
        extra_rhs = " + exposure_sd:pv + t:exposure_sd:pv", key = "t:pv:exposure_sd", to = to),
    fit(d %>% filter(!is.na(exposure_sd)), type, y, "E1 triple: type x exposure x post", wcovs = wc,
        extra_rhs = " + exposure_sd:pv + t:exposure_sd:pv", key = "t:pv:exposure_sd", to = to),
    fit(d %>% filter(!is.na(pen0)), type, y, "E1b triple: type x penalised in FY2013 x post", wcovs = wc,
        extra_rhs = " + pen0:pv + t:pen0:pv", key = "t:pv:pen0", to = to),
    # moderation by baseline case mix (pre-specified 3 Nov, before results); MAIN weights, which balance the type means of cmi0
    fit(d, type, y, "M1 triple: type x baseline case mix (per SD) x post", wcovs = wc,
        extra_rhs = " + cmi0_sd:pv + t:cmi0_sd:pv", key = "t:pv:cmi0_sd", to = to),
    fit(d, type, y, "M1b triple: type x high case mix (above median) x post", wcovs = wc,
        extra_rhs = " + cmi_hi0:pv + t:cmi_hi0:pv", key = "t:pv:cmi_hi0", to = to),
    fit(d, type, y, "M1b base: type gap among low case mix hospitals", wcovs = wc,
        extra_rhs = " + cmi_hi0:pv + t:cmi_hi0:pv", key = "t:pv", to = to))
  # S1 / S2 (3 Nov, before results): system members vs independent hospitals at baseline (indep0), weights without indep0. Tells whether
  # V1's smaller average gap reflects its tilt toward independents (the 990 often names the system CEO for system members).
  wc_s <- setdiff(wc, "indep0")
  rows <- c(rows, list(
    fit(d %>% filter(indep0 %in% 0), type, y, paste0("S1 system members only (", n_distinct(d$id[d$indep0 %in% 0 & d[[type]] %in% 1]), " type-1)"), wcovs = wc_s, to = to),
    fit(d %>% filter(indep0 %in% 1), type, y, paste0("S2 independents only (", n_distinct(d$id[d$indep0 %in% 1 & d[[type]] %in% 1]), " type-1)"), wcovs = wc_s, to = to),
    fit(d %>% filter(indep0 %in% 0, !is.na(exposure_sd)), type, y, "S1 + E1 triple: system members", wcovs = wc_s,
        extra_rhs = " + exposure_sd:pv + t:exposure_sd:pv", key = "t:pv:exposure_sd", to = to),
    fit(d %>% filter(indep0 %in% 1, !is.na(exposure_sd)), type, y, "S2 + E1 triple: independents", wcovs = wc_s,
        extra_rhs = " + exposure_sd:pv + t:exposure_sd:pv", key = "t:pv:exposure_sd", to = to),
    fit(d, type, y, "S3 triple: type x independent x post", wcovs = wc, extra_rhs = " + indep0:pv + t:indep0:pv", key = "t:pv:indep0", to = to)))
  # V1 (3 Nov, before results): 990 CEO types on hospitals where the algorithm-assigned 990 CEO is the HIMSS CEO in the type year,
  # removing CEO-assignment errors while keeping the 990's validated clinical status. Same weights, re-estimated on the subsample.
  if (type %in% c("ceo_md0", "ceo_clinical0")) {
    dv <- d %>% filter(ceo_same0 %in% 1)
    rows <- c(rows, list(fit(dv, type, y, paste0("V1 990 CEO = HIMSS CEO in type year (", n_distinct(dv$id[dv[[type]] %in% 1]), " type-1 hospitals)"),
                             wcovs = wc, to = to),
                         fit(dv %>% filter(!is.na(exposure_sd)), type, y, "V1 + E1 triple: type x exposure x post", wcovs = wc,
                             extra_rhs = " + exposure_sd:pv + t:exposure_sd:pv", key = "t:pv:exposure_sd", to = to)))
  }
  alts <- ALT_OUTCOMES %>% filter(main == .env$y)   # .env: ALT_OUTCOMES has its own column `y` (without it no O-rows were ever produced)
  rows <- c(rows, pmap(alts %>% select(-main), function(y, spec, to) fit(d, type, y, spec, wcovs = wc, to = to)))
  n_fp1 <- n_distinct(d$id[d$fp == 1 & d[[type]] %in% 1])
  # ownership heterogeneity (3 Nov, before results): HIMSS types estimated separately for nonprofits and for-profits (weights without fp).
  # H1 also separates data source from sample: the same nonprofit sample as the 990 types, measured with HIMSS.
  if (panel == "B") rows <- c(rows, list(
    fit(d %>% filter(fp == 0), type, y, "H1 nonprofits only", wcovs = setdiff(wc, "fp"), to = to),
    fit(d %>% filter(fp == 1), type, y, paste0("H2 for-profits only (", n_fp1, " type-1 hospitals)"), wcovs = setdiff(wc, "fp"), to = to)))
  if (panel == "B") rows <- c(rows, list(fit(d, type, y, paste0("E2 type gap: for-profit minus nonprofit (", n_fp1, " for-profit type-1 hospitals)"),
                                             wcovs = wc, extra_rhs = " + fp:pv + t:fp:pv", key = "t:pv:fp", to = to)))
  bind_rows(rows) %>% mutate(main_outcome = y, panel = panel, type = label, .before = 1)
}
res <- expand_grid(TYPES, MAIN_OUTCOMES %>% select(y, to)) %>% pmap_dfr(function(panel, type, label, y, to) run_type(panel, type, label, y, to))
# fixest may order interaction names differently; flag any extension that did not come back
miss_ext <- anti_join(expand_grid(TYPES %>% select(type = label), main_outcome = MAIN_OUTCOMES$y, spec = c("E1 triple: type x exposure x post", "E1b triple: type x penalised in FY2013 x post",
                                                                                                           "M1 triple: type x baseline case mix (per SD) x post", "M1b triple: type x high case mix (above median) x post")),
                      res, by = c("type", "main_outcome", "spec"))
if (nrow(miss_ext)) message("Check the coefficient name for: ", paste(unique(miss_ext$spec), collapse = ", "), " (print rownames(coeftable(m)))")

fmt <- function(t) t %>% mutate(across(c(est, se, ci_lo, ci_hi, mean_pre, sd_pre), ~ round(.x, 3)), p = round(p, 3))
cat("\n================ MAIN ================\n"); print(fmt(res %>% filter(spec == "MAIN")), width = Inf)
cat("\n================ ROBUSTNESS & EXTENSIONS ================\n")
print(fmt(res %>% filter(spec != "MAIN")) %>% arrange(main_outcome, panel, type, spec), width = Inf, n = 300)
write_csv(res, paste0(tab_path, "response_by_type.csv"))

# ---- side by side: CEO types under the combined measure vs the 990 measure (3 Nov) ----
CMP_SPECS <- c("MAIN", "T1 share of window after rule", "E1 triple: type x exposure x post", "W2 weights incl. 2009-10 outcome", "A1 state x year FE",
               "T4 no CEO change in 2011-12", "T4 + E1 triple: no CEO change in 2011-12")
cmp <- res %>% mutate(spec = str_remove(spec, " \\(\\d+ type-1\\)$")) %>% filter(str_detect(type, "CEO"), spec %in% CMP_SPECS) %>%
  mutate(role = if_else(str_detect(type, "^Physician"), "Physician CEO", "Clinical CEO"),
         measure = case_when(str_detect(type, "agreement") ~ "agreement, MAIN (A)", str_detect(type, "combined, incl") ~ "combined, all (B)", str_detect(type, "no 990 fallback") ~ "combined, no fallback (A)",
                             str_detect(type, "combined") ~ "combined, nonprofits (A)",
                             str_detect(type, "990 measure") ~ "990 measure (A)", TRUE ~ "HIMSS (B)"),
         cell = sprintf("%.3f (p=%.3f, n1=%s)", est, p, n_type1)) %>%
  select(main_outcome, role, spec, measure, cell) %>% pivot_wider(names_from = measure, values_from = cell) %>%
  mutate(spec = factor(spec, levels = CMP_SPECS)) %>% arrange(main_outcome, desc(role), spec)
cat("\n================ CEO types: combined measure vs 990 measure ================\n"); print(cmp, width = Inf, n = 50)
write_csv(cmp, paste0(tab_path, "response_by_type_measures.csv"))

# ---- equivalence bounds (5 Nov, DECISIONS.md item D): what each CEO estimate rules out -------------------------------------
# 90% CI (= two one-sided 5% tests): effects outside [ci90_lo, ci90_hi] are rejected. tost_bound = smallest symmetric bound d such that
# |effect| < d is established at 5% (= |est| + 1.645 se). mde80 = effect detectable with 80% power at 5% two-sided (= 2.8 se).
# Note the sign: for readmission, ci90_hi > 0 bounds how much LESS clinically led hospitals could have responded than others.
eqv <- res %>% filter(str_detect(type, "CEO"), spec %in% c("MAIN", "E1 triple: type x exposure x post")) %>%
  transmute(main_outcome, panel, type, spec, est, se, ci90_lo = est - 1.645 * se, ci90_hi = est + 1.645 * se,
            tost_bound = abs(est) + 1.645 * se, tost_bound_pct_mean = 100 * tost_bound / mean_pre, mde80 = 2.8 * se, mean_pre, n_type1)
cat("\n================ Equivalence bounds: what the CEO estimates rule out (pp; % of the pre-period mean) ================\n")
print(eqv %>% mutate(across(where(is.numeric), ~ round(.x, 3))), width = Inf, n = 60)
write_csv(eqv, paste0(tab_path, "response_by_type_equivalence.csv"))
# main table: the PRIMARY estimate (E1 exposure triple, DECISIONS.md 3 Nov) and the average gap (MAIN), every type
writeLines(as.character(kable(fmt(res %>% filter(spec %in% c("E1 triple: type x exposure x post", "MAIN"))) %>%
                                mutate(spec = if_else(spec == "MAIN", "MAIN (average gap)", "E1 (primary): x exposure")) %>%
                                arrange(main_outcome, panel, type, spec) %>%
                                select(main_outcome, spec, panel, type, est, se, p, ci_sd, ci_pct, n_type1, n_type0, ess_type0, N),
                              format = "latex", booktabs = TRUE)), paste0(tab_path, "response_by_type_main.tex"))

# ---- why do the 990 and combined measures differ? (3 Nov, after the combined results; logged in DECISIONS.md) -------------------
# Panel A only (the 990 measure exists only for nonprofits). Rows, for physician and clinical CEO, MAIN and E1 triple:
#   R1 agreement hospitals (HIMSS and the 990 name the same CEO in the type year): both measures give the same type
#   Ra disagreement hospitals, typed by the 990 measure (the 990's algorithm-assigned CEO)
#   Rb the same disagreement hospitals, typed by the combined measure (the CEO HIMSS names)
#   Rc "physician-led system": type 1 = the 990's assigned CEO is a physician but the hospital's own CEO (HIMSS, no fallback) is not;
#      type 0 = neither is. Hospitals whose own CEO is a physician are left out. Clustered by hospital, then by system; also unweighted.
#   Rd Rc without the systems that supply 3 or more type-1 hospitals (does a handful of systems carry it?)
src_d <- A %>% mutate(dis0 = ceo_same0 %in% 0,
                      sysmd0   = case_when(c_ceo_md_nofb0 %in% 1 ~ NA_integer_, c_ceo_md_nofb0 %in% 0 & ceo_md0 %in% 1 ~ 1L,
                                           c_ceo_md_nofb0 %in% 0 & ceo_md0 %in% 0 ~ 0L, TRUE ~ NA_integer_),
                      sysclin0 = case_when(c_ceo_clinical_nofb0 %in% 1 ~ NA_integer_, c_ceo_clinical_nofb0 %in% 0 & ceo_clinical0 %in% 1 ~ 1L,
                                           c_ceo_clinical_nofb0 %in% 0 & ceo_clinical0 %in% 0 ~ 0L, TRUE ~ NA_integer_))
ROLES <- tribble(~role, ~t990, ~tcomb, ~tsys,
                 "Physician CEO", "ceo_md0",       "c_ceo_md0",       "sysmd0",
                 "Clinical CEO",  "ceo_clinical0", "c_ceo_clinical0", "sysclin0")
cat("\n================ Why the measures differ: hospitals by type (Panel A, type year) ================\n")
for (r in seq_len(nrow(ROLES))) {
  h <- src_d %>% distinct(id, .keep_all = TRUE)
  cat("\n", ROLES$role[r], ": disagreement hospitals, 990 measure x combined measure\n", sep = "")
  print(h %>% filter(dis0) %>% count(m990 = .data[[ROLES$t990[r]]], combined = .data[[ROLES$tcomb[r]]]))
  big <- h %>% filter(.data[[ROLES$tsys[r]]] %in% 1, !is.na(sys0)) %>% count(sys0, name = "type1_hospitals") %>% arrange(desc(type1_hospitals))
  cat(ROLES$role[r], ": systems behind the 'physician-led system' type-1 hospitals (AHA SYSID; NA-system hospitals are independent)\n"); print(big, n = 30)
}
src_rows <- function(d, type, y, to, tag, ...) {
  wc <- covs_for("A")
  list(fit(d, type, y, paste0(tag, " | MAIN"), wcovs = wc, to = to, ...),
       fit(d %>% filter(!is.na(exposure_sd)), type, y, paste0(tag, " | E1 triple"), wcovs = wc, to = to,
           extra_rhs = " + exposure_sd:pv + t:exposure_sd:pv", key = "t:pv:exposure_sd", ...))
}
res_src <- pmap_dfr(expand_grid(ROLES, MAIN_OUTCOMES %>% select(y, to)), function(role, t990, tcomb, tsys, y, to) {
  big <- src_d %>% distinct(id, .keep_all = TRUE) %>% filter(.data[[tsys]] %in% 1, !is.na(sys0)) %>% count(sys0) %>% filter(n >= 3) %>% pull(sys0)
  bind_rows(c(
    src_rows(src_d %>% filter(ceo_same0 %in% 1), t990,  y, to, "R1 agreement hospitals (both measures)"),
    src_rows(src_d %>% filter(dis0),             t990,  y, to, "Ra disagreement hospitals, 990 measure"),
    src_rows(src_d %>% filter(dis0),             tcomb, y, to, "Rb disagreement hospitals, combined measure"),
    src_rows(src_d, tsys, y, to, "Rc physician-led system (990-assigned CEO has it, hospital CEO not)"),
    src_rows(src_d, tsys, y, to, "Rc clustered by system", clus = ~clus_sys),
    src_rows(src_d, tsys, y, to, "Rc unweighted, covariates x post", weighted = FALSE),
    src_rows(src_d %>% filter(!sys0 %in% big), tsys, y, to, paste0("Rd Rc without systems with 3+ type-1 hospitals (", length(big), " dropped)")),
    src_rows(src_d %>% filter(!sys0 %in% big), tsys, y, to, "Rd unweighted, covariates x post", weighted = FALSE))) %>%
    mutate(main_outcome = y, role = role, .before = 1)
})
cat("\n================ Why the measures differ: estimates ================\n")
print(res_src %>% transmute(main_outcome, role, spec, est = round(est, 3), se = round(se, 3), p = round(p, 3), n_type1, n_type0), width = Inf, n = 100)
write_csv(res_src, paste0(tab_path, "response_by_type_sources.csv"))

# ---- SYSTEM CEO TEST (pre-specified 3 Nov, before results; DECISIONS.md) ------------------------------------------------------
# Question: for system hospitals, is the response tied to the HOSPITAL's CEO or to the SYSTEM's CEO?
# Sample: system members in the type year (HIMSS parent chain) with both types known. Two treatments entered together, so no entropy
# balancing: unweighted, baseline covariates x post (as W0), hospital + year FE, ACA and system-change controls, clustered by HIMSS system.
#   SY1 MAIN    h:pv + s:pv   (h = hospital CEO, combined measure; s = system CEO)
#   SY2 E1      + exposure triples for each:  h:pv:exposure_sd, s:pv:exposure_sd
#   SY3 cells   neither (ref) / hospital CEO only / system CEO only / both
#   SY4 SY1 + SY2 without hospitals whose system CEO IS the hospital's CEO (cleanest separation of the two leaders)
#   SY5 independents (no HIMSS parent): the hospital CEO gap, which is also the top leader's (unweighted, covariates x post; MAIN and E1)
fit_sys <- function(d, panel, hvar, svar, y, to, spec, extra = "", cells = FALSE) {
  wc <- covs_for(panel)
  dd <- d %>% filter(TaxYr <= to, !is.na(.data[[y]]), !is.na(.data[[hvar]]), !is.na(.data[[svar]])) %>%
    mutate(yv = .data[[y]], h = .data[[hvar]], s = .data[[svar]], pv = post,
           g = factor(case_when(h == 0 & s == 0 ~ "0 neither", h == 1 & s == 0 ~ "1 hospital CEO only", h == 0 & s == 1 ~ "2 system CEO only", TRUE ~ "3 both"))) %>%
    balance_ids()
  core <- if (cells) "i(g, pv, ref = '0 neither')" else "h:pv + s:pv"
  rhs <- paste0(core, " + post_sys_change + mcaid_exp_win + pv:(", paste(wc, collapse = " + "), ")", extra)
  m <- try(feols(as.formula(paste0("yv ~ ", rhs, " | id + TaxYr")), data = dd, cluster = ~clus_hs), silent = TRUE)
  if (inherits(m, "try-error")) { message("fit_sys failed: ", spec); return(NULL) }
  # R names an interaction by the order its variables first appear in the formula (s:pv comes out as pv:s), so match on the pieces
  ct <- coeftable(m); rn <- rownames(ct)
  keep <- str_detect(rn, "^g::") | (str_detect(rn, "(^|:)(h|s)(:|$)") & str_detect(rn, "(^|:)pv(:|$)"))
  ct <- ct[keep, , drop = FALSE]; rn <- rn[keep]
  lab <- if_else(str_detect(rn, "^g::"), rn,
                 paste0(if_else(str_detect(rn, "(^|:)h(:|$)"), "h", "s"), ":pv", if_else(str_detect(rn, "exposure_sd"), ":exposure_sd", "")))
  h0 <- dd %>% distinct(id, h, s, g)
  tibble(spec = spec, term = lab, est = ct[, 1], se = ct[, 2], p = ct[, 4],
         n_hosp = nrow(h0), n_h1 = sum(h0$h == 1), n_s1 = sum(h0$s == 1), n_both = sum(h0$h == 1 & h0$s == 1),
         n_systems = n_distinct(dd$clus_hs), N = nobs(m))
}
SYS_ROLES <- tribble(~role, ~hvar, ~svar,
                     "Physician CEO", "c_ceo_md0",       "s_ceo_md0",
                     "Clinical CEO",  "c_ceo_clinical0", "s_ceo_clinical0")
E1X <- " + exposure_sd:pv + h:pv:exposure_sd + s:pv:exposure_sd"
res_sys <- pmap_dfr(expand_grid(panel = c("A", "B"), SYS_ROLES, MAIN_OUTCOMES %>% select(y, to)), function(panel, role, hvar, svar, y, to) {
  d  <- if (panel == "A") A else B
  dm <- d %>% filter(s_member0 %in% 1)
  dx <- d %>% filter(s_ids0 %in% 1)          # members of a multi-hospital system (HIMSS IDS/RHA parent)
  di <- d %>% filter(!s_ids0 %in% 1)         # not in a multi-hospital system (single-hospital health system or no parent)
  rows <- list(
    fit_sys(dm, panel, hvar, svar, y, to, "SY1 MAIN: hospital CEO and system CEO together"),
    fit_sys(dm %>% filter(!is.na(exposure_sd)), panel, hvar, svar, y, to, "SY2 E1 triples", extra = E1X),
    fit_sys(dm, panel, hvar, svar, y, to, "SY3 cells", cells = TRUE),
    fit_sys(dm %>% filter(s_same0 %in% 0), panel, hvar, svar, y, to, "SY4 MAIN, system CEO is not the hospital CEO"),
    fit_sys(dm %>% filter(s_same0 %in% 0, !is.na(exposure_sd)), panel, hvar, svar, y, to, "SY4 E1 triples, system CEO is not the hospital CEO", extra = E1X),
    fit_sys(dx, panel, hvar, svar, y, to, "SY6 MAIN, multi-hospital systems (IDS/RHA)"),
    fit_sys(dx %>% filter(!is.na(exposure_sd)), panel, hvar, svar, y, to, "SY6 E1 triples, multi-hospital systems (IDS/RHA)", extra = E1X),
    fit_sys(dx, panel, hvar, svar, y, to, "SY6 cells, multi-hospital systems (IDS/RHA)", cells = TRUE),
    fit(di, hvar, y, "SY5 not in a multi-hospital system: hospital CEO gap (unweighted, covariates x post)", wcovs = covs_for(panel), weighted = FALSE, to = to) %>%
      { if (is.null(.)) NULL else transmute(., spec, term = "t:pv", est, se, p, n_hosp = n_type1 + n_type0, n_h1 = n_type1, N) },
    fit(di %>% filter(!is.na(exposure_sd)), hvar, y, "SY5 not in a multi-hospital system: E1 triple (unweighted, covariates x post)", wcovs = covs_for(panel), weighted = FALSE, to = to,
        extra_rhs = " + exposure_sd:pv + t:exposure_sd:pv", key = "t:pv:exposure_sd") %>%
      { if (is.null(.)) NULL else transmute(., spec, term = "t:pv:exposure_sd", est, se, p, n_hosp = n_type1 + n_type0, n_h1 = n_type1, N) })
  bind_rows(rows) %>% mutate(main_outcome = y, panel = panel, role = role, .before = 1)
})
cat("\n================ SYSTEM CEO TEST (h = hospital CEO, s = system CEO) ================\n")
print(res_sys %>% mutate(across(c(est, se, p), ~ round(.x, 3))), width = Inf, n = 200)
write_csv(res_sys, paste0(tab_path, "response_by_type_system.csv"))

# ---- COMPLEMENTARITY: clinical CEO x CMO (pre-specified 5 Nov before results; DECISIONS.md item L) -------------------------------------
# Panel A (nonprofits). Four groups from the type year: neither (reference) / clinical CEO only / CMO only / both, for clinical CEO and for
# physician CEO (combined measure) crossed with has_cmo0 (990 CMO in place). Unweighted, baseline covariates x post, hospital + year FE,
# ACA and system-change controls, clustered by hospital (four groups cannot be entropy-balanced at once).
#   K1 MAIN  i(g, pv)                                    -> each group's change vs neither
#   K2 E1    + i(g, pv x exposure_sd) + exposure_sd:pv     -> each group's exposure gradient vs neither
#   complementarity = both - (CEO only + CMO only), from the same model (est and se from the coefficient covariance)
# Outcomes: all_read, all_mort (36-month windows, to 2014) and hf1 / hf1_noceil (HF discharge instructions, 12-month windows, to 2014).
K_OUT <- tribble(~y, ~to, ~aca,
                 "all_read", 2014, "mcaid_exp_win", "all_mort", 2014, "mcaid_exp_win",
                 "hf1", 2014, "mcaid_exp", "hf1_noceil", 2014, "mcaid_exp") %>% filter(y %in% names(A))
K_CEO <- c("Clinical CEO" = "v_ceo_clinical0", "Physician CEO" = "v_ceo_md0")   # main (agreement) definition from 6 Nov
fit_cells <- function(ceo_var, y, to, aca, e1) {
  wc <- covs_for("A")
  dd <- A %>% filter(TaxYr <= to, !is.na(.data[[y]]), !is.na(.data[[ceo_var]]), !is.na(has_cmo0), if (e1) !is.na(exposure_sd) else TRUE) %>%
    mutate(yv = .data[[y]], pv = post,
           g = factor(case_when(.data[[ceo_var]] == 0 & has_cmo0 == 0 ~ "0 neither", .data[[ceo_var]] == 1 & has_cmo0 == 0 ~ "1 CEO only",
                                .data[[ceo_var]] == 0 & has_cmo0 == 1 ~ "2 CMO only", TRUE ~ "3 both"),
                      levels = c("0 neither", "1 CEO only", "2 CMO only", "3 both")),
           pvx = pv * coalesce(exposure_sd, 0)) %>% balance_ids()
  core <- if (e1) "i(g, pv, ref = '0 neither') + i(g, pvx, ref = '0 neither') + exposure_sd:pv" else "i(g, pv, ref = '0 neither')"
  fml <- as.formula(paste0("yv ~ ", core, " + post_sys_change + ", aca, " + pv:(", paste(wc, collapse = " + "), ") | id + TaxYr"))
  m <- try(feols(fml, data = dd, cluster = ~id), silent = TRUE)
  if (inherits(m, "try-error")) { message("fit_cells failed: ", y, " ", ceo_var); return(NULL) }
  b <- coef(m); V <- vcov(m); key <- if (e1) "pvx" else "pv"
  nm <- function(lv) paste0("g::", lv, ":", key)
  grp <- c("1 CEO only", "2 CMO only", "3 both")
  rows <- map_dfr(grp, function(lv) { k <- nm(lv); if (!k %in% names(b)) return(NULL)
  tibble(term = lv, est = b[[k]], se = sqrt(V[k, k])) })
  if (all(nm(grp) %in% names(b))) {                                  # both - (CEO only + CMO only)
    w <- setNames(c(-1, -1, 1), nm(grp)); est <- sum(w * b[names(w)]); se <- sqrt(as.numeric(t(w) %*% V[names(w), names(w)] %*% w))
    rows <- bind_rows(rows, tibble(term = "complementarity: both - (CEO only + CMO only)", est = est, se = se))
  }
  n_g <- dd %>% distinct(id, g) %>% count(g)
  rows %>% mutate(p = 2 * pnorm(-abs(est / se)), spec = if (e1) "K2 E1: gradient with exposure" else "K1 MAIN: change after vs before",
                  n_neither = n_g$n[n_g$g == "0 neither"], n_ceo_only = sum(n_g$n[n_g$g == "1 CEO only"]),
                  n_cmo_only = sum(n_g$n[n_g$g == "2 CMO only"]), n_both = sum(n_g$n[n_g$g == "3 both"]), N = nobs(m))
}
res_k <- pmap_dfr(expand_grid(ceo = names(K_CEO), K_OUT, e1 = c(FALSE, TRUE)), function(ceo, y, to, aca, e1)
  fit_cells(K_CEO[[ceo]], y, to, aca, e1) %>% { if (is.null(.)) NULL else mutate(., ceo_type = ceo, outcome = y, .before = 1) })
cat("\n================ COMPLEMENTARITY: clinical / physician CEO x CMO (Panel A; vs neither) ================\n")
print(res_k %>% mutate(across(c(est, se, p), ~ round(.x, 3))) %>% select(ceo_type, outcome, spec, term, est, se, p, n_neither, n_ceo_only, n_cmo_only, n_both),
      width = Inf, n = 100)
write_csv(res_k, paste0(tab_path, "response_by_type_complementarity.csv"))

# ---- PHYSICIAN EXECUTIVE SPLIT BY ROLE (pre-specified 5 Nov before results; DECISIONS.md item I) ----------------------------------
# Why does "Physician executive (990)" (has_md0) behave unlike the physician CEO types? has_md0 = 1 when ANY 990 executive is a physician,
# so its type-1 group mixes physician CEOs with physicians in other roles. Panel A (nonprofits), type year as for the other types.
# Mutually exclusive groups (role_grp0, built in build()):
#   0 no physician executive (has_md0 = 0, CEO not a physician)          <- reference for RS1
#   1 physician CEO (combined measure) and a 990 physician executive
#   2 physician executive(s), CEO not a physician, CMO in place (990 title; the 990 does not confirm the CMO is the physician)
#   3 physician executive(s), CEO not a physician, no CMO
#   x physician CEO (combined) but no 990 physician executive; y CEO or CMO status unknown -> counted, not estimated
# RS0 descriptives: counts, share of has_md0 = 1 hospitals in each group, baseline means by group.
# RS1 binary comparisons, entropy-balanced as MAIN: each group vs group 0 (rs_ceo0, rs_nonceo0 = 2+3, rs_cmo0, rs_oth0); rs_md_core0 = has_md0
#     on the groups 0-3 sample; rs_ceo_vs_nonceo0 = group 1 vs groups 2+3. Specs MAIN, E1 triple, W0 unweighted, T4 no CEO change 2011-12.
# RS2 one model, all groups vs group 0 (unweighted, covariates x post, clustered by hospital, as K1 / K2), with contrasts between groups.
# RS3 event study for the RS1 types (MAIN weights), all_read and all_mort.
cat("\n================ PHYSICIAN EXECUTIVE SPLIT: groups (Panel A, type year) ================\n")
rs_h <- A %>% distinct(id, .keep_all = TRUE) %>% mutate(role_grp0 = factor(role_grp0, levels = RS_GRP))
print(rs_h %>% count(has_md0, v_ceo_md0, has_cmo0, role_grp0), n = 40, width = Inf)
rs_bal <- rs_h %>% group_by(role_grp0) %>%
  summarise(hospitals = n(), share_of_has_md1 = sum(has_md0 %in% 1) / sum(rs_h$has_md0 %in% 1),
            across(all_of(unique(c(COVS, "exposure", "all_read0"))), ~ mean(.x, na.rm = TRUE)),
            beds = mean(exp(lbeds0)), sys_member = mean(!is.na(sys0)), clin_share = mean(clin_share_raw0, na.rm = TRUE),
            ceo_chg1112 = mean(ceo_chg1112), .groups = "drop") %>% select(-any_of("lbeds0"))
cat("\nBaseline means by group (beds = geometric mean; share_of_has_md1 = share of all has_md0 = 1 hospitals):\n")
print(rs_bal %>% mutate(across(where(is.numeric), ~ round(.x, 3))), width = Inf)
write_csv(rs_bal, paste0(tab_path, "response_by_type_role_groups.csv"))

RS_TYPES <- tribble(~type, ~label,
                    "has_md0",           "Physician executive (990), as in the main table",
                    "rs_md_core0",       "Physician executive (990), groups 0-3 only",
                    "rs_ceo0",           "1 physician CEO vs 0 none",
                    "rs_nonceo0",        "2+3 physician exec, not CEO vs 0 none",
                    "rs_cmo0",           "2 physician exec, not CEO, CMO in place vs 0 none",
                    "rs_oth0",           "3 physician exec, not CEO, no CMO vs 0 none",
                    "rs_ceo_vs_nonceo0", "1 physician CEO vs 2+3 physician exec, not CEO")
res_rs <- pmap_dfr(expand_grid(RS_TYPES, MAIN_OUTCOMES %>% select(y, to)), function(type, label, y, to) {
  wc <- covs_for("A")
  bind_rows(
    fit(A, type, y, "MAIN", wcovs = wc, to = to),
    fit(A %>% filter(!is.na(exposure_sd)), type, y, "E1 triple: type x exposure x post", wcovs = wc,
        extra_rhs = " + exposure_sd:pv + t:exposure_sd:pv", key = "t:pv:exposure_sd", to = to),
    fit(A, type, y, "W0 unweighted, covariates x post", wcovs = wc, weighted = FALSE, to = to),
    fit(A %>% filter(!ceo_chg1112), type, y, "T4 no CEO change in 2011-12", wcovs = wc, to = to)) %>%
    mutate(main_outcome = y, type = label, .before = 1)
})
cat("\n================ PHYSICIAN EXECUTIVE SPLIT: RS1 binary comparisons (entropy-balanced) ================\n")
print(res_rs %>% transmute(main_outcome, type, spec, est = round(est, 3), se = round(se, 3), p = round(p, 3), n_type1, n_type0, ess_type0),
      width = Inf, n = 100)
write_csv(res_rs, paste0(tab_path, "response_by_type_role_split.csv"))

# RS2: all groups in one model. cells = named vector role_grp0 -> cell (first cell = reference); contr = list of c(a, b) for a - b.
RS_CELLS <- list(
  four   = c(setNames(c("g0_none", "g1_ceo", "g2_cmo", "g3_other"), RS_GRP[1:4])),
  pooled = c(setNames(c("g0_none", "g1_ceo", "g23_nonceo", "g23_nonceo"), RS_GRP[1:4])))
RS_CONTR <- list(four = list(c("g1_ceo", "g2_cmo"), c("g1_ceo", "g3_other"), c("g2_cmo", "g3_other")),
                 pooled = list(c("g1_ceo", "g23_nonceo")))
fit_grp <- function(cells, contr, y, to, aca, e1) {
  wc <- covs_for("A"); lv <- unique(unname(cells))
  dd <- A %>% filter(TaxYr <= to, !is.na(.data[[y]]), role_grp0 %in% names(cells), if (e1) !is.na(exposure_sd) else TRUE) %>%
    mutate(yv = .data[[y]], pv = post, g = factor(unname(cells[role_grp0]), levels = lv), pvx = pv * coalesce(exposure_sd, 0)) %>% balance_ids()
  core <- paste0("i(g, pv, ref = '", lv[1], "')", if (e1) paste0(" + i(g, pvx, ref = '", lv[1], "') + exposure_sd:pv") else "")
  m <- try(feols(as.formula(paste0("yv ~ ", core, " + post_sys_change + ", aca, " + pv:(", paste(wc, collapse = " + "), ") | id + TaxYr")),
                 data = dd, cluster = ~id), silent = TRUE)
  if (inherits(m, "try-error")) { message("fit_grp failed: ", y); return(NULL) }
  b <- coef(m); V <- vcov(m); nm <- function(g) paste0("g::", g, ":", if (e1) "pvx" else "pv")
  rows <- map_dfr(lv[-1], function(g) if (nm(g) %in% names(b)) tibble(term = paste(g, "vs", lv[1]), est = b[[nm(g)]], se = sqrt(V[nm(g), nm(g)])))
  rows <- bind_rows(rows, map_dfr(contr, function(cc) { k <- nm(cc); if (!all(k %in% names(b))) return(NULL)
  tibble(term = paste(cc[1], "minus", cc[2]), est = b[[k[1]]] - b[[k[2]]], se = sqrt(V[k[1], k[1]] + V[k[2], k[2]] - 2 * V[k[1], k[2]])) }))
  n_g <- dd %>% distinct(id, g) %>% count(g)
  rows %>% mutate(p = 2 * pnorm(-abs(est / se)), spec = if (e1) "RS2 E1: gradient with exposure" else "RS2 MAIN: change after vs before",
                  n_by_group = paste(paste0(n_g$g, "=", n_g$n), collapse = ", "), N = nobs(m))
}
res_rs2 <- pmap_dfr(expand_grid(model = names(RS_CELLS), K_OUT, e1 = c(FALSE, TRUE)), function(model, y, to, aca, e1)
  fit_grp(RS_CELLS[[model]], RS_CONTR[[model]], y, to, aca, e1) %>% { if (is.null(.)) NULL else mutate(., model = model, outcome = y, .before = 1) })
cat("\n================ PHYSICIAN EXECUTIVE SPLIT: RS2 all groups in one model (unweighted, covariates x post) ================\n")
print(res_rs2 %>% mutate(across(c(est, se, p), ~ round(.x, 3))) %>% select(outcome, model, spec, term, est, se, p, n_by_group), width = Inf, n = 100)
write_csv(res_rs2, paste0(tab_path, "response_by_type_role_split_cells.csv"))

# RS3: event studies for the RS1 types (MAIN weights; same model as the main event-study figures)
rs_es <- pmap_dfr(expand_grid(RS_TYPES %>% filter(!type %in% c("rs_md_core0", "rs_ceo_vs_nonceo0")), MAIN_OUTCOMES %>% select(y, to)), function(type, label, y, to) {
  d <- A %>% filter(TaxYr <= to, !is.na(.data[[y]]), !is.na(.data[[type]])) %>% mutate(yv = .data[[y]], t = .data[[type]]) %>% balance_ids()
  w <- get_w(d, "t", covs_for("A")); if (is.null(w)) return(NULL)
  d <- d %>% inner_join(w, by = "id")
  m <- feols(yv ~ i(TaxYr, t, ref = REF_YEAR) + post_sys_change + mcaid_exp_win | id + TaxYr, data = d, weights = ~w, cluster = ~id)
  ct <- as.data.frame(coeftable(m))
  wt <- try(wald(m, keep = paste0("^TaxYr::(", paste(min(YEARS):(REF_YEAR - 1), collapse = "|"), "):t$"), print = FALSE), silent = TRUE)
  tibble(term = rownames(ct), est = ct$Estimate, se = ct$`Std. Error`) %>%
    mutate(year = as.integer(str_match(term, "^TaxYr::(\\d{4}):t$")[, 2])) %>% filter(!is.na(year)) %>% select(-term) %>%
    bind_rows(tibble(year = REF_YEAR, est = 0, se = 0)) %>%
    mutate(lo = est - 1.96 * se, hi = est + 1.96 * se, main_outcome = y, type = label, pretrend_p = if (inherits(wt, "try-error")) NA_real_ else wt$p)
})
write_csv(rs_es, paste0(tab_path, "es_role_split.csv"))
cat("\nPre-trend p-values, role split:\n"); print(rs_es %>% distinct(main_outcome, type, pretrend_p) %>% mutate(pretrend_p = round(pretrend_p, 3)), width = Inf)
for (y in unique(rs_es$main_outcome)) {
  prs <- ggplot(rs_es %>% filter(main_outcome == y) %>% mutate(type = factor(type, levels = RS_TYPES$label),
                                                               lab = paste0(type, "\npre-trend p = ", round(pretrend_p, 3))), aes(year, est)) +
    geom_hline(yintercept = 0, linetype = 2) + geom_vline(xintercept = REF_YEAR + 0.5, linetype = 3) +
    geom_pointrange(aes(ymin = lo, ymax = hi), size = 0.3) + facet_wrap(~ lab, ncol = 1) + scale_x_continuous(breaks = YEARS) +
    labs(title = paste("Physician executive split by role:", y), x = "Year the 36-month window ends (June)",
         y = "Type 1 minus type 0, relative to 2011 (pp)", caption = "Panel A. Entropy-balanced; hospital and year FE; system-change and Medicaid controls.") + theme_bw()
  ggsave(paste0(fig_path, "es_role_split_", y, ".pdf"), prs, width = 7, height = 11); print(prs)
}


# ---- MANAGING VS ORGANISATIONAL TYPE (item C; pre-specified 3 Nov, coded 6 Nov before results; DECISIONS.md) --------------------------
# Is the response tied to the clinical leader IN POST at the policy (managing), or to hospitals that have clinical leaders at some point
# (organisational type / selection)? Groups from mvt_group(): AT / BEFORE / AFTER vs NEVER, all in one model. Unweighted, baseline
# covariates x post, hospital + year FE, ACA and system-change controls, clustered by hospital (as RS2 / K1). MAIN (group x post) and
# E1 (group x post x exposure). Contrasts AT - BEFORE, AT - AFTER, BEFORE - AFTER.
# Readings (set 3 Nov): only AT responds -> the leader in post manages the response; BEFORE and AFTER like AT -> clinical leadership marks
# a clinically oriented organisation; BEFORE only -> imprinting / legacy; AFTER responding -> selection of hospitals that will choose
# clinicians (AFTER uses future information). Small groups expected: counts first.
cat("\n================ MANAGING VS TYPE: hospitals by group ================\n")
mvt_n <- pmap_dfr(MVT_TYPES, function(panel, gvar, label) (if (panel == "A") A else B) %>% distinct(id, .keep_all = TRUE) %>%
                    count(group = factor(.data[[gvar]], levels = MVT_LEVELS)) %>% mutate(panel = panel, type = label, .before = 1))
print(mvt_n %>% pivot_wider(names_from = group, values_from = n, values_fill = 0), width = Inf)
fit_mvt <- function(panel, gvar, label, y, to, e1, pooled = FALSE) {
  wc <- covs_for(panel); sp <- mvt_spec(pooled)
  dd <- (if (panel == "A") A else B) %>% filter(TaxYr <= to, !is.na(.data[[y]]), !is.na(.data[[gvar]]), if (e1) !is.na(exposure_sd) else TRUE) %>%
    mutate(yv = .data[[y]], pv = post, g = factor(if (pooled) unname(MVT_POOL[.data[[gvar]]]) else .data[[gvar]], levels = sp$lv),
           pvx = pv * coalesce(exposure_sd, 0)) %>% balance_ids()
  core <- paste0("i(g, pv, ref = 'NEVER')", if (e1) " + i(g, pvx, ref = 'NEVER') + exposure_sd:pv" else "")
  m <- try(feols(as.formula(paste0("yv ~ ", core, " + post_sys_change + mcaid_exp_win + pv:(", paste(wc, collapse = " + "), ") | id + TaxYr")),
                 data = dd, cluster = ~id), silent = TRUE)
  if (inherits(m, "try-error")) { message("fit_mvt failed: ", label, " | ", y); return(NULL) }
  b <- coef(m); V <- vcov(m); nm <- function(g) paste0("g::", g, ":", if (e1) "pvx" else "pv")
  rows <- map_dfr(sp$lv[-1], function(g) if (nm(g) %in% names(b)) tibble(term = paste(g, "vs NEVER"), est = b[[nm(g)]], se = sqrt(V[nm(g), nm(g)])))
  rows <- bind_rows(rows, map_dfr(sp$contr, function(cc) { k <- nm(cc)
  if (!all(k %in% names(b))) return(NULL)
  tibble(term = paste(cc[1], "minus", cc[2]), est = b[[k[1]]] - b[[k[2]]], se = sqrt(V[k[1], k[1]] + V[k[2], k[2]] - 2 * V[k[1], k[2]])) }))
  n_g <- dd %>% distinct(id, g) %>% count(g)
  rows %>% mutate(p = 2 * pnorm(-abs(est / se)), ci_lo = est - 1.96 * se, ci_hi = est + 1.96 * se,
                  grouping = if (pooled) "pooled: BY (AT + BEFORE) / AFTER" else "four groups (pre-specified)",
                  spec = if (e1) "E1 group x post x exposure" else "MAIN group x post",
                  n_by_group = paste(paste0(n_g$g, "=", n_g$n), collapse = ", "), N = nobs(m))
}
res_mvt <- expand_grid(MVT_TYPES, MAIN_OUTCOMES %>% select(y, to), e1 = c(FALSE, TRUE), pooled = c(FALSE, TRUE)) %>%
  pmap_dfr(function(panel, gvar, label, y, to, e1, pooled) fit_mvt(panel, gvar, label, y, to, e1, pooled) %>%
             { if (is.null(.)) NULL else mutate(., main_outcome = y, panel = panel, type = label, .before = 1) })
cat("\n================ MANAGING VS TYPE: group x post (vs NEVER) and contrasts ================\n")
print(res_mvt %>% mutate(across(c(est, se, ci_lo, ci_hi), ~ round(.x, 3)), p = round(p, 3)) %>%
        select(main_outcome, type, grouping, spec, term, est, se, p, n_by_group), width = Inf, n = 300)
write_csv(res_mvt, paste0(tab_path, "response_by_type_managing.csv"))


# ---- COMPETITION (item J; pre-specified 6 Nov before results; DECISIONS.md) ---------------------------------------------------------
# Does the type gap differ by baseline market concentration? HHI of admissions 2009-11 from all AHA general hospitals, same-system
# hospitals = one firm (data02). Primary market = HRR (HSAs are mostly single-hospital outside cities, little variation); HSA = robustness.
#   CJ1 split sample: MAIN and E1 within high-competition (HHI below the panel median) and low-competition markets, weights re-estimated
#       within each; difference high - low with se = sqrt(se_h^2 + se_l^2) (disjoint hospitals).
#   CJ2 continuous, one model: MAIN + pv:hhi + t:pv:hhi (key t:pv:hhi, per SD of HHI); E1 + all lower-order terms (key t:pv:exposure:hhi);
#       HHI added to the balancing covariates.
# Sign: readmission gaps are negative when type 1 improves more. High - low < 0, or t:pv:hhi > 0, = larger clinical-leader response in
# COMPETITIVE markets (fits the information / reputation channel: competition raises the return to visible quality); the reverse =
# larger where market discipline is weak. Descriptive heterogeneity: concentration goes with rural, smaller, independent hospitals.
# 6 Nov (user): HSA dropped; operating margin added as a second moderator (MG). For margin, the continuous term is per SD of the
# baseline margin RANK (margin_r, already a balancing covariate, so nothing is added to the weights); higher = healthier margin.
COMP <- tribble(~market, ~cvar, ~hvar, ~hraw, ~lab1, ~lab0, ~addbal,
                "HRR (primary)", "comp_hi_hrr0", "hhi_hrr_sd0", "hhi_hrr0", "high competition", "low competition", TRUE,
                "MG operating margin (2009-11)", "margin_lo0", "margin_rsd0", "margin_r", "low margin", "high margin", FALSE)
cat("\n================ COMPETITION: HHI by type (hospital level) ================\n")
print(pmap_dfr(TYPES %>% filter(label %in% FIG_LABELS), function(panel, type, label) (if (panel == "A") A else B) %>% distinct(id, .keep_all = TRUE) %>%
                 filter(!is.na(.data[[type]])) %>% group_by(t = .data[[type]]) %>%
                 summarise(hospitals = n(), hhi_hrr = mean(hhi_hrr0, na.rm = TRUE), hhi_hsa = mean(hhi_hsa0, na.rm = TRUE),
                           share_comp_hi_hrr = mean(comp_hi_hrr0, na.rm = TRUE), share_comp_hi_hsa = mean(comp_hi_hsa0, na.rm = TRUE),
                           missing_hhi = mean(is.na(hhi_hrr0)), .groups = "drop") %>% mutate(type = label, .before = 1)) %>%
        mutate(across(where(is.double), ~ round(.x, 3))), width = Inf)
E1_RHS <- " + exposure_sd:pv + t:exposure_sd:pv"
run_comp <- function(panel, type, label, y, to, market, cvar, hvar, hraw, lab1, lab0, addbal) {
  d <- (if (panel == "A") A else B) %>% filter(!is.na(.data[[hvar]])); wc <- covs_for(panel); de <- d %>% filter(!is.na(exposure_sd))
  split <- map_dfr(c(high = 1L, low = 0L), function(v) bind_rows(
    fit(d %>% filter(.data[[cvar]] == v), type, y, "MAIN", wcovs = wc, to = to),
    fit(de %>% filter(.data[[cvar]] == v), type, y, "E1", wcovs = wc, extra_rhs = E1_RHS, key = "t:pv:exposure_sd", to = to)) %>%
      mutate(comp = if (v == 1L) "high competition" else "low competition"), .id = NULL)   # internal labels; renamed below
  if (!nrow(split)) split <- tibble(spec = character(), comp = character(), est = double(), se = double())
  diff <- inner_join(split %>% filter(comp == "high competition") %>% select(spec, est_h = est, se_h = se),
                     split %>% filter(comp == "low competition") %>% select(spec, est_l = est, se_l = se), by = "spec") %>%
    transmute(spec, comp = "high minus low", est = est_h - est_l, se = sqrt(se_h^2 + se_l^2), p = 2 * pnorm(-abs(est / se)))   # both sides must exist
  wcc <- if (addbal) unique(c(wc, hraw)) else wc
  cont <- bind_rows(
    fit(d, type, y, "MAIN", wcovs = wcc, extra_rhs = paste0(" + pv:", hvar, " + t:pv:", hvar), key = paste0("t:pv:", hvar), to = to),
    fit(de, type, y, "E1", wcovs = wcc, key = paste0("t:pv:exposure_sd:", hvar), to = to,
        extra_rhs = paste0(E1_RHS, " + pv:", hvar, " + t:pv:", hvar, " + exposure_sd:pv:", hvar, " + t:pv:exposure_sd:", hvar))) %>%
    mutate(comp = if (str_detect(market, "^MG")) "continuous: per SD of margin rank (higher = healthier margin)" else "continuous: per SD of HHI (higher = more concentrated)")
  relab <- function(x) recode(x, "high competition" = lab1, "low competition" = lab0, "high minus low" = paste(lab1, "minus", lab0))
  bind_rows(split %>% mutate(design = "CJ1 split sample"), diff %>% mutate(design = "CJ1 split sample"), cont %>% mutate(design = "CJ2 continuous")) %>%
    mutate(comp = relab(comp)) %>%
    mutate(main_outcome = y, panel = panel, type = label, market = market, .before = 1)
}
res_comp <- expand_grid(TYPES, MAIN_OUTCOMES %>% select(y, to), COMP) %>%
  pmap_dfr(function(panel, type, label, y, to, market, cvar, hvar, hraw, lab1, lab0, addbal) run_comp(panel, type, label, y, to, market, cvar, hvar, hraw, lab1, lab0, addbal))
miss_c <- expand_grid(type = FIG_LABELS, main_outcome = MAIN_OUTCOMES$y, market = COMP$market, spec = c("MAIN", "E1")) %>%
  anti_join(res_comp %>% filter(design == "CJ2 continuous"), by = c("type", "main_outcome", "market", "spec"))
if (nrow(miss_c)) message("Competition: ", nrow(miss_c), " continuous-interaction rows missing (check coefficient names)")
cat("\n================ COMPETITION: type gap by market concentration ================\n")
print(res_comp %>% filter(type %in% FIG_LABELS) %>%
        transmute(main_outcome, market, type, design, spec, comp, est = round(est, 3), se = round(se, 3), p = round(p, 3), n_type1, n_type0),
      width = Inf, n = 300)
write_csv(res_comp, paste0(tab_path, "response_by_type_competition.csv"))


# ---- WHO CLOSES THE GAP? (6 Nov, pre-specified before results; DECISIONS.md) --------------------------------------------------------
# GC0 baseline profile by competition half (HRR): margins, size, teaching, independence, case mix, Medicare share - is the "competitive
#     markets have thinner margins" premise true here?
# GC1 each group's OWN exposure gradient within each half of each moderator (competition, margin), MAIN weights re-estimated within the
#     half: y ~ t:pv + pv:exposure + t:pv:exposure + controls | hospital + year. Non-clinical-led gradient = pv:exposure (reweighted to look
#     like the clinical-led hospitals); clinical-led gradient = pv:exposure + t:pv:exposure; gap = t:pv:exposure (= the E1 rows above).
#     Plus the change across halves of each group's gradient (se from both halves).
# Reading set in advance: the user's account (non-clinical leaders in competitive / low-margin settings do more to avoid the penalty)
# predicts a MORE negative non-clinical gradient in high-competition (low-margin) halves and a similar clinical-led gradient in both.
# A clinical-led gradient that is more negative in concentrated (high-margin) halves instead = clinical leaders drive the gap.
cat("\n================ GC0: baseline profile by competition half (HRR) ================\n")
gc0 <- map_dfr(c(A = "A", B = "B"), function(pn) (if (pn == "A") A else B) %>% distinct(id, .keep_all = TRUE) %>% filter(!is.na(comp_hi_hrr0)) %>%
                 group_by(competition = if_else(comp_hi_hrr0 == 1, "high competition", "low competition")) %>%
                 summarise(hospitals = n(), margin_median = median(margin0, na.rm = TRUE), margin_mean = mean(margin0, na.rm = TRUE), margin_rank = mean(margin_r),
                           low_margin_share = mean(margin_lo0, na.rm = TRUE), beds = exp(mean(lbeds0)), teaching = mean(teaching0), independent = mean(indep0),
                           cmi = mean(cmi0), medicare_rank = mean(mcare_r), exposure = mean(exposure, na.rm = TRUE), hhi = mean(hhi_hrr0), .groups = "drop") %>%
                 mutate(panel = pn, .before = 1))
print(gc0 %>% mutate(across(where(is.double), ~ round(.x, 3))), width = Inf)
write_csv(gc0, paste0(tab_path, "competition_profile.csv"))
fit_gr <- function(d, type, y, wcovs, to) {
  dd <- d %>% filter(TaxYr >= min(YEARS), TaxYr <= to, !is.na(.data[[y]]), !is.na(.data[[type]]), !is.na(exposure_sd)) %>%
    mutate(yv = .data[[y]], t = .data[[type]], pv = post) %>% balance_ids()
  w <- get_w(dd, "t", wcovs); if (is.null(w)) return(NULL); dd <- dd %>% inner_join(w, by = "id")
  m <- try(feols(yv ~ t:pv + exposure_sd:pv + t:exposure_sd:pv + post_sys_change + mcaid_exp_win | id + TaxYr, data = dd, weights = ~w, cluster = ~id), silent = TRUE)
  if (inherits(m, "try-error")) return(NULL)
  b <- coef(m); V <- vcov(m); k0 <- "pv:exposure_sd"; k1 <- "t:pv:exposure_sd"
  if (!all(c(k0, k1) %in% names(b))) { message("fit_gr: coefficient names not found: ", paste(names(b), collapse = ", ")); return(NULL) }
  tibble(group = c("non-clinical-led (type 0, reweighted)", "clinical-led (type 1)", "gap (type 1 - type 0)"),
         est = c(b[[k0]], b[[k0]] + b[[k1]], b[[k1]]),
         se = c(sqrt(V[k0, k0]), sqrt(V[k0, k0] + V[k1, k1] + 2 * V[k0, k1]), sqrt(V[k1, k1])),
         n_type1 = n_distinct(dd$id[dd$t == 1]), n_type0 = n_distinct(dd$id[dd$t == 0]), N = nobs(m))
}
gc1 <- expand_grid(TYPES %>% filter(label %in% FIG_LABELS | str_detect(label, "990 measure")), MAIN_OUTCOMES %>% select(y, to), COMP) %>%
  pmap_dfr(function(panel, type, label, y, to, market, cvar, hvar, hraw, lab1, lab0, addbal) {
    d <- (if (panel == "A") A else B) %>% filter(!is.na(.data[[cvar]]))
    map_dfr(c(1L, 0L), function(v) fit_gr(d %>% filter(.data[[cvar]] == v), type, y, covs_for(panel), to) %>%
              { if (is.null(.)) NULL else mutate(., half = if (v == 1L) lab1 else lab0) }) %>%
      mutate(main_outcome = y, panel = panel, type = label, moderator = market, .before = 1)
  })
gc1 <- gc1 %>% mutate(p = 2 * pnorm(-abs(est / se)))
gc1_diff <- gc1 %>% group_by(main_outcome, panel, type, moderator, group) %>% filter(n() == 2) %>%
  summarise(half = paste(half[1], "minus", half[2]), est = est[1] - est[2], se = sqrt(se[1]^2 + se[2]^2), .groups = "drop") %>%
  mutate(p = 2 * pnorm(-abs(est / se)))
gc1 <- bind_rows(gc1, gc1_diff)
cat("\n================ GC1: each group's own exposure gradient (after vs before, per SD of exposure) by half ================\n")
print(gc1 %>% filter(type %in% FIG_LABELS) %>% transmute(main_outcome, moderator, type, half, group, est = round(est, 3), se = round(se, 3), p = round(p, 3)) %>%
        arrange(main_outcome, moderator, type, group, half), width = Inf, n = 300)
write_csv(gc1, paste0(tab_path, "competition_who_closes_gap.csv"))

# ---- BRIDGE: rebuild the 990-measure result from the combined measure (3 Nov, DECISIONS.md) ----------------------------------------
# Panel A, hospitals typed by BOTH measures (common sample). Start from the combined types; switch the hospitals whose type differs back
# to their 990 type, one group of disagreements at a time (cumulative path, and each group alone). The last step equals the 990 measure on
# the common sample. Then: the change in the estimate when each re-typed hospital alone is switched back (all_read).
BR_CATS <- c("A same CEO named, status differs (credential / hand-check / nurse)", "B name variant (same person, name link missed)",
             "C timing (990 CEO is the HIMSS CEO 1-2 years later / earlier)", "D 990 CEO is the HIMSS system CEO",
             "E 990 CEO elsewhere in HIMSS (other office, other entity)", "F unexplained", "G no usable 990 CEO name")
br_d <- A %>% mutate(br_cat = case_when(ceo_same0 %in% 1 ~ BR_CATS[1], is.na(ceo_same0) ~ BR_CATS[7], d_is_sys0 %in% 1 ~ BR_CATS[4],
                                        str_detect(coalesce(d_class0, ""), "name_variant") ~ BR_CATS[2],
                                        str_detect(coalesce(d_class0, ""), "himss_lags|himss_leads") ~ BR_CATS[3],
                                        str_detect(coalesce(d_class0, ""), "other_role|in_990_other|system_level|reports_to") ~ BR_CATS[5],
                                        TRUE ~ BR_CATS[6]))
br_fit2 <- function(d, y, to, spec) list(
  fit(d, "tb", y, paste0(spec, " | MAIN"), wcovs = covs_for("A"), to = to),
  fit(d %>% filter(!is.na(exposure_sd)), "tb", y, paste0(spec, " | E1 triple"), wcovs = covs_for("A"), to = to,
      extra_rhs = " + exposure_sd:pv + t:exposure_sd:pv", key = "t:pv:exposure_sd"))
br_path <- list(); br_hosp <- list()
for (r in seq_len(nrow(ROLES))) {
  t990 <- ROLES$t990[r]; tcomb <- ROLES$tcomb[r]; role <- ROLES$role[r]
  bd <- br_d %>% filter(!is.na(.data[[t990]]), !is.na(.data[[tcomb]])) %>% mutate(differs = .data[[t990]] != .data[[tcomb]])
  cat("\n================ BRIDGE,", role, ": hospitals whose type differs, by reason ================\n")
  print(bd %>% distinct(id, .keep_all = TRUE) %>% filter(differs) %>% count(br_cat, m990 = .data[[t990]], combined = .data[[tcomb]]), n = 30)
  for (i in seq_len(nrow(MAIN_OUTCOMES))) {
    y <- MAIN_OUTCOMES$y[i]; to <- MAIN_OUTCOMES$to[i]
    steps <- list(fit_rows = br_fit2(bd %>% mutate(tb = .data[[tcomb]]), y, to, "0 combined measure (common sample)"))
    for (k in seq_along(BR_CATS)) {
      steps[[paste0("cum", k)]] <- br_fit2(bd %>% mutate(tb = if_else(differs & br_cat %in% BR_CATS[1:k], .data[[t990]], .data[[tcomb]])), y, to,
                                           paste0("cumulative + ", substr(BR_CATS[k], 1, 1)))
      steps[[paste0("one", k)]] <- br_fit2(bd %>% mutate(tb = if_else(differs & br_cat == BR_CATS[k], .data[[t990]], .data[[tcomb]])), y, to,
                                           paste0("only ", BR_CATS[k]))
    }
    steps$end <- br_fit2(bd %>% mutate(tb = .data[[t990]]), y, to, "Z 990 measure (common sample)")
    br_path[[length(br_path) + 1]] <- bind_rows(unlist(steps, recursive = FALSE)) %>% mutate(main_outcome = y, role = role, .before = 1)
    if (y == "all_read") {                        # each re-typed hospital alone
      base <- bind_rows(steps$fit_rows)
      ids <- bd %>% filter(differs) %>% distinct(id, .keep_all = TRUE)
      br_hosp[[length(br_hosp) + 1]] <- map_dfr(seq_len(nrow(ids)), function(j) {
        fr <- bind_rows(br_fit2(bd %>% mutate(tb = if_else(id == ids$id[j], .data[[t990]], .data[[tcomb]])), y, to, "one"))
        if (!nrow(fr)) return(NULL)
        tibble(role = role, id = ids$id[j], ccn = ids$ccn0[j], state = ids$st[j], system = ids$s_name0[j], reason = ids$br_cat[j],
               type_990 = ids[[t990]][j], type_combined = ids[[tcomb]][j],
               d_main = fr$est[1] - base$est[1], d_e1 = if (nrow(fr) > 1 && nrow(base) > 1) fr$est[2] - base$est[2] else NA_real_)
      })
    }
  }
}
br_path <- bind_rows(br_path); br_hosp <- bind_rows(br_hosp)
cat("\n================ BRIDGE: estimate as groups of disagreements are switched back to the 990 type ================\n")
print(br_path %>% transmute(main_outcome, role, spec, est = round(est, 3), p = round(p, 3), n_type1), width = Inf, n = 200)
cat("\n================ BRIDGE: change in the estimate from switching ONE hospital back (all_read; largest first) ================\n")
print(br_hosp %>% arrange(role, d_main) %>% mutate(across(c(d_main, d_e1), ~ round(.x, 3))), width = Inf, n = 100)
write_csv(br_path, paste0(tab_path, "response_by_type_bridge.csv"))
write_csv(br_hosp, paste0(tab_path, "response_by_type_bridge_hospitals.csv"))

# ---- how much of the raw type gap is hospital composition? (3 Nov) --------------------------------------------------
# R0 raw (no weights, no covariates) -> C0 (weights on size, teaching, independence, margin, Medicare share) -> MAIN (+ baseline case mix).
# Sequential and descriptive: the shares depend on the order in which covariates are added.
decomp <- res %>% filter(spec %in% c("R0 raw: unweighted, no covariates", "C0 weights without baseline case mix", "MAIN")) %>%
  mutate(step = recode(spec, "R0 raw: unweighted, no covariates" = "raw", "C0 weights without baseline case mix" = "other_covs", "MAIN" = "plus_case_mix")) %>%
  select(main_outcome, type, step, est) %>% pivot_wider(names_from = step, values_from = est) %>%
  mutate(change_other_covs = other_covs - raw, change_case_mix = plus_case_mix - other_covs)
cat("\n================ Type gap: raw -> other covariates -> + baseline case mix ================\n")
print(decomp %>% mutate(across(where(is.numeric), ~ round(.x, 3))), width = Inf, n = 50)
write_csv(decomp, paste0(tab_path, "response_by_type_decomposition.csv"))

# ---- descriptive: leadership and baseline case mix (3 Nov) -------------------------------------------------------------
# (a) share of hospitals of each leadership type, by quartile of baseline case mix (each type's own panel)
cq <- pmap_dfr(TYPES, function(panel, type, label) {
  (if (panel == "A") A else B) %>% distinct(id, .keep_all = TRUE) %>% mutate(t = .data[[type]], q = ntile(cmi0, 4)) %>% filter(!is.na(t)) %>%
    group_by(q) %>% summarise(share_type1 = mean(t), n = n(), .groups = "drop") %>% mutate(type = label)
})
cat("\n================ Share of type-1 hospitals by baseline case-mix quartile ================\n")
print(cq %>% select(type, q, share_type1) %>% mutate(share_type1 = round(share_type1, 3)) %>% pivot_wider(names_from = q, names_prefix = "Q", values_from = share_type1), width = Inf)
pq1 <- ggplot(cq %>% filter(type %in% FIG_LABELS), aes(factor(q), share_type1)) + geom_col() + facet_wrap(~ type, ncol = 2, scales = "free_y") +
  labs(x = "Baseline case mix quartile (1 = lowest)", y = "Share of hospitals that are type 1") + theme_bw()
ggsave(paste0(fig_path, "casemix_leadership_share.pdf"), pq1, width = 8, height = 9); print(pq1)
# (b) raw (unweighted) mean of each main outcome by baseline case-mix quartile, all hospitals (Panel B)
qB <- B %>% distinct(id, cmi0) %>% mutate(q = ntile(cmi0, 4)) %>% select(id, q)          # quartiles across hospitals, not rows
rq <- map_dfr(seq_len(nrow(MAIN_OUTCOMES)), function(i) {
  y <- MAIN_OUTCOMES$y[i]
  B %>% filter(TaxYr <= MAIN_OUTCOMES$to[i], !is.na(.data[[y]])) %>% inner_join(qB, by = "id") %>% group_by(TaxYr, q) %>% summarise(mean_y = mean(.data[[y]]), n = n(), .groups = "drop") %>% mutate(outcome = MAIN_OUTCOMES$ylab[i])
})
pq2 <- ggplot(rq, aes(TaxYr, mean_y, colour = factor(q))) + geom_vline(xintercept = REF_YEAR + 0.5, linetype = 3, colour = "grey40") +
  geom_line() + geom_point() + facet_wrap(~ outcome, scales = "free_y", ncol = 1) +
  labs(x = "Year the 36-month window ends (June)", y = "Unweighted mean", colour = "Case mix quartile") + theme_bw()
ggsave(paste0(fig_path, "casemix_outcome_paths.pdf"), pq2, width = 8, height = 7); print(pq2)

# ---- event study + weighted levels (main specification), one figure per main outcome -----------------------------------
for (i in seq_len(nrow(MAIN_OUTCOMES))) {
  y <- MAIN_OUTCOMES$y[i]; to <- MAIN_OUTCOMES$to[i]; ylab <- MAIN_OUTCOMES$ylab[i]
  es_lv <- pmap(TYPES, function(panel, type, label) {
    d  <- (if (panel == "A") A else B) %>% filter(TaxYr <= to, !is.na(.data[[y]]), !is.na(.data[[type]])) %>% mutate(yv = .data[[y]], t = .data[[type]]) %>% balance_ids()
    w  <- get_w(d, "t", covs_for(panel)); if (is.null(w)) return(NULL)
    d  <- d %>% inner_join(w, by = "id")
    m  <- feols(yv ~ i(TaxYr, t, ref = REF_YEAR) + post_sys_change + mcaid_exp_win | id + TaxYr, data = d, weights = ~w, cluster = ~id)
    ct <- as.data.frame(coeftable(m)); ci <- as.data.frame(confint(m))
    pre <- setdiff(min(YEARS):(REF_YEAR - 1), REF_YEAR)
    wt <- try(wald(m, keep = paste0("^TaxYr::(", paste(pre, collapse = "|"), "):t$"), print = FALSE), silent = TRUE)
    es <- tibble(term = rownames(ct), est = ct$Estimate, lo = ci[[1]], hi = ci[[2]]) %>%
      mutate(year = as.integer(str_match(term, "^TaxYr::(\\d{4}):t$")[, 2])) %>% filter(!is.na(year)) %>% select(-term) %>%
      bind_rows(tibble(year = REF_YEAR, est = 0, lo = 0, hi = 0)) %>% mutate(type = label, pretrend_p = if (inherits(wt, "try-error")) NA_real_ else wt$p)
    lv <- d %>% group_by(TaxYr, t) %>% summarise(mean_y = weighted.mean(yv, w), .groups = "drop") %>% mutate(type = label)
    list(es = es, lv = lv)
  })
  es <- map_dfr(es_lv, "es"); lv <- map_dfr(es_lv, "lv")
  cat("\nPre-trend tests,", y, "(windows ending", min(YEARS), "-", REF_YEAR - 1, "= 0):\n"); print(es %>% distinct(type, pretrend_p))
  write_csv(es, paste0(tab_path, "es_response_by_type_", y, ".csv"))
  p1 <- ggplot(es %>% filter(type %in% FIG_LABELS), aes(year, est)) + geom_hline(yintercept = 0, linetype = 2) + geom_vline(xintercept = REF_YEAR + 0.5, linetype = 3) +
    geom_pointrange(aes(ymin = lo, ymax = hi), size = 0.3) + facet_wrap(~ type, ncol = 1, scales = "free_y") +
    scale_x_continuous(breaks = min(YEARS):to) +
    labs(x = "Year the 36-month window ends (June)", y = paste0(ylab, ": type 1 minus type 0, relative to ", REF_YEAR),
         caption = "Entropy-balanced on baseline size, teaching, independence, margin, Medicare share, case mix; hospital and year FE; controls for system change and Medicaid expansion.") + theme_bw()
  ggsave(paste0(fig_path, "es_response_by_type_", y, ".pdf"), p1, width = 8, height = 8); print(p1)
  p2 <- ggplot(lv %>% filter(type %in% FIG_LABELS) %>% mutate(group = factor(t, levels = c(0, 1), labels = c("Type 0 (reweighted)", "Type 1"))), aes(TaxYr, mean_y, colour = group)) +
    geom_vline(xintercept = REF_YEAR + 0.5, linetype = 3, colour = "grey40") + geom_line() + geom_point() +
    facet_wrap(~ type, ncol = 1, scales = "free_y") + scale_x_continuous(breaks = min(YEARS):to) +
    labs(x = "Year the 36-month window ends (June)", y = paste0(ylab, " (weighted mean)"), colour = NULL) + theme_bw()
  ggsave(paste0(fig_path, "levels_response_by_type_", y, ".pdf"), p2, width = 8, height = 8); print(p2)
}

# ==== E3: for-profit vs clinical-led nonprofit vs non-clinical-led nonprofit (added 2 Nov, before results) ===================
# Groups are fixed per hospital: ownership (modal) and, for nonprofits, leadership type in 2009 (else 2010). For-profits are NOT split by type
# (only 17 have a physician CEO). Reference group = non-clinical nonprofits. Weights: multi-group entropy balancing to the pooled sample's
# covariate means (ATE), so all three groups are compared at the same baseline size / teaching / independence / finances / case mix.
# Contrasts are descriptive: ownership differs in many ways besides leadership.  Run for each main outcome.
CLIN_VAR <- "has_md0"     # what "clinical-led" means for nonprofits: "has_md0" (physician executive, 990), "ceo_md0" (physician CEO, 990), "h_ceo_md0" (physician CEO, HIMSS),
# "ceo_clinical0" (physician or nurse CEO, 990), "h_ceo_clinical0" (physician or nurse CEO, HIMSS)
# "c_ceo_md0" / "c_ceo_clinical0" (combined measure, 3 Nov). Kept at has_md0 (team measure, unaffected by CEO identity).
G_LEVELS <- c("Nonprofit, non-clinical", "Nonprofit, clinical", "For-profit")
COVS3 <- c(COVS, EXTRA_MATCH)

e3_fit <- function(d, ref, weighted, label) {
  dd <- d %>% mutate(g = relevel(g3, ref = ref), w = if (weighted) w else 1)
  rhs <- paste0("i(g, post, ref = '", ref, "') + post_sys_change + mcaid_exp_win", if (!weighted) paste0(" + post:(", paste(COVS, collapse = " + "), ")") else "")
  m  <- feols(as.formula(paste0("yv ~ ", rhs, " | id + TaxYr")), data = dd, weights = ~w, cluster = ~id)
  ct <- as.data.frame(coeftable(m)); ct <- ct[str_detect(rownames(ct), "^g::"), , drop = FALSE]
  pre <- dd %>% filter(TaxYr < 2012); sdp <- sd(pre$yv)
  tibble(spec = label, contrast = paste(str_match(rownames(ct), "^g::(.*):post$")[, 2], "minus", ref),
         est = ct$Estimate, se = ct$`Std. Error`, p = ct$`Pr(>|t|)`, ci_lo = est - 1.96 * se, ci_hi = est + 1.96 * se,
         ci_sd = sprintf("[%.2f, %.2f]", ci_lo / sdp, ci_hi / sdp), N = nobs(m))
}
e3_all <- list()
for (i in seq_len(nrow(MAIN_OUTCOMES))) {
  y <- MAIN_OUTCOMES$y[i]; to <- MAIN_OUTCOMES$to[i]; ylab <- MAIN_OUTCOMES$ylab[i]
  d3 <- B %>% filter(TaxYr <= to, !is.na(.data[[y]])) %>%
    mutate(yv = .data[[y]], cl = .data[[CLIN_VAR]],
           g3 = case_when(fp == 1 ~ "For-profit", cl == 1 ~ "Nonprofit, clinical", cl == 0 ~ "Nonprofit, non-clinical", TRUE ~ NA_character_)) %>%
    filter(!is.na(g3)) %>% mutate(g3 = factor(g3, levels = G_LEVELS), clin_np = as.integer(g3 == "Nonprofit, clinical"), fpd = as.integer(g3 == "For-profit")) %>%
    balance_ids()
  cat("\nE3 groups (", CLIN_VAR, ",", y, "):\n"); print(d3 %>% distinct(id, g3) %>% count(g3))
  h3 <- d3 %>% distinct(id, .keep_all = TRUE) %>% select(id, g3, all_of(COVS3)) %>% filter(if_all(all_of(COVS3), ~ !is.na(.x)))
  d3 <- d3 %>% semi_join(h3, by = "id")
  W3 <- weightit(as.formula(paste("g3 ~", paste(COVS3, collapse = " + "))), data = h3, method = "ebal", estimand = "ATE")
  print(summary(W3)$effective.sample.size)
  d3 <- d3 %>% inner_join(tibble(id = h3$id, w = W3$weights), by = "id")
  
  e3 <- bind_rows(e3_fit(d3, G_LEVELS[1], TRUE,  "E3 weighted (ATE)"),
                  e3_fit(d3, G_LEVELS[2], TRUE,  "E3 weighted (ATE)") %>% filter(str_detect(contrast, "^For-profit")),
                  e3_fit(d3, G_LEVELS[1], FALSE, "E3 unweighted, covariates x post"),
                  e3_fit(d3, G_LEVELS[2], FALSE, "E3 unweighted, covariates x post") %>% filter(str_detect(contrast, "^For-profit"))) %>%
    mutate(main_outcome = y, .before = 1)
  cat("\n================ E3: three-group contrasts (", y, ") ================\n")
  print(e3 %>% mutate(across(c(est, se, ci_lo, ci_hi), ~ round(.x, 3)), p = round(p, 3)), width = Inf)
  e3_all[[y]] <- e3
  
  # event study (each group vs non-clinical nonprofits, relative to 2011) and weighted levels
  m3 <- feols(yv ~ i(TaxYr, clin_np, ref = REF_YEAR) + i(TaxYr, fpd, ref = REF_YEAR) + post_sys_change + mcaid_exp_win | id + TaxYr, data = d3, weights = ~w, cluster = ~id)
  ct <- as.data.frame(coeftable(m3)); ci <- as.data.frame(confint(m3))
  es3 <- tibble(term = rownames(ct), est = ct$Estimate, lo = ci[[1]], hi = ci[[2]]) %>%
    mutate(year = as.integer(str_match(term, "^TaxYr::(\\d{4}):")[, 2]),
           contrast = case_when(str_detect(term, ":clin_np$") ~ "Nonprofit, clinical minus non-clinical", str_detect(term, ":fpd$") ~ "For-profit minus nonprofit non-clinical")) %>%
    filter(!is.na(year), !is.na(contrast)) %>% select(-term) %>%
    bind_rows(tibble(year = REF_YEAR, est = 0, lo = 0, hi = 0, contrast = c("Nonprofit, clinical minus non-clinical", "For-profit minus nonprofit non-clinical")))
  p3 <- ggplot(es3, aes(year, est)) + geom_hline(yintercept = 0, linetype = 2) + geom_vline(xintercept = REF_YEAR + 0.5, linetype = 3) +
    geom_pointrange(aes(ymin = lo, ymax = hi), size = 0.3) + facet_wrap(~ contrast, ncol = 1, scales = "free_y") + scale_x_continuous(breaks = min(YEARS):to) +
    labs(x = "Year the 36-month window ends (June)", y = paste0(ylab, ": difference relative to ", REF_YEAR), caption = paste("Nonprofit split by", CLIN_VAR, "; ATE entropy-balancing weights.")) + theme_bw()
  ggsave(paste0(fig_path, "es_three_groups_", y, ".pdf"), p3, width = 8, height = 7); print(p3)
  lv3 <- d3 %>% group_by(TaxYr, g3) %>% summarise(mean_y = weighted.mean(yv, w), .groups = "drop")
  p4 <- ggplot(lv3, aes(TaxYr, mean_y, colour = g3)) + geom_vline(xintercept = REF_YEAR + 0.5, linetype = 3, colour = "grey40") + geom_line() + geom_point() +
    scale_x_continuous(breaks = min(YEARS):to) + labs(x = "Year the 36-month window ends (June)", y = paste0(ylab, " (weighted mean)"), colour = NULL) + theme_bw()
  ggsave(paste0(fig_path, "levels_three_groups_", y, ".pdf"), p4, width = 8, height = 5); print(p4)
}
write_csv(bind_rows(e3_all), paste0(tab_path, "response_three_groups.csv"))