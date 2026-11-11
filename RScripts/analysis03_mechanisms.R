library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(purrr)
library(tibble)
library(ggplot2)
library(fixest)
library(WeightIt)

source("./RScripts/paths.R")

### Mechanisms: did hospitals with different (pre-policy) leadership types change OTHER outcomes differently after HRRP?
### Same design as analysis02 MAIN for every outcome:  y_it = b (type_i x post_t) + post_sys_change + hospital FE + year FE,
### entropy-balancing weights (type 0 reweighted to type 1 on baseline size, teaching, independence, margin, Medicare share, case mix; + ownership in Panel B),
### SEs clustered by hospital.  post = year >= 2012 (window/period end year; fiscal year for HCRIS/AHA).
### Outcomes that are not in hospital_data are skipped with a message (e.g. CMI, non-targeted conditions before the data00 patch).
### ACA Medicaid expansion (state-level, 2014+; built in data02_aca_block.R): MAIN adds a time-varying control - mcaid_exp_win (share of the
###   36-month window after the state's expansion) for Hospital Compare outcomes, mcaid_exp (state expanded by that year) for annual outcomes;
###   robustness STYR replaces year FE with state x year FE (absorbs expansion and any other state-level shock). Both use the same sample.
### Multiple testing: Benjamini-Hochberg q-values WITHIN FAMILY = spec x leadership type x outcome group.
### HRRP / VBP payment factors (IPPS impact files) are their own family: 1 before FY2013, so their estimate is a post-period level gap.
### Pre-trends: an event study (relative to 2011) for every outcome, MAIN controls -> mechanisms_event_study.csv / .pdf and pretrend_p_main.
### Not run when written (no R in the sandbox).

# ---- settings ---------------------------------------------------------------------------------------------------
YEARS      <- 2009:2017      # as in analysis02 (2008 window removed 6 Nov)
TYPE_YEARS <- c(2010, 2009)        # type measured in 2010, else 2009 (3 Nov; was 2009 then 2010)
COVS       <- c("lbeds0", "teaching0", "indep0", "margin_r", "mcare_r", "cmi0")   # cmi0 added 2 Nov (baseline case mix index, 2009-2011), as in analysis02
HCAHPS_VARS <- NULL      # NULL = all numeric columns from hcahps_data.rds (printed); or set the ones you want, e.g. c("recommend_hospital")
BALANCED    <- FALSE     # TRUE = per outcome, keep only hospitals observed in every year of that outcome's window (as in analysis02)
balance_ids <- function(dd) { if (!BALANCED) return(dd); yrs <- n_distinct(dd$TaxYr)
dd %>% group_by(id) %>% filter(n_distinct(TaxYr) == yrs) %>% ungroup() }
fig_path <- "./Objects/"; tab_path <- "./Tables/"; dir.create(fig_path, showWarnings = FALSE); dir.create(tab_path, showWarnings = FALSE)
setFixest_notes(FALSE)
first_nn <- function(x) { x <- x[!is.na(x)]; if (length(x)) x[[1]] else NA }
# physician executive split by role (5 Nov, item I) - same groups as analysis02; x / y are counted, not estimated
RS_GRP <- c("0 no physician executive", "1 physician CEO", "2 physician exec (not CEO), CMO in place", "3 physician exec (not CEO), no CMO",
            "x physician CEO, no 990 physician executive", "y physician executive, CEO or CMO status unknown")
role_split <- function(has_md, ceo_md, cmo) case_when(
  has_md %in% 0 & !ceo_md %in% 1 ~ RS_GRP[1], has_md %in% 1 & ceo_md %in% 1 ~ RS_GRP[2],
  has_md %in% 1 & ceo_md %in% 0 & cmo %in% 1 ~ RS_GRP[3], has_md %in% 1 & ceo_md %in% 0 & cmo %in% 0 ~ RS_GRP[4],
  has_md %in% 0 & ceo_md %in% 1 ~ RS_GRP[5], has_md %in% 1 ~ RS_GRP[6], TRUE ~ NA_character_)
wins <-function(x, p = c(0.01, 0.99)) { q <- quantile(x, p, na.rm = TRUE); pmin(pmax(x, q[1]), q[2]) }

hd <- read_rds(paste0(created_data_path, "/hospital_data.rds"))
has <- function(v) v %in% names(hd)

# ---- state and ACA Medicaid expansion: built in data02 (data02_aca_block.R) -------------------------------------------
if (!all(c("state", "mcaid_exp", "mcaid_exp_win", "mcaid_exp_date") %in% names(hd))) stop("Add data02_aca_block.R to data02 and rebuild hospital_data first")
hd <- hd %>% mutate(st = state, exp2014_state = as.integer(!is.na(mcaid_exp_date) & mcaid_exp_date < as.Date("2015-01-01")))
WINDOW_GROUPS <- "^Readmissions|^Mortality"   # Hospital Compare window outcomes use mcaid_exp_win (share of window after expansion); others mcaid_exp

# ---- outcome registry -------------------------------------------------------------------------------------------
hd <- hd %>% mutate(
  mcare_share = if (has("mcare_discharges")) mcare_discharges / tot_discharges else NA_real_,
  mcaid_share = if (has("mcaid_discharges")) mcaid_discharges / tot_discharges else NA_real_,
  los         = if (has("IPDTOT") && has("ADMTOT")) IPDTOT / ADMTOT else NA_real_,
  pn_hf_cohort_ratio = if (has("pneumonia_readmission_n") && has("hf_readmission_n"))
    ifelse(pneumonia_readmission_n > 0 & hf_readmission_n > 0, log(pneumonia_readmission_n) - log(hf_readmission_n), NA_real_) else NA_real_)
# HCAHPS: by default every numeric column that hospital_data got from hcahps_data.rds (data05); one family. HCAHPS is also a Hospital VBP
# domain (patient experience), like mortality.
hc_file <- paste0(created_data_path, "/hcahps_data.rds")
hc_vars <- if (!is.null(HCAHPS_VARS)) HCAHPS_VARS else if (file.exists(hc_file)) {
  nm <- setdiff(names(read_rds(hc_file)), c("provider_id", "year")); nm[nm %in% names(hd)][map_lgl(hd[nm[nm %in% names(hd)]], is.numeric)]
} else character(0)
cat("HCAHPS columns used:", if (length(hc_vars)) paste(hc_vars, collapse = ", ") else "none found", "\n")

# COPD, stroke, hip/knee, CABG and hospital-wide measures start with windows ending 2012-2014 (no pre-2011 window): they are analysed
# around the 2013 / 2014 additions in analysis04_added_conditions.R, not here.
reg <- tribble(
  ~group,                                    ~label,                               ~var,                   ~tf,     ~to,
  "Readmissions by HRRP condition",          "HF readmission (to 2014)",           "hf_readmission",       "none",  2014,
  "Readmissions by HRRP condition",          "HF readmission (to 2017)",           "hf_readmission",       "none",  2017,
  "Readmissions by HRRP condition",          "Pneumonia readmission (to 2014)",    "pneumonia_readmission", "none", 2014,
  "Readmissions by HRRP condition",          "AMI readmission",                    "ami_readmission",      "none",  2017,
  # coding (3 Nov, before results): if pneumonia patients were recoded (e.g. to sepsis / respiratory failure as principal diagnosis), the
  # measured pneumonia cohort shrinks - absolutely, and relative to HF. Hospital Compare 36-month cohort sizes; to 2014 (cohort widened after).
  "Readmissions: cohort size (coding)",      "log pneumonia readmission cohort",   "pneumonia_readmission_n", "log", 2014,
  "Readmissions: cohort size (coding)",      "log HF readmission cohort",          "hf_readmission_n",     "log",   2014,
  "Readmissions: cohort size (coding)",      "log pneumonia minus log HF cohort",  "pn_hf_cohort_ratio",   "none",  2014,
  "Mortality",                               "HF mortality",                       "hf_mortality",         "none",  2017,
  "Mortality",                               "AMI mortality",                      "ami_mortality",        "none",  2017,
  "Mortality",                               "Pneumonia mortality (to 2014)",      "pneumonia_mortality",  "none",  2014,
  "Finances (HCRIS)",                        "Operating margin (winsorised)",      "oper_margin",          "wins",  2017,
  "Finances (HCRIS)",                        "log operating expenses",             "tot_operating_exp",    "log",   2017,
  "Finances (HCRIS)",                        "log patient revenue",                "tot_pat_rev",          "log",   2017,
  "Finances (HCRIS)",                        "log labour costs",                   "labor_costs",          "log",   2017,
  "Finances (HCRIS)",                        "log equipment purchases",            "movableequipment_purch", "log1p", 2017,
  "Volume and patient mix",                  "log total discharges",               "tot_discharges",       "log",   2017,
  "Volume and patient mix",                  "Medicare share of discharges",       "mcare_share",          "wins",  2017,
  "Volume and patient mix",                  "Medicaid share of discharges",       "mcaid_share",          "wins",  2017,
  "Volume and patient mix",                  "log admissions (AHA)",               "ADMTOT",               "log",   2017,
  "Volume and patient mix",                  "Length of stay (AHA)",               "los",                  "wins",  2017,
  "Volume and patient mix",                  "log ED visits (AHA)",                "VEM",                  "log1p", 2017,
  "Staffing (AHA)",                          "log FT physicians & dentists",       "FTMDTF",               "log1p", 2017,
  "Staffing (AHA)",                          "log FT residents",                   "FTRES",                "log1p", 2017,
  "Case mix (IPPS impact files)",            "Case mix index (transfer-adjusted)", "cmi",                  "none",  2017,
  "Case mix (IPPS impact files)",            "Case mix index, same-grouper chain", "cmi_chain",            "none",  2017,
  "Case mix (IPPS impact files)",            "log Medicare cases (impact file)",   "cmi_cases",            "log",   2017,
  # care processes (5 Nov, pre-specified): Hospital Compare 12-month windows keyed to the window end year, releases 2008-2014 (data06)
  "Care processes (Hospital Compare)",       "HF discharge instructions (HF-1)",   "hf1",                  "none",  2014,
  "Care processes (Hospital Compare)",       "HF-1, hospitals below 98% at baseline", "hf1_noceil",        "none",  2014,
  "Care processes (Hospital Compare)",       "HF LVS function evaluated (HF-2)",   "hf2",                  "none",  2014,
  "Care processes (Hospital Compare)",       "HF ACE inhibitor / ARB (HF-3)",      "hf3",                  "none",  2014,
  "Care processes (Hospital Compare)",       "HF-1 minus HF-2 (within hospital)",  "hf1_minus_hf2",        "none",  2014,
  "Care processes (Hospital Compare)",       "HF-1 minus HF-3 (within hospital)",  "hf1_minus_hf3",        "none",  2014) %>%   # HRRP / VBP factors dropped 3 Nov
  bind_rows(tibble(group = "Patient experience (HCAHPS)", label = hc_vars, var = hc_vars, tf = "none", to = 2017))

reg <- reg %>% mutate(found = map_lgl(var, has))
cat("\nOutcomes NOT in hospital_data (skipped):", paste(reg$label[!reg$found], collapse = "; "), "\n")
reg <- reg %>% filter(found) %>% mutate(col = paste0("m_", row_number()))
for (i in seq_len(nrow(reg))) {
  x <- suppressWarnings(as.numeric(hd[[reg$var[i]]]))
  hd[[reg$col[i]]] <- switch(reg$tf[i], none = x, wins = wins(x), log = ifelse(x > 0, log(x), NA_real_), log1p = ifelse(x >= 0, log1p(x), NA_real_))
}

# ---- panels and types (as in analysis02) ------------------------------------------------------------------------
build <- function(df) {
  df <- df %>% filter(TaxYr %in% c(2008:max(YEARS))) %>%
    mutate(has_md = as.integer(n_md_exec > 0), post = as.integer(TaxYr >= 2012), post_sys_change = coalesce(post_sys_change, 0L))
  b <- df %>% group_by(id) %>% summarise(
    hrrp = any(hrrp_pre_measured %in% TRUE), general0 = any(general[TaxYr %in% 2009:2011] %in% 1),
    lbeds0 = { v <- mean(bed_num[TaxYr %in% 2009:2011], na.rm = TRUE); if (is.finite(v) && v > 0) log(v) else NA_real_ },
    teaching0 = { x <- as.numeric(teaching[TaxYr %in% 2009:2011]); x <- x[!is.na(x)]; if (length(x)) max(x) else NA_real_ },
    indep0 = { x <- as.numeric(independent[TaxYr %in% 2009:2011]); x <- x[!is.na(x)]; if (length(x)) max(x) else NA_real_ },
    margin0 = { x <- oper_margin[which(TaxYr %in% 2009:2011 & is.finite(oper_margin))]; if (length(x)) mean(x) else NA_real_ },
    cmi0 = { x <- cmi[TaxYr %in% 2009:2011]; x <- x[!is.na(x)]; if (length(x)) mean(x) else NA_real_ },
    mcare0 = { x <- (mcare_discharges / tot_discharges)[which(TaxYr %in% 2009:2011 & tot_discharges > 0)]; x <- x[is.finite(x)]; if (length(x)) mean(x) else NA_real_ },
    has_md0 = first_nn(c(has_md[TaxYr == TYPE_YEARS[1]], has_md[TaxYr == TYPE_YEARS[2]])),
    ceo_md0 = first_nn(c(ceo_md[TaxYr == TYPE_YEARS[1]], ceo_md[TaxYr == TYPE_YEARS[2]])),
    h_ceo_md0 = first_nn(c(h_ceo_md[TaxYr == TYPE_YEARS[1]], h_ceo_md[TaxYr == TYPE_YEARS[2]])),
    ceo_clinical0 = first_nn(c(ceo_clinical[TaxYr == TYPE_YEARS[1]], ceo_clinical[TaxYr == TYPE_YEARS[2]])),
    h_ceo_clinical0 = first_nn(c(h_ceo_clinical[TaxYr == TYPE_YEARS[1]], h_ceo_clinical[TaxYr == TYPE_YEARS[2]])),
    c_ceo_md0 = first_nn(c(c_ceo_md[TaxYr == TYPE_YEARS[1]], c_ceo_md[TaxYr == TYPE_YEARS[2]])),                   # combined measure (3 Nov)
    c_ceo_clinical0 = first_nn(c(c_ceo_clinical[TaxYr == TYPE_YEARS[1]], c_ceo_clinical[TaxYr == TYPE_YEARS[2]])),
    ceo_same0 = first_nn(c(h_ceo_same_990[TaxYr == TYPE_YEARS[1]], h_ceo_same_990[TaxYr == TYPE_YEARS[2]])),   # 990 CEO = HIMSS CEO (main def. 6 Nov)
    has_cmo0  = first_nn(c(has_cmo[TaxYr == TYPE_YEARS[1]], has_cmo[TaxYr == TYPE_YEARS[2]])),
    h_cmo0    = first_nn(c(h_cmo_title[TaxYr == TYPE_YEARS[1]], h_cmo_title[TaxYr == TYPE_YEARS[2]])),
    clin_share_raw0 = first_nn(c(exec_perc_clinical_no_vp[TaxYr == TYPE_YEARS[1]], exec_perc_clinical_no_vp[TaxYr == TYPE_YEARS[2]])),
    exposure = first_nn(pmax(err_wmean_pre - 1, 0)),                       # pre-policy excess readmissions (Jul 2008 - Jun 2011 window)
    pen0 = first_nn(as.integer(n_cond_err_above1_pre > 0)),               # would be penalised in FY2013 (any condition ERR > 1)
    fp = first_nn(fp), st = first_nn(c(st[TaxYr %in% 2009:2011], st)), exp_by2014 = first_nn(exp2014_state), .groups = "drop") %>%
    mutate(margin_r = percent_rank(margin0), mcare_r = percent_rank(mcare0)) %>%
    filter(hrrp, general0, if_all(all_of(COVS), ~ !is.na(.x))) %>%
    mutate(clin_share_hi0 = as.integer(clin_share_raw0 > median(clin_share_raw0, na.rm = TRUE)),
           exposure_sd = (exposure - mean(exposure, na.rm = TRUE)) / sd(exposure, na.rm = TRUE),
           cmi0_sd = (cmi0 - mean(cmi0)) / sd(cmi0), cmi_hi0 = as.integer(cmi0 > median(cmi0)),   # case-mix moderators (3 Nov)
           v_ceo_md0       = ifelse(ceo_same0 %in% 1, ceo_md0, NA),                                 # MAIN CEO definition (6 Nov, as analysis02)
           v_ceo_clinical0 = ifelse(ceo_same0 %in% 1, ceo_clinical0, NA),
           role_grp0  = if_else(ceo_same0 %in% 1, role_split(has_md0, v_ceo_md0, has_cmo0), NA_character_),   # role split, main definition
           rs_ceo0    = case_when(role_grp0 == RS_GRP[1] ~ 0L, role_grp0 == RS_GRP[2] ~ 1L),
           rs_nonceo0 = case_when(role_grp0 == RS_GRP[1] ~ 0L, role_grp0 %in% RS_GRP[3:4] ~ 1L),
           rs_cmo0    = case_when(role_grp0 == RS_GRP[1] ~ 0L, role_grp0 == RS_GRP[3] ~ 1L),
           rs_oth0    = case_when(role_grp0 == RS_GRP[1] ~ 0L, role_grp0 == RS_GRP[4] ~ 1L),
           rs_ceo_vs_nonceo0 = case_when(role_grp0 == RS_GRP[2] ~ 1L, role_grp0 %in% RS_GRP[3:4] ~ 0L))
  df %>% filter(TaxYr %in% YEARS) %>% select(-any_of(c("fp", "st", "exp_by2014"))) %>% inner_join(b, by = "id")
}
A <- hd %>% filter(in_leadership_data, !is.na(Filer.EIN)) %>% mutate(id = as.character(Filer.EIN), fp = 0L) %>% build()
B <- hd %>% filter(!is.na(MCRNUM)) %>% group_by(MCRNUM, TaxYr) %>% slice(1) %>% ungroup() %>%
  mutate(id = as.character(MCRNUM), fp = as.integer(!in_leadership_data)) %>% group_by(id) %>% mutate(fp = as.integer(mean(fp) > 0.5)) %>% ungroup() %>% build()
# CEO types = combined measure (3 Nov); the 990-measure comparison lives in analysis02 only (uncomment to add it here)
TYPES <- tribble(~panel, ~type, ~tlabel,
                 "A", "v_ceo_md0",       "Physician CEO (agreement)",          # MAIN (6 Nov): 990 = HIMSS CEO in the type year
                 "A", "v_ceo_clinical0", "Clinical CEO (agreement)",
                 "A", "has_md0",         "Physician executive (990)",
                 "A", "c_ceo_md0",       "Physician CEO (combined)",
                 "B", "c_ceo_md0",       "Physician CEO (combined, incl. for-profits)",
                 "A", "c_ceo_clinical0", "Clinical CEO (combined)",
                 "B", "c_ceo_clinical0", "Clinical CEO (combined, incl. for-profits)",
                 # "A", "ceo_md0",         "Physician CEO (990 measure)",
                 # "A", "ceo_clinical0",   "Clinical CEO (990 measure)",
                 "A", "has_cmo0",        "CMO in place (990)",
                 "B", "h_cmo0",          "CMO-titled executive (HIMSS)",
                 "A", "clin_share_hi0",  "Clinical share above median (990)")

get_w <- function(h, covs) {
  h <- h %>% filter(!is.na(t), if_all(all_of(covs), ~ !is.na(.x)))
  if (length(unique(h$t)) < 2 || min(table(h$t)) < 10) return(NULL)
  W <- try(weightit(as.formula(paste("t ~", paste(covs, collapse = " + "))), data = h, method = "ebal", estimand = "ATT"), silent = TRUE)
  if (inherits(W, "try-error")) NULL else tibble(id = h$id, w = W$weights)
}
# ---- ACA exposure by type: share of hospitals in states that expanded Medicaid in 2014, type 1 vs type 0 (raw and weighted) ----
cat("\n================ Share of hospitals in 2014-expansion states, by type ================\n")
cat("Hospitals dropped for missing state: A =", n_distinct(A$id[is.na(A$st)]), "of", n_distinct(A$id),
    "| B =", n_distinct(B$id[is.na(B$st)]), "of", n_distinct(B$id), "\n")
print(pmap_dfr(TYPES, function(panel, type, tlabel) {
  h <- (if (panel == "A") A else B) %>% filter(!is.na(st)) %>% distinct(id, .keep_all = TRUE) %>% mutate(t = .data[[type]])
  w <- get_w(h, if (panel == "B") c(COVS, "fp") else COVS); h <- h %>% filter(!is.na(t)); if (!is.null(w)) h <- h %>% left_join(w, by = "id")
  tibble(type = tlabel, type1 = mean(h$exp_by2014[h$t == 1]), type0_raw = mean(h$exp_by2014[h$t == 0]),
         type0_weighted = if (is.null(w)) NA_real_ else weighted.mean(h$exp_by2014[h$t == 0], h$w[h$t == 0], na.rm = TRUE))
}) %>% mutate(across(where(is.numeric), ~ round(.x, 3))), width = Inf)

SPECS <- c(MAIN = "Main: + Medicaid expansion control", STYR = "Robustness: state x year FE",
           E1 = "Triple: type x pre-policy excess readmissions (per SD) x post", E1B = "Triple: type x penalised in FY2013 x post",
           R0 = "Raw: unweighted, no covariates", C0 = "Weights without baseline case mix",
           M1 = "Triple: type x baseline case mix (per SD) x post", M1B = "Triple: type x high case mix (above median) x post")
# R0 / C0 / MAIN (3 Nov): how much of the raw type gap is hospital composition (other covariates, then case mix). M1 / M1B (3 Nov, before
# results): does the type gap differ with baseline case mix? Same structure as E1 / E1B (est = triple term, est_base = type x post).
# E1 / E1B (added 3 Nov): does the type gap grow with exposure to the penalty? MAIN controls plus exposure x post; `est` = the triple term,
# `est_base` = type x post (the gap at mean exposure for E1, among hospitals NOT penalised in FY2013 for E1B). Exposure is measured on the
# Jul 2008 - Jun 2011 window, which overlaps the post-ACA period; for readmission outcomes exposure x post also absorbs regression to the mean.
coef_named <- function(m, vars) { ct <- coeftable(m); hit <- which(map_lgl(strsplit(rownames(ct), ":"), ~ setequal(.x, vars))); if (length(hit) == 1) ct[hit, ] else NULL }
fit <- function(spec, panel, type, tlabel, group, label, col, to) {
  out <- function(...) tibble(spec = spec, type = tlabel, group = group, outcome = label, ...)
  xv <- c(E1 = "exposure_sd", E1B = "pen0", M1 = "cmi0_sd", M1B = "cmi_hi0")[spec]
  d  <- (if (panel == "A") A else B) %>% filter(TaxYr <= to, !is.na(.data[[col]]), !is.na(.data[[type]]), !is.na(st)) %>%
    { if (!is.na(xv)) filter(., !is.na(.data[[xv]])) %>% mutate(x = .data[[xv]]) else . } %>%
    mutate(yv = .data[[col]], t = .data[[type]]) %>% balance_ids()
  if (!any(d$TaxYr < 2012) || !any(d$TaxYr >= 2012)) return(out(note = "no pre- or post-period"))
  covs <- if (panel == "B") c(COVS, "fp") else COVS
  if (spec == "C0") covs <- setdiff(covs, "cmi0")
  w <- get_w(d %>% distinct(id, .keep_all = TRUE), covs); if (is.null(w)) return(out(note = "too few hospitals"))
  d <- d %>% inner_join(w, by = "id")             # R0 keeps the same hospitals but sets the weights to 1
  if (spec == "R0") d$w <- 1
  aca <- if (str_detect(group, WINDOW_GROUPS)) "mcaid_exp_win" else "mcaid_exp"
  fml <- as.formula(switch(spec,
                           MAIN = paste("yv ~ t:post + post_sys_change +", aca, "| id + TaxYr"),
                           STYR = "yv ~ t:post + post_sys_change | id + st^TaxYr",
                           R0 = , C0 = paste("yv ~ t:post + post_sys_change +", aca, "| id + TaxYr"),
                           paste("yv ~ t:post + x:post + t:x:post + post_sys_change +", aca, "| id + TaxYr")))
  m <- try(feols(fml, data = d, weights = ~w, cluster = ~id), silent = TRUE)
  if (inherits(m, "try-error")) return(out(note = "model failed"))
  r <- coef_named(m, if (is.na(xv)) c("t", "post") else c("t", "x", "post")); rb <- coef_named(m, c("t", "post"))
  if (is.null(r)) return(out(note = "model failed"))
  pre <- d %>% filter(TaxYr < 2012); mp <- weighted.mean(pre$yv, pre$w)
  sdp <- sd(pre$yv); sd_basis <- "pre"
  if (!is.finite(sdp) || sdp == 0) { sdp <- sd(d$yv[d$TaxYr >= 2012]); sd_basis <- "post" }   # programme payments are 0 before
  out(est = r[[1]], se = r[[2]], p = r[[4]], ci_lo = r[[1]] - 1.96 * r[[2]], ci_hi = r[[1]] + 1.96 * r[[2]], mean_pre = mp, sd_pre = sdp, sd_basis = sd_basis,
      est_sd = r[[1]] / sdp, ci_lo_sd = (r[[1]] - 1.96 * r[[2]]) / sdp, ci_hi_sd = (r[[1]] + 1.96 * r[[2]]) / sdp,
      est_base = if (is.na(xv)) NA_real_ else rb[[1]], p_base = if (is.na(xv)) NA_real_ else rb[[4]],
      years = paste(range(d$TaxYr), collapse = "-"), n_type1 = n_distinct(d$id[d$t == 1]), n_type0 = n_distinct(d$id[d$t == 0]), N = nobs(m), note = NA_character_)
}

res <- expand_grid(spec = names(SPECS), TYPES, reg %>% select(group, label, col, to)) %>%
  pmap_dfr(function(spec, panel, type, tlabel, group, label, col, to) { message(spec, " | ", tlabel, " | ", label); fit(spec, panel, type, tlabel, group, label, col, to) }) %>%
  group_by(spec, type, group) %>% mutate(n_family = sum(!is.na(p)), q_bh = p.adjust(p, method = "BH")) %>% ungroup()   # family = spec x type x outcome group

cat("\n================ MECHANISMS (MAIN): type x post, by outcome; q = BH within type x outcome group ================\n")
print(res %>% filter(spec == "MAIN", is.na(note)) %>% mutate(across(c(est, se, mean_pre, ci_lo_sd, ci_hi_sd), ~ round(.x, 3)), across(c(p, q_bh), ~ round(.x, 3))) %>%
        select(type, group, outcome, est, se, p, q_bh, n_family, ci_lo_sd, ci_hi_sd, sd_basis, mean_pre, years, n_type1, n_type0), width = Inf, n = 300)
cat("\n================ MAIN vs state x year FE (est, p) ================\n")
print(res %>% filter(is.na(note)) %>% select(spec, type, outcome, est, p) %>%
        pivot_wider(names_from = spec, values_from = c(est, p)) %>% mutate(across(where(is.numeric), ~ round(.x, 3))), width = Inf, n = 300)
cat("\n================ Type gap: raw (R0) -> other covariates (C0) -> + baseline case mix (MAIN), in SD ================\n")
print(res %>% filter(spec %in% c("R0", "C0", "MAIN"), is.na(note)) %>% select(spec, type, group, outcome, est_sd) %>%
        pivot_wider(names_from = spec, values_from = est_sd) %>% mutate(across(where(is.numeric), ~ round(.x, 3))), width = Inf, n = 300)
cat("\n================ TRIPLE: type gap x exposure (E1 per SD of pre-policy excess readmissions; E1B penalised in FY2013) ================\n")
print(res %>% filter(spec %in% c("E1", "E1B", "M1", "M1B"), is.na(note)) %>% select(spec, type, group, outcome, est_base, p_base, est, p, q_bh) %>%
        mutate(across(where(is.numeric), ~ round(.x, 3))), width = Inf, n = 600)
if (any(!is.na(res$note))) { cat("\nNot estimated:\n"); print(res %>% filter(!is.na(note)) %>% distinct(outcome, note), n = 50) }

# ---- pre-trends: event study for every outcome (MAIN controls), relative to 2011 ----------------------------------------
# Pre-period coefficients (2009, 2010) and their joint Wald test. Programme payments are skipped (constant before FY2013).
es_fit <- function(panel, type, tlabel, group, label, col, to) {
  if (str_detect(group, "^Program payments")) return(NULL)
  d <- (if (panel == "A") A else B) %>% filter(TaxYr <= to, !is.na(.data[[col]]), !is.na(.data[[type]]), !is.na(st)) %>%
    mutate(yv = .data[[col]], t = .data[[type]]) %>% balance_ids()
  if (!any(d$TaxYr < 2011) || !any(d$TaxYr >= 2012)) return(NULL)
  w <- get_w(d %>% distinct(id, .keep_all = TRUE), if (panel == "B") c(COVS, "fp") else COVS); if (is.null(w)) return(NULL)
  d <- d %>% inner_join(w, by = "id")
  aca <- if (str_detect(group, WINDOW_GROUPS)) "mcaid_exp_win" else "mcaid_exp"
  m <- try(feols(as.formula(paste0("yv ~ i(TaxYr, t, ref = 2011) + post_sys_change + ", aca, " | id + TaxYr")), data = d, weights = ~w, cluster = ~id), silent = TRUE)
  if (inherits(m, "try-error")) return(NULL)
  ct <- as.data.frame(coeftable(m)); ct$term <- rownames(ct)
  pre_yrs <- intersect(min(YEARS):2010, unique(d$TaxYr))
  wt <- try(wald(m, keep = paste0("^TaxYr::(", paste(pre_yrs, collapse = "|"), "):t$"), print = FALSE), silent = TRUE)
  sdp <- sd(d$yv[d$TaxYr < 2012])
  ct %>% filter(str_detect(term, "^TaxYr::\\d{4}:t$")) %>%
    transmute(type = tlabel, group = group, outcome = label, year = as.integer(str_extract(term, "\\d{4}")), est = Estimate, se = `Std. Error`) %>%
    bind_rows(tibble(type = tlabel, group = group, outcome = label, year = 2011L, est = 0, se = 0)) %>%
    mutate(lo = est - 1.96 * se, hi = est + 1.96 * se, sd_pre = sdp, pretrend_p = if (inherits(wt, "try-error")) NA_real_ else wt$p) %>% as_tibble()
}
es <- expand_grid(TYPES, reg %>% select(group, label, col, to)) %>%
  pmap_dfr(function(panel, type, tlabel, group, label, col, to) { message("ES | ", tlabel, " | ", label); es_fit(panel, type, tlabel, group, label, col, to) })
write_csv(es, paste0(tab_path, "mechanisms_event_study.csv"))
pt <- es %>% distinct(type, outcome, pretrend_p)
cat("\n================ Pre-trend p-values (windows / years 2009-2010 = 2011), MAIN controls ================\n")
print(pt %>% mutate(type = factor(type, levels = TYPES$tlabel), pretrend_p = round(pretrend_p, 3)) %>%
        pivot_wider(names_from = type, values_from = pretrend_p), width = Inf, n = 100)
res <- res %>% left_join(pt %>% rename(pretrend_p_main = pretrend_p) %>% mutate(spec = "MAIN"), by = c("spec", "type", "outcome"))
write_csv(res, paste0(tab_path, "mechanisms.csv"))

# one page per outcome: event study by type, in SD of the pre-period outcome
pdf(paste0(fig_path, "mechanisms_event_study.pdf"), width = 10, height = 8)
for (o in unique(es$outcome)) {
  print(ggplot(es %>% filter(outcome == o, !type %in% c("CMO in place (990)", "CMO-titled executive (HIMSS)", "Clinical share above median (990)", "Physician CEO (990 measure)", "Clinical CEO (990 measure)")) %>% mutate(type = factor(type, levels = TYPES$tlabel),
                                                                                                                                                                                                                            lab = paste0(type, "\npre-trend p = ", round(pretrend_p, 3))),
               aes(year, est / sd_pre)) +
          geom_hline(yintercept = 0, linetype = 2) + geom_vline(xintercept = 2011.5, linetype = 3) +
          geom_pointrange(aes(ymin = lo / sd_pre, ymax = hi / sd_pre), size = 0.25) + facet_wrap(~ lab, ncol = 2) +
          scale_x_continuous(breaks = YEARS) +
          labs(title = o, x = NULL, y = "Type 1 minus type 0, relative to 2011 (SD of the pre-period outcome)",
               caption = "Entropy-balanced; hospital and year FE; controls for system change and Medicaid expansion.") + theme_bw())
}
dev.off()

# ---- coefficient plot (effects in SD of the pre-period outcome, so outcomes are comparable) ------------------------
APPENDIX_TYPES <- c("CMO in place (990)", "CMO-titled executive (HIMSS)", "Clinical share above median (990)", "Physician CEO (990 measure)", "Clinical CEO (990 measure)",
                    "Physician CEO (combined)", "Clinical CEO (combined)")   # 6 Nov: combined (A) moved to comparison   # in the tables, not the figures
pd <- res %>% filter(is.na(note), !spec %in% c("R0", "C0", "STYR"), !type %in% APPENDIX_TYPES) %>% mutate(outcome = factor(outcome, levels = rev(unique(reg$label))), type = factor(type, levels = TYPES$tlabel),
                                                                                                          spec = factor(SPECS[spec], levels = SPECS))
p <- ggplot(pd, aes(est_sd, outcome, colour = type)) + geom_vline(xintercept = 0, linetype = 2) +
  geom_pointrange(aes(xmin = ci_lo_sd, xmax = ci_hi_sd), position = position_dodge(width = 0.7), size = 0.2) +
  facet_grid(group ~ spec, scales = "free_y", space = "free_y", labeller = labeller(spec = label_wrap_gen(30))) +
  labs(x = "Type 1 minus type 0, after vs before (SD of the pre-period outcome; post-period SD for programme payments; 95% CI)", y = NULL, colour = NULL,
       caption = "Entropy-balanced; hospital and year FE; controls for system change and Medicaid expansion; SEs clustered by hospital. CMO and clinical-share types: tables only.") +
  theme_bw() + theme(strip.text.y = element_text(angle = 0), legend.position = "bottom")
ggsave(paste0(fig_path, "mechanisms.pdf"), p, width = 16, height = 0.28 * nrow(distinct(pd, outcome)) * 2 + 3); print(p)

# ---- one figure per outcome family (5 Nov): pick main-text vs appendix later without rerunning ----------------------------
# Files: Objects/mechanisms_family_<family>.pdf. FAMILY_SPECS = which estimates are shown (columns); any of names(SPECS) except R0 / C0 / STYR
# can be added (E1B, M1, M1B). Same types as the combined figure (appendix types excluded).
FAMILY_SPECS <- c("MAIN", "E1")
slug <- function(x) str_replace_all(str_to_lower(x), "[^a-z0-9]+", "_") %>% str_remove_all("^_|_$")
for (g in unique(reg$group)) {
  pg <- res %>% filter(is.na(note), group == g, spec %in% FAMILY_SPECS, !type %in% APPENDIX_TYPES) %>%
    mutate(outcome = factor(outcome, levels = rev(unique(reg$label[reg$group == g]))), type = factor(type, levels = TYPES$tlabel),
           spec = factor(SPECS[spec], levels = SPECS[FAMILY_SPECS]))
  if (!nrow(pg)) next
  pf <- ggplot(pg, aes(est_sd, outcome, colour = type)) + geom_vline(xintercept = 0, linetype = 2) +
    geom_pointrange(aes(xmin = ci_lo_sd, xmax = ci_hi_sd), position = position_dodge(width = 0.7), size = 0.25) +
    facet_wrap(~ spec, nrow = 1, labeller = labeller(spec = label_wrap_gen(35))) +
    labs(title = g, x = "Type 1 minus type 0, after vs before (SD of the pre-period outcome; 95% CI)", y = NULL, colour = NULL,
         caption = "Entropy-balanced; hospital and year FE; controls for system change and Medicaid expansion; SEs clustered by hospital.") +
    theme_bw() + theme(legend.position = "bottom") + guides(colour = guide_legend(ncol = 2))
  ggsave(paste0(fig_path, "mechanisms_family_", slug(g), ".pdf"), pf, width = 4 + 4 * length(FAMILY_SPECS),
         height = 2.5 + 0.45 * n_distinct(pg$outcome) * max(1, n_distinct(pg$type) / 3))
}
cat("Family figures written:", paste0("mechanisms_family_", slug(unique(reg$group)), ".pdf", collapse = ", "), "\n")

# =============================== QUICK CHECKS (5 Nov; DECISIONS.md items E and H) ======================================
# (a) AHA full-time physicians & dentists (FTMDTF): is the large post-policy drop for clinically led hospitals a reporting change?
#     Reporting by year, year-to-year jumps, raw levels by type; MAIN / E1 / event study again (FT1) without hospitals with any jump of more
#     than 1 log point between consecutive years and (FT2) only hospitals reporting > 0 in every year.
# (b) AMI mortality one-year dip: reporting set by year; MAIN / E1 / event study again on a balanced panel (AMI1: hospitals reporting AMI
#     mortality in every window). A real change in care should show in ~3 consecutive overlapping windows, not one.
CHK_TYPES <- TYPES %>% filter(!tlabel %in% APPENDIX_TYPES)
rg <- function(lab) reg %>% filter(label == lab) %>% slice(1)
rerun <- function(lab, restrict, tag) {               # rerun MAIN, E1 and the event study on restricted panels (restrict(d, panel))
  r <- rg(lab); A0 <- A; B0 <- B
  on.exit({ A <<- A0; B <<- B0 })
  A <<- restrict(A0, "A"); B <<- restrict(B0, "B")
  cat("\n", tag, ": hospitals kept A =", n_distinct(A$id), "of", n_distinct(A0$id), "| B =", n_distinct(B$id), "of", n_distinct(B0$id), "\n")
  est <- pmap_dfr(expand_grid(spec = c("MAIN", "E1"), CHK_TYPES), function(spec, panel, type, tlabel) fit(spec, panel, type, tlabel, r$group, r$label, r$col, r$to))
  ev  <- pmap_dfr(CHK_TYPES, function(panel, type, tlabel) es_fit(panel, type, tlabel, r$group, r$label, r$col, r$to))
  list(est = est %>% mutate(check = tag, .before = 1), es = ev %>% mutate(check = tag, .before = 1))
}
orig <- function(lab) list(est = res %>% filter(outcome == lab, spec %in% c("MAIN", "E1"), type %in% CHK_TYPES$tlabel) %>% mutate(check = "0 as run", .before = 1),
                           es  = es  %>% filter(outcome == lab, type %in% CHK_TYPES$tlabel) %>% mutate(check = "0 as run", .before = 1))

# ---- (a) FT physicians & dentists ----
ft_lab <- "log FT physicians & dentists"; ft_col <- rg(ft_lab)$col
ft_flags <- function(d) d %>% transmute(id, TaxYr, ft = suppressWarnings(as.numeric(FTMDTF)), lft = .data[[ft_col]]) %>% arrange(id, TaxYr) %>%
  group_by(id) %>% mutate(jump = if_else(!is.na(lag(TaxYr)) & lag(TaxYr) == TaxYr - 1, lft - lag(lft), NA_real_)) %>% ungroup()
fA <- ft_flags(A)
cat("\n================ CHECK (a): AHA FT physicians & dentists, Panel A, by year ================\n")
print(fA %>% group_by(TaxYr) %>% summarise(hospitals = n(), reported = round(mean(!is.na(ft)), 3), zero = round(mean(ft == 0, na.rm = TRUE), 3),
                                           median = median(ft, na.rm = TRUE), mean = round(mean(ft, na.rm = TRUE), 1), p90 = quantile(ft, 0.9, na.rm = TRUE),
                                           jump_gt1 = round(mean(abs(jump) > 1, na.rm = TRUE), 3)), n = 20)
cat("\nMean log1p(FT physicians) and share zero by year, by physician / clinical CEO (agreement; raw, unweighted):\n")
for (tv in c("v_ceo_md0", "v_ceo_clinical0")) {
  cat(tv, "\n")
  print(fA %>% left_join(A %>% distinct(id, t = .data[[tv]]), by = "id") %>% filter(!is.na(t)) %>% group_by(TaxYr, t) %>%
          summarise(mean_log = round(mean(lft, na.rm = TRUE), 2), zero = round(mean(ft == 0, na.rm = TRUE), 3),
                    jump_gt1 = round(mean(abs(jump) > 1, na.rm = TRUE), 3), .groups = "drop") %>%
          pivot_wider(names_from = t, values_from = c(mean_log, zero, jump_gt1)), n = 20)
}
ft_keep <- function(mode) function(d, panel) {
  f <- ft_flags(d) %>% group_by(id) %>% summarise(any_jump = any(abs(jump) > 1, na.rm = TRUE), always_pos = sum(!is.na(ft)) >= 6 && all(ft > 0, na.rm = TRUE), .groups = "drop")
  d %>% filter(id %in% (if (mode == "FT1") f$id[!f$any_jump] else f$id[f$always_pos]))
}
chk <- list(orig(ft_lab),
            rerun(ft_lab, ft_keep("FT1"), "FT1 no year-to-year jump > 1 log point"),
            rerun(ft_lab, ft_keep("FT2"), "FT2 reports > 0 FT physicians every year"))

# ---- (b) AMI mortality ----
ami_lab <- "AMI mortality"; ami_col <- rg(ami_lab)$col
cat("\n================ CHECK (b): AMI mortality, Panel A, reporting set and raw mean by window-end year ================\n")
ami_yrs <- sort(unique(A$TaxYr[!is.na(A[[ami_col]]) & A$TaxYr <= rg(ami_lab)$to]))
ami_bal_ids <- function(d) d %>% filter(TaxYr %in% ami_yrs, !is.na(.data[[ami_col]])) %>% count(id) %>% filter(n == length(ami_yrs)) %>% pull(id)
balA <- ami_bal_ids(A)
print(A %>% filter(TaxYr %in% ami_yrs) %>% group_by(TaxYr) %>%
        summarise(reporting = sum(!is.na(.data[[ami_col]])), mean_all = round(mean(.data[[ami_col]], na.rm = TRUE), 3),
                  mean_balanced = round(mean(.data[[ami_col]][id %in% balA], na.rm = TRUE), 3), .groups = "drop") %>%
        mutate(balanced_hospitals = length(balA)), n = 20)
chk <- c(chk, list(orig(ami_lab),
                   rerun(ami_lab, function(d, panel) d %>% filter(id %in% ami_bal_ids(d)), "AMI1 balanced panel (AMI mortality in every window)")))

chk_est <- bind_rows(map(chk, "est")); chk_es <- bind_rows(map(chk, "es"))
write_csv(chk_est, paste0(tab_path, "mechanisms_checks.csv")); write_csv(chk_es, paste0(tab_path, "mechanisms_checks_es.csv"))
cat("\n================ QUICK CHECKS: MAIN and E1 as run vs restricted samples ================\n")
print(chk_est %>% filter(is.na(note)) %>% transmute(outcome, check, spec, type, est = round(est, 3), p = round(p, 3), n_type1) %>%
        arrange(outcome, type, spec, check), width = Inf, n = 200)
cat("\n================ QUICK CHECKS: event-study coefficients by year (relative to 2011) ================\n")
print(chk_es %>% transmute(outcome, check, type, year, est = round(est, 3)) %>% pivot_wider(names_from = year, values_from = est, names_sort = TRUE) %>%
        arrange(outcome, type, check), width = Inf, n = 200)

# =============================== PHYSICIAN EXECUTIVE SPLIT BY ROLE (pre-specified 5 Nov before results; DECISIONS.md item I) ===============
# Why does "Physician executive (990)" differ from the CEO types (null on readmission, HCAHPS declines)? Panel A, mutually exclusive groups
# (role_grp0; see RS_GRP): 0 none / 1 physician CEO / 2 physician exec not CEO, CMO in place / 3 physician exec not CEO, no CMO.
# Each binary type below is estimated exactly like the main types (fit(): entropy balancing, MAIN and E1) for every outcome, plus the
# event study. BH q-values within family = spec x type x outcome group, as above; these are diagnostic splits of an existing type.
RS_TYPES <- tribble(~panel, ~type, ~tlabel,
                    "A", "has_md0",           "Physician executive (990)",
                    "A", "rs_ceo0",           "1 physician CEO vs 0 none",
                    "A", "rs_nonceo0",        "2+3 physician exec, not CEO vs 0 none",
                    "A", "rs_cmo0",           "2 physician exec, not CEO, CMO in place vs 0 none",
                    "A", "rs_oth0",           "3 physician exec, not CEO, no CMO vs 0 none",
                    "A", "rs_ceo_vs_nonceo0", "1 physician CEO vs 2+3 physician exec, not CEO")
RS_SPECS <- c("MAIN", "E1")
cat("\n================ PHYSICIAN EXECUTIVE SPLIT: hospitals by group (Panel A) ================\n")
print(A %>% distinct(id, .keep_all = TRUE) %>% count(role_grp0 = factor(role_grp0, levels = RS_GRP)), width = Inf)
res_rs <- expand_grid(spec = RS_SPECS, RS_TYPES, reg %>% select(group, label, col, to)) %>%
  pmap_dfr(function(spec, panel, type, tlabel, group, label, col, to) { message("RS ", spec, " | ", tlabel, " | ", label); fit(spec, panel, type, tlabel, group, label, col, to) }) %>%
  group_by(spec, type, group) %>% mutate(n_family = sum(!is.na(p)), q_bh = p.adjust(p, method = "BH")) %>% ungroup()
es_rs <- expand_grid(RS_TYPES, reg %>% select(group, label, col, to)) %>%
  pmap_dfr(function(panel, type, tlabel, group, label, col, to) es_fit(panel, type, tlabel, group, label, col, to))
res_rs <- res_rs %>% left_join(es_rs %>% distinct(type, outcome, pretrend_p) %>% rename(pretrend_p_main = pretrend_p) %>% mutate(spec = "MAIN"),
                               by = c("spec", "type", "outcome"))
write_csv(res_rs, paste0(tab_path, "mechanisms_role_split.csv")); write_csv(es_rs, paste0(tab_path, "mechanisms_role_split_event_study.csv"))
cat("\n================ PHYSICIAN EXECUTIVE SPLIT: MAIN and E1 by outcome ================\n")
print(res_rs %>% filter(is.na(note)) %>% transmute(group, outcome, spec, type, est = round(est, 3), p = round(p, 3), q_bh = round(q_bh, 3),
                                                   pretrend_p_main = round(pretrend_p_main, 3), n_type1, n_type0) %>%
        arrange(group, outcome, spec, factor(type, levels = RS_TYPES$tlabel)), width = Inf, n = 600)

# figure: one page per outcome family, MAIN and E1 side by side, in SD of the pre-period outcome
pdf(paste0(fig_path, "mechanisms_role_split.pdf"), width = 13, height = 9)
for (g in unique(reg$group)) {
  pg <- res_rs %>% filter(is.na(note), group == g, type != "1 physician CEO vs 2+3 physician exec, not CEO") %>%
    mutate(outcome = factor(outcome, levels = rev(unique(reg$label[reg$group == g]))), type = factor(type, levels = RS_TYPES$tlabel),
           spec = factor(SPECS[spec], levels = SPECS[RS_SPECS]))
  if (!nrow(pg)) next
  print(ggplot(pg, aes(est_sd, outcome, colour = type)) + geom_vline(xintercept = 0, linetype = 2) +
          geom_pointrange(aes(xmin = ci_lo_sd, xmax = ci_hi_sd), position = position_dodge(width = 0.75), size = 0.25) +
          facet_wrap(~ spec, nrow = 1, labeller = labeller(spec = label_wrap_gen(35))) +
          labs(title = paste("Physician executive split by role:", g), x = "Type 1 minus type 0, after vs before (SD of the pre-period outcome; 95% CI)",
               y = NULL, colour = NULL, caption = "Panel A (nonprofits). Entropy-balanced; hospital and year FE; system-change and Medicaid controls; SEs clustered by hospital.") +
          theme_bw() + theme(legend.position = "bottom") + guides(colour = guide_legend(ncol = 2)))
}
dev.off()
# event studies for the split, HCAHPS first (the has_md0 decline), one page per outcome
pdf(paste0(fig_path, "mechanisms_role_split_event_study.pdf"), width = 10, height = 9)
for (o in c(unique(es_rs$outcome[str_detect(es_rs$group, "HCAHPS")]), setdiff(unique(es_rs$outcome), es_rs$outcome[str_detect(es_rs$group, "HCAHPS")]))) {
  print(ggplot(es_rs %>% filter(outcome == o, type != "1 physician CEO vs 2+3 physician exec, not CEO") %>%
                 mutate(type = factor(type, levels = RS_TYPES$tlabel), lab = paste0(type, "\npre-trend p = ", round(pretrend_p, 3))),
               aes(year, est / sd_pre)) +
          geom_hline(yintercept = 0, linetype = 2) + geom_vline(xintercept = 2011.5, linetype = 3) +
          geom_pointrange(aes(ymin = lo / sd_pre, ymax = hi / sd_pre), size = 0.25) + facet_wrap(~ lab, ncol = 2) +
          scale_x_continuous(breaks = YEARS) +
          labs(title = paste("Physician executive split:", o), x = NULL, y = "Type 1 minus type 0, relative to 2011 (SD of the pre-period outcome)") + theme_bw())
}
dev.off()

# =====================================================================================================================
# ---- GAMING CHECK: observation days (11 Nov; specification logged in DECISIONS.md before any estimate) --------------
# Observation stays are billed as outpatient care, so a returning patient kept in observation is not an HRRP readmission.
# HCRIS S-3 Part I: the 2552-10 form reports only TOTAL observation days (line 28, col 8; no Medicare column), and the count breaks
# at the change from the 2552-96 form (same hospitals: median 0.63x), so only 2010-form years are used: FY2011-2017.
#   Outcome (primary): observation share of days = 100 * obs / (obs + inpatient days), all patients. Secondary: log observation days.
#   Sample: Panel A, agreement CEO definition, hospitals reporting observation days in FY2011 (later missing = missing).
#   Design: annual MAIN and E1 as above (post = FY2012+; FY2011 is the only pre-year, so no pre-trend test).
#   Robustness: R13 = post is FY2013+ (penalty years), with FY2012 as a separate placebo term vs FY2011; Z0 = missing later years = 0.
#   Predictions: E1 (type x exposure x post) <= 0 if clinical leaders weigh patients' health; > 0 points to gaming / the published number.
#   Also reported: each group's own exposure gradient (type 0 = x:post; type 1 = x:post + t:x:post).
#   Family for BH q-values: spec x type x term, across the two outcomes.
# =====================================================================================================================
if (!all(c("obs_days_tot", "inpt_days_tot") %in% names(A))) {
  message("Observation days not in hospital_data - re-run the HCRIS extracts, HCRIS_combine.R and data02 first. Skipping the gaming check.")
} else {
  OBS_YEARS <- 2011:2017
  OBS_TYPES <- TYPES %>% filter(panel == "A", type %in% c("v_ceo_md0", "v_ceo_clinical0"))
  ob <- A %>% filter(TaxYr %in% OBS_YEARS) %>%
    mutate(obs_share   = if_else(inpt_days_tot > 0 & !is.na(obs_days_tot), 100 * obs_days_tot / (obs_days_tot + inpt_days_tot), NA_real_),
           log_obs     = if_else(obs_days_tot > 0, log(obs_days_tot), NA_real_),
           obs_share_z = if_else(inpt_days_tot > 0, 100 * coalesce(obs_days_tot, 0) / (coalesce(obs_days_tot, 0) + inpt_days_tot), NA_real_),
           p12 = as.integer(TaxYr == 2012), post13 = as.integer(TaxYr >= 2013))
  base_ids <- ob %>% filter(TaxYr == 2011, !is.na(obs_share)) %>% distinct(id) %>% pull(id)
  cat("\n================ GAMING CHECK: observation days (FY2011-2017) ================\n")
  cat("Panel A hospitals:", n_distinct(ob$id), "| reporting observation days in FY2011 (sample):", length(base_ids), "\n")
  ob <- ob %>% filter(id %in% base_ids)
  print(ob %>% group_by(TaxYr) %>% summarise(hospitals = n_distinct(id), share_reported = mean(!is.na(obs_share)),
                                             mean_obs_share = mean(obs_share, na.rm = TRUE), .groups = "drop") %>% mutate(across(where(is.double), ~ round(.x, 3))))
  dup <- ob %>% count(id, TaxYr) %>% filter(n > 1)
  if (nrow(dup)) warning(nrow(dup), " id-years appear more than once in the observation sample")
  
  O_RUNS <- tribble(~spec,     ~olabel,                                 ~var,
                    "MAIN",    "Observation share of days (%)",        "obs_share",
                    "MAIN",    "log observation days",                  "log_obs",
                    "E1",      "Observation share of days (%)",        "obs_share",
                    "E1",      "log observation days",                  "log_obs",
                    "MAIN_R13", "Observation share of days (%)",       "obs_share",
                    "E1_R13",  "Observation share of days (%)",        "obs_share",
                    "MAIN_Z0", "Observation share, missing = 0 (%)",   "obs_share_z",
                    "E1_Z0",   "Observation share, missing = 0 (%)",   "obs_share_z")
  term_of <- function(b, vars) { nm <- names(b)[map_lgl(strsplit(names(b), ":"), ~ setequal(.x, vars))]; if (length(nm) == 1) nm else NA_character_ }
  fit_obs <- function(spec, olabel, var, type, tlabel) {
    out <- function(...) tibble(spec = spec, type = tlabel, outcome = olabel, ...)
    x_on <- str_detect(spec, "^E1"); r13 <- str_detect(spec, "R13"); pv <- if (r13) "post13" else "post"
    d <- ob %>% filter(!is.na(.data[[var]]), !is.na(.data[[type]]), !is.na(st), !is.na(exposure_sd)) %>%
      mutate(yv = .data[[var]], t = .data[[type]], x = exposure_sd)
    w <- get_w(d %>% distinct(id, .keep_all = TRUE), COVS); if (is.null(w)) return(out(note = "too few hospitals"))
    d <- d %>% inner_join(w, by = "id")
    rhs <- case_when(!r13 & !x_on ~ "t:post", !r13 & x_on ~ "t:post + x:post + t:x:post",
                     r13 & !x_on ~ "t:p12 + t:post13", TRUE ~ "t:p12 + t:post13 + x:p12 + x:post13 + t:x:p12 + t:x:post13")
    m <- try(feols(as.formula(paste("yv ~", rhs, "+ post_sys_change + mcaid_exp | id + TaxYr")), data = d, weights = ~w, cluster = ~id), silent = TRUE)
    if (inherits(m, "try-error")) return(out(note = "model failed"))
    b <- coef(m); V <- vcov(m)
    one <- function(term_lab, vars) { k <- term_of(b, vars); if (is.na(k)) return(NULL)
    tibble(term = term_lab, est = unname(b[k]), se = sqrt(V[k, k])) }
    comb <- function(term_lab, v1, v2) { k1 <- term_of(b, v1); k2 <- term_of(b, v2); if (anyNA(c(k1, k2))) return(NULL)
    tibble(term = term_lab, est = unname(b[k1] + b[k2]), se = sqrt(V[k1, k1] + V[k2, k2] + 2 * V[k1, k2])) }
    rows <- if (!x_on) bind_rows(one("type x post", c("t", pv)), if (r13) one("placebo: type x FY2012", c("t", "p12")))
    else bind_rows(one("type x exposure x post (E1)", c("t", "x", pv)), one("type x post (at mean exposure)", c("t", pv)),
                   one("type 0 own exposure gradient", c("x", pv)), comb("type 1 own exposure gradient", c("x", pv), c("t", "x", pv)),
                   if (r13) one("placebo: type x exposure x FY2012", c("t", "x", "p12")))
    sdp <- sd(d$yv[d$TaxYr == 2011]); mp <- weighted.mean(d$yv[d$TaxYr == 2011], d$w[d$TaxYr == 2011])
    rows %>% mutate(p = 2 * pnorm(-abs(est / se)), ci_lo = est - 1.96 * se, ci_hi = est + 1.96 * se, est_sd = est / sdp,
                    mean_2011 = mp, sd_2011 = sdp, n_type1 = n_distinct(d$id[d$t == 1]), n_type0 = n_distinct(d$id[d$t == 0]), N = nobs(m),
                    note = NA_character_) %>%
      { bind_cols(out()[rep(1, nrow(.)), ], .) }
  }
  obs_res <- expand_grid(O_RUNS, OBS_TYPES %>% select(type, tlabel)) %>%
    pmap_dfr(function(spec, olabel, var, type, tlabel) { message("OBS | ", spec, " | ", tlabel, " | ", olabel); fit_obs(spec, olabel, var, type, tlabel) }) %>%
    group_by(spec, type, term) %>% mutate(q_bh = if_else(spec %in% c("MAIN", "E1"), p.adjust(p, method = "BH"), NA_real_)) %>% ungroup()
  # p uses the normal approximation to the clustered t statistic (hundreds of clusters)
  print(obs_res %>% filter(is.na(note)) %>% mutate(across(c(est, se, est_sd, mean_2011), ~ round(.x, 3)), across(c(p, q_bh), ~ round(.x, 3))) %>%
          select(spec, type, outcome, term, est, se, p, q_bh, est_sd, mean_2011, n_type1, n_type0), width = Inf, n = 100)
  if (any(!is.na(obs_res$note))) print(obs_res %>% filter(!is.na(note)) %>% select(spec, type, outcome, note))
  write_csv(obs_res, paste0(tab_path, "mechanisms_observation_days.csv"))
  
  # year-by-year (relative to FY2011): type gap (MAIN) and type x exposure (E1) - post-period dynamics only, no pre-years exist
  obs_es <- pmap_dfr(OBS_TYPES %>% select(type, tlabel), function(type, tlabel) {
    d <- ob %>% filter(!is.na(obs_share), !is.na(.data[[type]]), !is.na(st), !is.na(exposure_sd)) %>%
      mutate(yv = obs_share, t = .data[[type]], x = exposure_sd, tx = t * x)
    w <- get_w(d %>% distinct(id, .keep_all = TRUE), COVS); if (is.null(w)) return(NULL)
    d <- d %>% inner_join(w, by = "id")
    m1 <- try(feols(yv ~ i(TaxYr, t, ref = 2011) + post_sys_change + mcaid_exp | id + TaxYr, data = d, weights = ~w, cluster = ~id), silent = TRUE)
    m2 <- try(feols(yv ~ i(TaxYr, t, ref = 2011) + i(TaxYr, x, ref = 2011) + i(TaxYr, tx, ref = 2011) + post_sys_change + mcaid_exp | id + TaxYr,
                    data = d, weights = ~w, cluster = ~id), silent = TRUE)
    grab <- function(m, v, lab) { if (inherits(m, "try-error")) return(NULL); ct <- as.data.frame(coeftable(m)); ct$term <- rownames(ct)
    ct %>% filter(str_detect(term, paste0("^TaxYr::\\d{4}:", v, "$"))) %>%
      transmute(type = tlabel, series = lab, year = as.integer(str_extract(term, "\\d{4}")), est = Estimate, se = `Std. Error`) %>%
      bind_rows(tibble(type = tlabel, series = lab, year = 2011L, est = 0, se = 0)) }
    bind_rows(grab(m1, "t", "type gap (MAIN)"), grab(m2, "tx", "type x exposure (E1)")) %>% mutate(lo = est - 1.96 * se, hi = est + 1.96 * se)
  })
  write_csv(obs_es, paste0(tab_path, "mechanisms_observation_days_es.csv"))
  if (nrow(obs_es)) {
    p_obs <- ggplot(obs_es, aes(year, est)) + geom_hline(yintercept = 0, linetype = 2) + geom_vline(xintercept = 2011.5, linetype = 3) +
      geom_pointrange(aes(ymin = lo, ymax = hi), size = 0.25) + facet_grid(series ~ type, scales = "free_y") + scale_x_continuous(breaks = OBS_YEARS) +
      labs(title = "Observation share of days (%), relative to FY2011", x = NULL, y = "Estimate (percentage points; E1 per SD of exposure)",
           caption = "2552-10 form years only. Entropy-balanced; hospital and year FE; controls for system change and Medicaid expansion.") + theme_bw()
    ggsave(paste0(fig_path, "mechanisms_observation_days_es.pdf"), p_obs, width = 9, height = 6)
  }
}