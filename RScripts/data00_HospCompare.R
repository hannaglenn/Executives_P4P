library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(purrr)

# =============================================================================
# data04_HospCompare_v2.R
#
# Builds hospital outcome panels (AMI / HF / pneumonia 30-day mortality and
# readmission) from the Hospital Compare archive, KEYED TO THE ACTUAL
# MEASUREMENT WINDOW rather than to the year in the file name.
#
# Why: a Hospital Compare file named year Y reports a 36-month window,
#      Jul (Y-4) through Jun (Y-1)  (e.g. the "2016" file is Jul 2012-Jun 2015).
#      The old script used Y as the year, so every outcome was dated 1-4 years
#      later than the care it measures, and consecutive years share 2 of 3 years
#      of discharges.
#
# Window source, per observation:
#   * "in_file" - measure start/end dates are inside the file (2014 onward)
#   * "rule"    - undated files (<= 2013): window = Jul(Y-4)-Jun(Y-1). The rule is
#                 corroborated by the dated HRRP files for the 2012 and 2013
#                 releases, and by in-file dates for 2014-2019.
#
# Outputs (CreatedData/):
#   mort_read_data_windowed.rds  wide: one row per provider x window-end-year
#   hc_outcomes_long.rds         long: provider x outcome x window, all metadata
#   hc_file_audit.csv            one row per source file: windows and counts
#   hrrp_err_long.rds            HRRP excess readmission ratios, every window
#   hrrp_pre_policy_exposure.rds provider-level ERR for Jul 2008-Jun 2011
#
# Requires: raw_data_path and created_data_path (source("./Scripts/paths.R"))
# =============================================================================

# source("./Scripts/paths.R")

hc_dir <- file.path(raw_data_path, "Hospital Compare")

RULE_YEARS_BACK  <- 4                      # undated release Y starts Jul (Y-4)
PN_EXPANSION_END <- as.Date("2015-06-30")  # first window with the expanded
# pneumonia cohort (the "2016" file)
HRRP_PRE_START   <- as.Date("2008-07-01")  # HRRP window used for FY2013 penalties
# Fill readmission windows that have no Hospital Compare file (here: the window ending Jun 2016) from the HRRP files?
# FALSE = leave them missing. The bridge matches Hospital Compare closely (corr 0.98+) but HRRP covers ~25% fewer hospitals
# (no critical access hospitals etc.), so a bridged year changes the hospital mix in that year only.
USE_HRRP_BRIDGE  <- TRUE

# ---- helpers ----------------------------------------------------------------

read_chr <- function(f) {
  # read everything as character: avoids guessing on IDs and masked scores
  if (str_detect(f, regex("\\.dta$", ignore_case = TRUE))) {
    # Stata file: drop value labels, turn dates/numbers into text so it looks like the csv reads, lower-case the names
    haven::read_dta(f) %>%
      haven::zap_labels() %>%
      mutate(across(everything(), as.character)) %>%
      rename_with(tolower)
  } else {
    read_csv(f, col_types = cols(.default = col_character()),
             locale = locale(encoding = "latin1"),
             show_col_types = FALSE, progress = FALSE) %>%
      rename_with(tolower)
  }
}

pick_chr <- function(df, vars) {
  found <- intersect(vars, names(df))
  if (length(found) == 0) return(rep(NA_character_, nrow(df)))
  df[[found[1]]]
}

release_year <- function(f) {
  y <- as.integer(str_extract(basename(f), "(?<!\\d)(20\\d{2})(?!\\d)"))
  if (is.na(y)) stop("No release year in file name: ", basename(f),
                     "  -> rename it to include the release year, e.g. ",
                     "unplanned_hospital_visits_2018.csv")
  y
}

# 6-digit CMS provider number. Old code lost the leading zero in some years
# (e.g. 10001 vs 010001), which silently breaks the join to MCRNUM.
clean_id <- function(x) {
  x <- str_trim(as.character(x))
  x <- str_remove(x, "\\.0+$")
  if_else(str_detect(x, "^[0-9]+$"), str_pad(x, 6, pad = "0"), x)
}

parse_hc_date <- function(x) {
  x   <- str_trim(as.character(x))
  out <- as.Date(rep(NA_character_, length(x)))
  iso <- !is.na(x) & str_detect(x, "^\\d{4}-\\d{2}-\\d{2}")
  mdy <- !is.na(x) & str_detect(x, "^\\d{1,2}/\\d{1,2}/\\d{4}")
  out[iso] <- as.Date(substr(x[iso], 1, 10), format = "%Y-%m-%d")
  out[mdy] <- as.Date(x[mdy], format = "%m/%d/%Y")
  # Oracle / Stata style day-month-year: "01-JUL-11", "30-JUN-2014", "01JUL2013". Month names are matched by hand so this
  # does not depend on the computer's language settings.
  dmy <- !is.na(x) & str_detect(x, "^\\d{1,2}-?[A-Za-z]{3}-?\\d{2,4}$")
  if (any(dmy)) {
    parts <- str_match(x[dmy], "^(\\d{1,2})-?([A-Za-z]{3})-?(\\d{2,4})$")
    yr <- as.integer(parts[, 4]); yr <- ifelse(yr < 100, 2000L + yr, yr)
    mo <- match(toupper(parts[, 3]), toupper(month.abb))
    out[dmy] <- as.Date(sprintf("%04d-%02d-%02d", yr, mo, as.integer(parts[, 2])))
  }
  out
}
valid_date <- function(d) {
  !is.na(d) & d >= as.Date("2000-01-01") & d <= as.Date("2035-12-31")
}
# the HRRP files contain garbled dates ("6/30/0201", "7/1/1920") in one column
# and a clean copy in another: take the first that is a plausible date
resolve_date <- function(primary, backup) {
  p <- parse_hc_date(primary); b <- parse_hc_date(backup)
  if_else(valid_date(p), p, b)
}

# 2008/2009 files mask decimals ("16.xx", "16.RR"): integer precision only
parse_score <- function(x) {
  x <- str_trim(as.character(x))
  tibble(
    score = suppressWarnings(as.numeric(str_extract(x, "^\\d+\\.?\\d*"))),
    score_rounded = replace_na(str_detect(x, "^\\d+\\.(xx|RR)"), FALSE)
  )
}

# AMI / HF / PN x mortality / readmission. Uses the measure ID when the file has
# one, otherwise the measure name; everything else (CABG, COPD, stroke, hip/knee,
# hospital-wide, complications) is left NA and dropped.
classify_outcome <- function(mid, name, cond) {
  mid <- str_to_upper(replace_na(mid, ""))
  txt <- str_to_lower(paste(replace_na(name, ""), replace_na(cond, "")))
  m   <- str_match(mid, "^(MORT|READM)_30_(AMI|HF|PN|COPD|STK|CABG|HIP_KNEE|THA_TKA|HOSP_WIDE)([-_]HRRP)?$")
  typ_id <- case_when(m[, 2] == "MORT" ~ "mortality", m[, 2] == "READM" ~ "readmission", TRUE ~ NA_character_)
  cd_id  <- case_when(m[, 3] == "AMI" ~ "ami", m[, 3] == "HF" ~ "hf", m[, 3] == "PN" ~ "pneumonia", m[, 3] == "COPD" ~ "copd",
                      m[, 3] == "STK" ~ "stk", m[, 3] == "CABG" ~ "cabg", m[, 3] %in% c("HIP_KNEE", "THA_TKA") ~ "hip_knee",
                      m[, 3] == "HOSP_WIDE" ~ "hosp_wide", TRUE ~ NA_character_)
  excluded <- str_detect(txt, "complication")                                   # complication measures stay out
  cd_txt <- case_when(
    excluded ~ NA_character_,
    str_detect(txt, "hospital-wide|hospital wide") ~ "hosp_wide",
    str_detect(txt, "\\bhip\\b|\\bknee\\b") ~ "hip_knee",
    str_detect(txt, "copd|chronic obstructive") ~ "copd",
    str_detect(txt, "cabg|coronary artery bypass") ~ "cabg",
    str_detect(txt, "stroke") ~ "stk",
    str_detect(txt, "heart attack|myocardial|\\bami\\b") ~ "ami",
    str_detect(txt, "heart failure|\\(hf\\)|\\bhf\\b") ~ "hf",
    str_detect(txt, "pneumonia|\\(pn\\)") ~ "pneumonia",
    TRUE ~ NA_character_)
  typ_txt <- case_when(excluded ~ NA_character_, str_detect(txt, "readmission") ~ "readmission",
                       str_detect(txt, "mortality|death") ~ "mortality", TRUE ~ NA_character_)
  cd <- coalesce(cd_id, cd_txt); typ <- coalesce(typ_id, typ_txt)
  if_else(is.na(cd) | is.na(typ), NA_character_, paste(cd, typ, sep = "_"))
}

# ---- 1. mortality / readmission files -----------------------------------------

all_files <- list.files(hc_dir, pattern = "\\.(csv|dta)$", full.names = TRUE,
                        ignore.case = TRUE)
# if the same file exists as both .csv and .dta, read the csv only
stems <- tools::file_path_sans_ext(all_files)
all_files <- all_files[!(str_detect(all_files, regex("\\.dta$", ignore_case = TRUE)) &
                           stems %in% stems[str_detect(all_files, regex("\\.csv$", ignore_case = TRUE))])]

outcome_files <- all_files[
  str_detect(basename(all_files),
             regex("mortality|readmdeath|readmcompdeath|deaths_hospital|unplanned",
                   ignore_case = TRUE)) &
    !str_detect(basename(all_files), regex("readm(ission)?_reduction", ignore_case = TRUE))
]
print(basename(outcome_files))

read_outcome_file <- function(f) {
  df  <- read_chr(f)
  rel <- release_year(f)
  
  start_raw <- pick_chr(df, c("measurestartdate", "measure_start_date", "start_date"))
  start_bak <- pick_chr(df, c("measurestartdatestr", "measure_start_datestr", "start_datestr"))
  end_raw   <- pick_chr(df, c("measureenddate", "measure_end_date", "end_date"))
  end_bak   <- pick_chr(df, c("measureenddatestr", "measure_end_datestr", "end_datestr"))
  
  dated <- any(!is.na(start_raw)) && any(!is.na(end_raw))
  rule_start <- as.Date(sprintf("%d-07-01", rel - RULE_YEARS_BACK))
  rule_end   <- as.Date(sprintf("%d-06-30", rel - 1))
  if (dated) {
    win_start <- resolve_date(start_raw, start_bak)
    win_end   <- resolve_date(end_raw, end_bak)
  } else {
    win_start <- rep(as.Date(NA), nrow(df))
    win_end   <- rep(as.Date(NA), nrow(df))
  }
  # per row: a row whose dates cannot be read falls back to the file-name rule (and is marked undated) instead of being lost
  row_dated <- !is.na(win_start) & !is.na(win_end)
  if (dated && any(!row_dated))
    message(basename(f), ": ", sum(!row_dated), " of ", length(row_dated),
            " rows have unreadable dates -> file-name rule used. Example raw values: ",
            paste(head(unique(na.omit(c(start_raw[!row_dated], end_raw[!row_dated]))), 3), collapse = " | "))
  win_start <- if_else(row_dated, win_start, rule_start)
  win_end   <- if_else(row_dated, win_end, rule_end)
  
  sc <- parse_score(pick_chr(df, c("score", "mortality_readmrate", "mortalityrate", "morreadr")))
  
  tibble(
    provider_id = clean_id(pick_chr(df, c("providerid", "provider_id", "provider", "facility_id"))),
    outcome = classify_outcome(
      pick_chr(df, c("measureid", "msr_cd", "measure_id")),
      pick_chr(df, c("measurename", "measure_name")),
      pick_chr(df, "condition")),
    score = sc$score,
    score_rounded = sc$score_rounded,
    lower = suppressWarnings(as.numeric(pick_chr(df, c("lowerestimate", "lower_estimate", "morreadl", "lowermortalityestimate")))),
    upper = suppressWarnings(as.numeric(pick_chr(df, c("higherestimate", "higher_estimate", "morreadu", "uppermortaility_readmestimate", "uppermortalityestimate")))),
    denominator = suppressWarnings(as.numeric(pick_chr(df, c("denominator", "patnum")))),
    release_year = rel,
    source_file = basename(f),
    window_dated = row_dated,
    win_start = win_start,
    win_end = win_end
  ) %>%
    filter(!is.na(outcome))
}

hc_long <- map_dfr(outcome_files, read_outcome_file)

n_nodate <- sum(is.na(hc_long$win_start) | is.na(hc_long$win_end))
if (n_nodate > 0) warning(n_nodate, " rows have no usable measurement window and are dropped")

hc_long <- hc_long %>%
  filter(!is.na(provider_id), !is.na(win_start), !is.na(win_end)) %>%
  arrange(desc(window_dated)) %>%                       # prefer in-file dates
  distinct(provider_id, outcome, win_start, win_end, .keep_all = TRUE) %>%
  mutate(win_end_year = as.integer(format(win_end, "%Y")),
         span_months  = round(as.numeric(win_end - win_start) / 30.44),
         data_source  = "hospital_compare")

# ---- checks -----------------------------------------------------------------

# every AMI/HF/PN window should be 36 months
bad_span <- hc_long %>% filter(span_months != 36) %>% count(source_file, span_months)
if (nrow(bad_span) > 0) { warning("Non-36-month windows found:"); print(bad_span) }

# a hospital should have one window per outcome ending in a given year
dup <- hc_long %>% count(provider_id, outcome, win_end_year) %>% filter(n > 1)
if (nrow(dup) > 0) stop("Multiple windows end in the same year for some provider/outcome; inspect `dup`")

# in-file dates should agree with the file-name rule (end = Jun of release - 1)
hc_long %>%
  filter(window_dated) %>%
  group_by(release_year) %>%
  summarise(share_matches_rule = mean(win_end_year == release_year - 1), .groups = "drop") %>%
  print()

file_audit <- hc_long %>%
  group_by(release_year, source_file, window_dated) %>%
  summarise(n = n(), first_start = min(win_start), last_end = max(win_end),
            share_rounded = mean(score_rounded), .groups = "drop") %>%
  arrange(release_year)
print(file_audit)
write_csv(file_audit, file.path(created_data_path, "hc_file_audit.csv"))

# ---- 3. HRRP excess readmission ratios (penalty exposure) -------------------

hrrp_files <- all_files[str_detect(basename(all_files),
                                   regex("readm(ission)?_reduction", ignore_case = TRUE))]

read_hrrp <- function(f) {
  df <- read_chr(f)
  measure <- str_to_upper(pick_chr(df, c("measurename", "measure_name")))
  tibble(
    provider_id = clean_id(pick_chr(df, c("provider", "provider_number", "providerid", "provider_id"))),
    condition = case_when(
      str_detect(measure, "\\bAMI\\b|MYOCARDIAL") ~ "ami",
      str_detect(measure, "\\bHF\\b|HEART FAILURE") ~ "hf",
      str_detect(measure, "\\bPN\\b|PNEUMONIA") ~ "pneumonia",
      str_detect(measure, "\\bHIP\\b") ~ "hip_knee",
      str_detect(measure, "COPD") ~ "copd",
      TRUE ~ NA_character_),
    err       = suppressWarnings(as.numeric(pick_chr(df, c("readratexc", "excess_readmission_ratio")))),
    predicted = suppressWarnings(as.numeric(pick_chr(df, c("readratp", "predicted_readmission_rate")))),
    expected  = suppressWarnings(as.numeric(pick_chr(df, c("readratexp", "expected_readmission_rate")))),
    discharges = suppressWarnings(as.numeric(pick_chr(df, c("disnum", "number_of_discharges")))),
    readmissions = suppressWarnings(as.numeric(pick_chr(df, c("readnum", "number_of_readmissions")))),
    win_start = resolve_date(pick_chr(df, c("startdatetime", "start_datestr")),
                             pick_chr(df, c("startdate", "start_date"))),
    win_end   = resolve_date(pick_chr(df, c("enddatetime", "end_datestr")),
                             pick_chr(df, c("enddate", "end_date"))),
    release_year = release_year(f),
    source_file = basename(f)
  ) %>%
    filter(!is.na(condition), !is.na(provider_id))
}

hrrp_long <- map_dfr(hrrp_files, read_hrrp) %>%
  distinct(provider_id, condition, win_start, win_end, .keep_all = TRUE)  # 2013 & 2014 files repeat a window

hrrp_long %>%
  distinct(release_year, source_file, win_start, win_end) %>%
  arrange(release_year) %>% print()

saveRDS(hrrp_long, file.path(created_data_path, "hrrp_err_long.rds"))

# pre-policy exposure: Jul 2008 - Jun 2011 (data behind the first FY2013 penalties)
hrrp_pre <- hrrp_long %>%
  filter(win_start == HRRP_PRE_START, condition %in% c("ami", "hf", "pneumonia"))

hrrp_pre_wide <- hrrp_pre %>%
  select(provider_id, condition, err, discharges) %>%
  pivot_wider(names_from = condition, values_from = c(err, discharges),
              names_glue = "{condition}_{.value}_pre")

hrrp_pre_summary <- hrrp_pre %>%
  filter(!is.na(err)) %>%
  group_by(provider_id) %>%
  summarise(
    err_wmean_pre = sum(err * discharges, na.rm = TRUE) / sum(discharges[!is.na(err)], na.rm = TRUE),
    err_max_pre = max(err),
    n_cond_err_above1_pre = sum(err > 1),
    n_cond_pre = n(),
    .groups = "drop")

hrrp_pre_exposure <- hrrp_pre_wide %>% left_join(hrrp_pre_summary, by = "provider_id")
saveRDS(hrrp_pre_exposure, file.path(created_data_path, "hrrp_pre_policy_exposure.rds"))

# ---- 2a. BRIDGE: fill readmission windows that have no Hospital Compare file from the HRRP files ----------
# Hospital Compare's readmission score is the risk-standardised rate = (predicted / expected) x national rate = ERR x national rate.
# The HRRP files carry the ERR for AMI / HF / pneumonia, so for a window with no Hospital Compare readmission file we rebuild
# the score as ERR x national rate. Checked on the Jul 2012 - Jun 2015 window (both sources exist): correlation 0.98-0.995 with the
# Hospital Compare score, RMSE about 0.2 percentage points. (HRRP's own `predicted` rate is NOT a good substitute: correlation 0.56-0.82.)
# Rows built this way are marked data_source = "hrrp_bridge" and flagged in the wide panel as any_readm_bridged.
bridge_cond <- c("ami", "hf", "pneumonia")

hrrp_cond <- hrrp_long %>%
  filter(condition %in% bridge_cond, !is.na(err), !is.na(discharges), discharges > 0) %>%
  mutate(win_end_year = as.integer(format(win_end, "%Y")))

# national crude readmission rate of each window x condition, from the hospitals in the file
nat_rate <- hrrp_cond %>%
  filter(!is.na(readmissions)) %>%
  group_by(condition, win_start, win_end) %>%
  summarise(national_crude = 100 * sum(readmissions) / sum(discharges), n_hosp = n(), .groups = "drop")

# calibration against Hospital Compare where both sources have the SAME window (exact start and end dates)
hc_readm <- hc_long %>%
  filter(str_detect(outcome, "_readmission$"), !score_rounded) %>%
  mutate(condition = str_remove(outcome, "_readmission$")) %>%
  select(provider_id, condition, win_start, win_end, hc_score = score)

calib <- hrrp_cond %>%
  inner_join(hc_readm, by = c("provider_id", "condition", "win_start", "win_end")) %>%
  left_join(nat_rate, by = c("condition", "win_start", "win_end")) %>%
  filter(!is.na(hc_score), !is.na(national_crude))

if (nrow(calib) > 0) {
  # scale factor between the true national rate behind the Hospital Compare score and the crude rate (about 1)
  adj_tbl <- calib %>%
    mutate(implied_nat = hc_score / err) %>%
    group_by(condition) %>%
    summarise(adj = median(implied_nat / national_crude, na.rm = TRUE), .groups = "drop")
  calib_check <- calib %>%
    left_join(adj_tbl, by = "condition") %>%
    mutate(bridge = err * national_crude * adj) %>%
    group_by(condition, win_end) %>%
    summarise(n = n(), corr = cor(hc_score, bridge), rmse = sqrt(mean((hc_score - bridge)^2)),
              mean_diff = mean(bridge - hc_score), .groups = "drop")
  message("HRRP bridge vs Hospital Compare readmission score, windows where both exist:")
  print(calib_check)
  write_csv(calib_check, file.path(created_data_path, "hrrp_bridge_check.csv"))
} else {
  warning("No window has both a Hospital Compare readmission file and an HRRP file: bridge scaled with adj = 1, UNVALIDATED")
  adj_tbl <- tibble(condition = bridge_cond, adj = 1)
}

# windows (by end year) that already have Hospital Compare readmission for a given provider and condition are NOT touched
hc_have <- hc_long %>%
  filter(str_detect(outcome, "_readmission$")) %>%
  transmute(provider_id, condition = str_remove(outcome, "_readmission$"), win_end_year) %>%
  distinct()

bridge_rows <- hrrp_cond %>%
  anti_join(hc_have, by = c("provider_id", "condition", "win_end_year")) %>%
  left_join(nat_rate, by = c("condition", "win_start", "win_end")) %>%
  left_join(adj_tbl, by = "condition") %>%
  filter(!is.na(national_crude)) %>%
  transmute(provider_id,
            outcome = paste0(condition, "_readmission"),
            score = err * national_crude * adj,
            score_rounded = FALSE, lower = NA_real_, upper = NA_real_,
            denominator = discharges,
            release_year, source_file, window_dated = TRUE,
            win_start, win_end, win_end_year,
            span_months = round(as.numeric(win_end - win_start) / 30.44),
            data_source = "hrrp_bridge")

bridge_rows %>% count(win_end_year, outcome) %>% print(n = 40)
message("Readmission rows added from HRRP: ", nrow(bridge_rows))

if (USE_HRRP_BRIDGE) {
  hc_long <- bind_rows(hc_long, bridge_rows)
} else {
  message("USE_HRRP_BRIDGE is FALSE: the ", nrow(bridge_rows), " bridged readmission rows were NOT added to the panel")
}

# any row without a window end year cannot be placed on the panel: show where it comes from, then drop it
no_year <- hc_long %>% filter(is.na(win_end_year))
if (nrow(no_year) > 0) {
  warning(nrow(no_year), " rows have no window end year and are dropped; sources:")
  print(no_year %>% count(source_file, data_source, outcome))
  hc_long <- hc_long %>% filter(!is.na(win_end_year))
}
dup2 <- hc_long %>% count(provider_id, outcome, win_end_year) %>% filter(n > 1)
if (nrow(dup2) > 0) stop("Bridge created duplicate provider/outcome/window-end-year rows; inspect `dup2`")

# ---- 2. wide panel keyed to the window --------------------------------------

win_tbl <- hc_long %>%
  group_by(win_end_year) %>%
  summarise(win_start = min(win_start), win_end = max(win_end),
            window_dated = all(window_dated), release_year = min(release_year),
            .groups = "drop") %>%
  mutate(year_mid = as.integer(format(win_start + (win_end - win_start) / 2, "%Y")))
stopifnot(!anyDuplicated(win_tbl$win_end_year))

scores <- hc_long %>%
  select(provider_id, year = win_end_year, outcome, score) %>%
  pivot_wider(names_from = outcome, values_from = score)

denoms <- hc_long %>%
  select(provider_id, year = win_end_year, outcome, denominator) %>%
  pivot_wider(names_from = outcome, values_from = denominator,
              names_glue = "{outcome}_n")

rounded <- hc_long %>%
  group_by(provider_id, year = win_end_year) %>%
  summarise(any_score_rounded = any(score_rounded),
            any_readm_bridged = any(data_source == "hrrp_bridge" & str_detect(outcome, "_readmission$")),
            .groups = "drop")

# mean only if ALL listed measures are reported (fixed composition). The old
# rowMeans(na.rm = TRUE) changed which conditions were averaged from year to
# year and hospital to hospital.
cc_mean <- function(...) {
  m <- cbind(...)
  if_else(rowSums(is.na(m)) == 0, rowMeans(m), NA_real_)
}

mort_panel <- scores %>%
  left_join(denoms,  by = c("provider_id", "year")) %>%
  left_join(rounded, by = c("provider_id", "year")) %>%
  left_join(win_tbl %>% rename(year = win_end_year), by = "year") %>%
  mutate(
    mort_3  = cc_mean(ami_mortality, hf_mortality, pneumonia_mortality),
    mort_hp = cc_mean(hf_mortality, pneumonia_mortality),
    mort_ap = cc_mean(ami_mortality, hf_mortality),
    read_3  = cc_mean(ami_readmission, hf_readmission, pneumonia_readmission),
    read_hp = cc_mean(hf_readmission, pneumonia_readmission),
    read_ap = cc_mean(ami_readmission, hf_readmission),
    # CMS expanded the pneumonia cohort (aspiration pneumonia, sepsis with
    # secondary pneumonia) starting with the "2016" release: level break in PN
    # mortality. AMI and HF are unaffected. Use mort_ap / hf_* to avoid it.
    pn_cohort_expanded = win_end >= PN_EXPANSION_END
  ) %>%
  arrange(provider_id, year)

# coverage by window, and the readmission gap
coverage <- mort_panel %>%
  group_by(year, win_start, win_end, window_dated) %>%
  summarise(across(c(ami_mortality, hf_mortality, pneumonia_mortality,
                     ami_readmission, hf_readmission, pneumonia_readmission),
                   ~ sum(!is.na(.x))), .groups = "drop")
print(coverage)

readm_years <- coverage %>% filter(!is.na(year), hf_readmission > 0) %>% pull(year)
readm_gap   <- setdiff(seq(min(readm_years), max(readm_years)), readm_years)
if (length(readm_gap) > 0)
  message("Readmission windows ending in ", paste(readm_gap, collapse = ", "),
          " are missing (no file) and are left NA.")

saveRDS(mort_panel, file.path(created_data_path, "mort_read_data_windowed.rds"))
saveRDS(hc_long,    file.path(created_data_path, "hc_outcomes_long.rds"))

# ---- 4. quick look: means by window, not by file year -------------------------

if (requireNamespace("ggplot2", quietly = TRUE)) {
  library(ggplot2)
  p_means <- mort_panel %>%
    select(year, all_of(c("ami_mortality", "hf_mortality", "pneumonia_mortality",
                          "ami_readmission", "hf_readmission", "pneumonia_readmission"))) %>%
    pivot_longer(-year, names_to = "outcome", values_to = "score") %>%
    group_by(year, outcome) %>%
    summarise(mean = mean(score, na.rm = TRUE), n = sum(!is.na(score)), .groups = "drop") %>%
    filter(n > 0) %>%
    ggplot(aes(year, mean, colour = outcome)) +
    geom_line() + geom_point() +
    geom_vline(xintercept = 2015, linetype = "dashed", colour = "grey50") +
    labs(x = "Year the 36-month window ends (June)", y = "Mean rate (%)",
         title = "Outcomes by measurement window",
         subtitle = "Dashed line: first window with expanded pneumonia cohort") +
    theme_minimal()
  print(p_means)
}