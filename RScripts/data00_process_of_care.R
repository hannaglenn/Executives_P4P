library(dplyr)
library(tidyr)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(purrr)

source("./RScripts/paths.R")

### Script 6: Hospital Compare process-of-care measures (heart failure) -> CreatedData/process_of_care.rds (provider_id x year).
### Pre-specified 5 Nov (DECISIONS.md) before any estimates. Not run when written (no R in the sandbox).
### Files: <raw>/Hospital Compare/dbo_vwhqi_hosp_msr_xwlk_<year>.csv, the LATEST release of each year 2008-2014 (MSR files stop after 2014).
### TIMING (assumption - the files carry no dates): a late-in-year release reports 12 months of discharges ending about 9 months before
###   publication. POC_END_MONTH = month in the release year in which the 12-month window ends; the row is keyed to that END year (the same
###   no-look-ahead convention as the 36-month readmission windows). With the default (March), release R = discharges Apr R-1 - Mar R, so
###   release 2012 is ~2/3 after the Aug 2011 rule, 2013 and 2014 are fully after it. Change POC_END_MONTH to test the assumption.
### Outcomes (share of eligible patients, 0-100; NA when fewer than MIN_SAMPLE patients):
###   hf1  discharge instructions (HF-1: the process most directly tied to readmission)   hf2  LVS function evaluated (HF-2, inpatient)
###   hf3  ACE inhibitor / ARB for LVSD (HF-3)   hf1_minus_hf2 / hf1_minus_hf3  within-hospital differences (a DiD on these = triple difference)
###   hf1_noceil  HF-1 only for hospitals whose 2009-11 mean HF-1 was below 98% (ceiling check)

HC_DIR        <- file.path(raw_data_path, "Hospital Compare")
POC_END_MONTH <- 3
MIN_SAMPLE    <- 25

files <- list.files(HC_DIR, pattern = "^dbo_vwhqi_hosp_msr_xwlk_\\d{4}\\.csv$", full.names = TRUE, ignore.case = TRUE)
pick <- function(nm, pat) { h <- nm[str_detect(nm, regex(pat, ignore_case = TRUE))]; if (length(h)) h[1] else NA_character_ }
code_from_name <- function(cond, name) {               # 2008 has names only
  n <- str_to_lower(paste(coalesce(cond, ""), coalesce(name, "")))
  case_when(str_detect(n, "heart failure") & str_detect(n, "discharge instruction") ~ "HF-1",
            str_detect(n, "heart failure") & str_detect(n, "evaluation of left ventricular|lvs function|left ventricular systolic.*evaluat") ~ "HF-2",
            str_detect(n, "heart failure") & str_detect(n, "ace inhibitor|arb") ~ "HF-3",
            TRUE ~ NA_character_)
}
poc <- map_dfr(files, function(f) {
  d  <- read_csv(f, col_types = cols(.default = "c"), show_col_types = FALSE, locale = locale(encoding = "latin1"))
  nm <- names(d); cid <- pick(nm, "measure.?code|msr_cd"); cnm <- pick(nm, "measure.?name"); ccd <- pick(nm, "condition")
  tibble(release = as.integer(str_extract(basename(f), "\\d{4}")),
         provider_id = str_pad(str_trim(d[[pick(nm, "^provider")]]), 6, pad = "0"),
         mid = coalesce(if (!is.na(cid)) str_to_upper(str_replace_all(d[[cid]], "_", "-")) else NA_character_,
                        code_from_name(if (!is.na(ccd)) d[[ccd]] else NA_character_, d[[cnm]])),
         score  = suppressWarnings(as.numeric(str_remove_all(d[[pick(nm, "^score$")]], "[%, ]"))),
         sample = suppressWarnings(as.numeric(str_remove_all(d[[pick(nm, "^sample$")]], "[^0-9.]"))))
}) %>% filter(mid %in% c("HF-1", "HF-2", "HF-3"))
cat("Releases read:", paste(sort(unique(poc$release)), collapse = ", "), "\n")

poc_w <- poc %>% mutate(score = if_else(!is.na(sample) & sample >= MIN_SAMPLE, score, NA_real_)) %>%
  distinct(release, provider_id, mid, .keep_all = TRUE) %>%
  select(release, provider_id, mid, score) %>%
  pivot_wider(names_from = mid, values_from = score) %>%
  rename(hf1 = `HF-1`, hf2 = `HF-2`, hf3 = `HF-3`) %>%
  mutate(year = release,                                                                   # keyed to the window END year (see TIMING)
         hf1_minus_hf2 = hf1 - hf2, hf1_minus_hf3 = hf1 - hf3)
month_end   <- function(y, m) as.Date(sprintf("%d-%02d-01", ifelse(m == 12, y + 1, y), ifelse(m == 12, 1, m + 1))) - 1
month_start <- function(y, m) as.Date(sprintf("%d-%02d-01", y, m))
poc_w <- poc_w %>% mutate(poc_win_end   = month_end(year, POC_END_MONTH),
                          poc_win_start = if (POC_END_MONTH == 12) month_start(year, 1L) else month_start(year - 1L, POC_END_MONTH + 1L))
base98 <- poc_w %>% filter(year %in% 2009:2011) %>% group_by(provider_id) %>% summarise(hf1_base = mean(hf1, na.rm = TRUE), .groups = "drop")
poc_w <- poc_w %>% left_join(base98, by = "provider_id") %>%
  mutate(hf1_noceil = if_else(is.finite(hf1_base) & hf1_base < 98, hf1, NA_real_)) %>% select(-release, -hf1_base)

cat("\nCoverage and means by window-end year (MIN_SAMPLE =", MIN_SAMPLE, ", window ends month", POC_END_MONTH, "):\n")
print(poc_w %>% group_by(year) %>% summarise(hospitals = n(), hf1_n = sum(!is.na(hf1)), hf1 = round(mean(hf1, na.rm = TRUE), 1),
                                             hf2 = round(mean(hf2, na.rm = TRUE), 1), hf3 = round(mean(hf3, na.rm = TRUE), 1),
                                             hf1_minus_hf2 = round(mean(hf1_minus_hf2, na.rm = TRUE), 1), hf1_noceil_n = sum(!is.na(hf1_noceil)),
                                             win = paste(min(poc_win_start), "to", min(poc_win_end)), .groups = "drop"), width = Inf)
stopifnot(!anyDuplicated(poc_w[, c("provider_id", "year")]))
saveRDS(poc_w, paste0(created_data_path, "/process_of_care.rds"))
cat("Saved process_of_care.rds\n")