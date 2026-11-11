library(dplyr)
library(readr)
library(lubridate)

source("./RScripts/paths.R")

### Combine the v1996 and v2010 HCRIS extracts into final_HCRIS_data.csv (one row per provider x fiscal year).
### Written 6 Nov to replace the lost combine step. Follows Ian McCarthy's _HCRIS_Data.R (github.com/imccart/HCRIS, data-code/),
### adapted to the variables in HCRIS_v1996.R / HCRIS_v2010.R (no PPS file; extra capital / labour / days variables).
### His rules:
###   0. Stack the two forms. Where the SAME report period (provider, fy_start, fy_end) appears in both, keep the v2010 version, then the
###      most recently created report.
###   1. fiscal year = calendar year of fy_end. Providers with one report in that year: keep it ("unique reports").
###   2. Several reports whose lengths add up to under 370 days (a year split across reports): add them up ("total for year").
###   3. Otherwise, if one report covers more than 360 days and is the latest-ending one, keep it ("primary report").
###   4. Otherwise (fiscal-year change): weight each report's flows by its share of the days and add up ("weighted_average").
###   Flows (money, counts, days) are summed (NA only if all NA); beds = max; cost-to-charge, address = first; HRRP / VBP NA -> 0 within
###   duplicates, as in his code. Like his code, stocks (cash, assets) are summed too - only matters for split years.
### The existing final_HCRIS_data.csv is backed up and compared with the new file at the end. Not run when written (no R in the sandbox).

na_sum <- function(x) if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)
na_max <- function(x) if (all(is.na(x))) x[NA_integer_] else max(x, na.rm = TRUE)
na_min <- function(x) if (all(is.na(x))) x[NA_integer_] else min(x, na.rm = TRUE)
as_date_any <- function(x) if (inherits(x, "Date")) x else as.Date(parse_date_time(as.character(x), orders = c("mdy", "ymd", "dmy")))

rd <- function(f, src) read_csv(file.path(raw_data_path, f), col_types = cols(provider_number = "c", npi = "c", street = "c", city = "c", state = "c",
                                                                              zip = "c", county = "c", .default = col_guess()),
                                show_col_types = FALSE) %>% mutate(data_source = src)
v96 <- rd("final_HCRIS_v1996.csv", "v1996")
v10 <- rd("final_HCRIS_v2010.csv", "v2010") %>%
  mutate(uncomp_care = tot_uncomp_care_charges - tot_uncomp_care_partial_pmts + bad_debt)   # v1996 reports it directly (as in his code)

ID_VARS   <- c("report", "provider_number", "npi", "fy_start", "fy_end", "date_processed", "date_created", "status", "year", "data_source")
TEXT_VARS <- c("street", "city", "state", "zip", "county")
FIRST_NUM <- c("cost_to_charge")
MAX_NUM   <- c("beds")

# ---- 0. stack, parse dates, drop the same report period filed under both forms -------------------------------------------------
hc <- bind_rows(v96, v10) %>%                                   # bind_rows fills variables a form does not have with NA
  mutate(across(any_of(c("fy_start", "fy_end", "date_processed", "date_created")), as_date_any),
         tot_discounts = abs(tot_discounts), allowance = abs(allowance), hrrp_payment = abs(hrrp_payment),
         src_rank = if_else(data_source == "v2010", 2L, 1L)) %>%
  filter(!is.na(provider_number), !is.na(fy_end)) %>%
  group_by(provider_number, fy_start, fy_end) %>% arrange(desc(src_rank), desc(date_created), .by_group = TRUE) %>% slice(1) %>% ungroup() %>%
  select(-src_rank) %>% mutate(fyear = year(fy_end))
cat("Reports after removing cross-form duplicates:", nrow(hc), "\n"); print(count(hc, fyear, data_source), n = 30)

FLOW_VARS <- setdiff(names(hc)[vapply(hc, is.numeric, logical(1))], c(ID_VARS, MAX_NUM, FIRST_NUM, "fyear"))
cat("Summed across reports in split / changed fiscal years:", paste(FLOW_VARS, collapse = ", "), "\n")
collapse <- function(d) d %>% summarise(across(all_of(FLOW_VARS), na_sum), across(any_of(MAX_NUM), na_max), across(any_of(FIRST_NUM), first),
                                        across(any_of(TEXT_VARS), first), fy_start = na_min(fy_start), fy_end = na_max(fy_end),
                                        date_processed = na_max(date_processed), date_created = na_min(date_created), .groups = "drop")

hc <- hc %>% add_count(provider_number, fyear, name = "total_reports")
# ---- 1. one report in the fiscal year ----------------------------------------------------------------------------------------
u1 <- hc %>% filter(total_reports == 1) %>% select(-any_of(c("report", "npi", "status", "total_reports", "year", "data_source"))) %>%
  mutate(source = "unique reports")
dup <- hc %>% filter(total_reports > 1) %>% mutate(time_diff = as.numeric(fy_end - fy_start)) %>%
  group_by(provider_number, fyear) %>% mutate(total_days = sum(time_diff)) %>% ungroup()
# ---- 2. split year (lengths add to < 370 days): add up ----------------------------------------------------------------------
u2 <- dup %>% filter(total_days < 370) %>%
  mutate(hrrp_payment = coalesce(hrrp_payment, 0), hvbp_payment = coalesce(hvbp_payment, 0)) %>%
  group_by(provider_number, fyear) %>% collapse() %>% mutate(source = "total for year")
# ---- 3. one report covers the year and ends last: keep it --------------------------------------------------------------------
dup2 <- dup %>% filter(total_days >= 370) %>% group_by(provider_number, fyear) %>%
  mutate(max_days = max(time_diff), max_date = max(fy_end)) %>% ungroup()
u3 <- dup2 %>% filter(max_days == time_diff, time_diff > 360, max_date == fy_end) %>%
  group_by(provider_number, fyear) %>% slice(1) %>% ungroup() %>%   # guard against exact ties (his code would keep both)
  select(-any_of(c("report", "npi", "status", "total_reports", "year", "data_source", "max_days", "time_diff", "total_days", "max_date"))) %>%
  mutate(source = "primary report")
# ---- 4. fiscal-year change: weight flows by days and add up -------------------------------------------------------------------
u4 <- anti_join(dup2, u3, by = c("provider_number", "fyear")) %>%
  mutate(across(all_of(FLOW_VARS), ~ .x * (time_diff / total_days)),
         hrrp_payment = coalesce(hrrp_payment, 0), hvbp_payment = coalesce(hvbp_payment, 0)) %>%
  group_by(provider_number, fyear) %>% collapse() %>% mutate(source = "weighted_average")

final_hcris <- bind_rows(u1, u2, u3, u4) %>% rename(year = fyear) %>% arrange(provider_number, year)
stopifnot(!anyDuplicated(final_hcris[, c("provider_number", "year")]))
cat("\nProvider-years by rule:\n"); print(count(final_hcris, source))

# ---- save, keeping a copy of the old file, and compare ---------------------------------------------------------------------------
out_f <- file.path(raw_data_path, "final_HCRIS_data.csv"); old_f <- file.path(raw_data_path, "final_HCRIS_data_OLD.csv")
if (file.exists(out_f) && !file.exists(old_f)) file.copy(out_f, old_f)
write_csv(final_hcris, out_f)
cat("\nWrote", out_f, "-", nrow(final_hcris), "provider-years\n")
if (file.exists(old_f)) {
  old <- read_csv(old_f, col_types = cols(provider_number = "c", .default = col_guess()), show_col_types = FALSE)
  pad <- function(x) ifelse(grepl("^[0-9]+$", x), sprintf("%06d", as.integer(x)), x)
  cmp <- inner_join(old %>% mutate(provider_number = pad(provider_number)), final_hcris %>% mutate(provider_number = pad(provider_number)),
                    by = c("provider_number", "year"), suffix = c("_old", "_new"))
  cat("\nRows: old", nrow(old), "| new", nrow(final_hcris), "| matched provider-years", nrow(cmp), "\n")
  for (v in intersect(c("tot_operating_exp", "tot_pat_rev", "beds", "tot_discharges", "mcare_discharges", "hrrp_payment"), names(old))) {
    a <- cmp[[paste0(v, "_old")]]; b <- cmp[[paste0(v, "_new")]]; ok <- !is.na(a) & !is.na(b)
    cat(sprintf("  %-18s identical %5.1f%%   correlation %.4f   (n = %d)\n", v, 100 * mean(abs(a[ok] - b[ok]) < 1e-6), cor(a[ok], b[ok]), sum(ok)))
  }
  cat("New columns not in the old file:", paste(setdiff(names(final_hcris), names(old)), collapse = ", "), "\n")
  # 11 Nov: year coverage - the new file must cover every year the old one did (data02 uses HCRIS through 2017)
  cov <- full_join(count(old, year, name = "old"), count(final_hcris, year, name = "new"), by = "year") %>% arrange(year)
  cat("\nProvider-years by fiscal year (old vs new):\n"); print(as.data.frame(cov), row.names = FALSE)
  lost <- cov %>% filter(year %in% 2008:2017, is.na(new) | new < 0.9 * old)
  if (nrow(lost)) warning("New file has fewer provider-years than the old one in: ", paste(lost$year, collapse = ", "),
                          " - check those raw HCRIS years were downloaded")
}

# 11 Nov: observation days plausibility (should be a few % of inpatient days, rising over time; no jump at the 2010-11 form change)
if (all(c("obs_days_mcare", "inpt_days_mcare") %in% names(final_hcris))) {
  chk <- final_hcris %>% filter(year %in% 2008:2017) %>% group_by(year) %>%
    summarise(n = n(), share_obs_reported = mean(!is.na(obs_days_mcare)), share_obs_zero = mean(obs_days_mcare %in% 0),
              med_ratio_mcare = median(obs_days_mcare / inpt_days_mcare, na.rm = TRUE),
              med_ratio_tot   = median(obs_days_tot / inpt_days_tot, na.rm = TRUE), .groups = "drop")
  cat("\nObservation days / inpatient days by year:\n"); print(as.data.frame(chk), row.names = FALSE, digits = 3)
}