# stable_sample.R
# Choose the hospital sample AT ANALYSIS TIME instead of inside data03.
# Source this file, then call stable_sample() on hospital_leader_data (or the merged hospital_data).
#
# A hospital is "stable" if it has leadership data in every year of a chosen set of years.
#   require = "all"       -> every year from start to end (a fully balanced panel)
#   require = c(2009,2010,2013:2018) -> any explicit set of years (e.g. skip the 2011-2012 transition)
#   min_run               -> alternative: keep hospitals with at least this many consecutive years inside the window
#
# Presence is read from the rows that exist (in_exec_data / in_board_data), so a hospital-year only counts if the
# leadership data actually has it.

library(dplyr)

stable_sample <- function(data, start = 2009, end = 2018, years = NULL,
                          source = c("exec", "board", "either", "both"),
                          min_run = NULL, max_missing_ceo = NULL) {
  source <- match.arg(source)
  need <- if (is.null(years)) start:end else years
  
  # hospital_exec_data.rds only has exec rows, so presence flags may not exist yet
  if (!"in_exec_data"  %in% names(data)) data$in_exec_data  <- TRUE
  if (!"in_board_data" %in% names(data)) data$in_board_data <- NA
  
  present <- data %>%
    mutate(.in = case_when(
      source == "exec"   ~ in_exec_data,
      source == "board"  ~ in_board_data,
      source == "either" ~ in_exec_data | in_board_data,
      source == "both"   ~ in_exec_data & in_board_data)) %>%
    filter(.in, TaxYr %in% need) %>%
    distinct(Filer.EIN, TaxYr)
  
  if (is.null(min_run)) {
    keep <- present %>% count(Filer.EIN) %>% filter(n == length(need)) %>% pull(Filer.EIN)
  } else {
    keep <- present %>% arrange(Filer.EIN, TaxYr) %>%
      group_by(Filer.EIN) %>% mutate(run_id = TaxYr - row_number()) %>%
      count(Filer.EIN, run_id) %>% group_by(Filer.EIN) %>%
      summarise(longest = max(n), .groups = "drop") %>%
      filter(longest >= min_run) %>% pull(Filer.EIN)
  }
  
  out <- data %>% filter(Filer.EIN %in% keep, TaxYr %in% need)
  
  # optional: also require few years with no CEO inside the window
  if (!is.null(max_missing_ceo)) {
    ok <- out %>% group_by(Filer.EIN) %>%
      summarise(miss = sum(has_ceo == 0, na.rm = TRUE), .groups = "drop") %>%
      filter(miss <= max_missing_ceo) %>% pull(Filer.EIN)
    out <- out %>% filter(Filer.EIN %in% ok)
  }
  out
}

# How many hospitals does each window keep? Use this to pick the window, and plot it.
window_survival <- function(data, starts = 2009:2012, ends = 2016:2019, source = "exec") {
  expand.grid(start = starts, end = ends) %>%
    filter(end - start >= 4) %>%
    rowwise() %>%
    mutate(n_hospitals = n_distinct(stable_sample(data, start, end, source = source)$Filer.EIN)) %>%
    ungroup()
}