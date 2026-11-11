library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(purrr)
library(tibble)
library(ggplot2)
library(fixest)
library(WeightIt)

### Descriptive / motivating (NOT causal): do outcomes move when a hospital GAINS a leadership attribute (e.g. a physician CEO)?
### Pre-specified 3 Nov, before results.
###   D1 "gain vs never": staggered DiD, Sun & Abraham (fixest::sunab). Treated = hospitals with a SUSTAINED gain in 2010-2016
###      (type 0 in the year before, type 1 in the gain year and, where observed, the year after); controls = hospitals of type 0 in every
###      observed year. Hospitals that start as type 1, or flip back and forth, are dropped. Event window -4..+4 (treated rows outside dropped).
###   D2 "same turnover, different hire" (CEO attributes only): among hospitals whose CEO changed in 2010-2016 while the outgoing CEO was
###      type 0, compare those that hired a type-1 CEO with those that hired a type-0 CEO, in event time around the change
###      (y ~ i(event time, hired type 1) | hospital + year + event time). Separates the attribute from turnover itself.
### Both: hospital and year FE, controls for system change and Medicaid expansion; SEs clustered by hospital.
### Known problems (why this is only motivating): leaders are chosen, often after bad years (look at the pre-period: an Ashenfelter-style
### dip), and arrive with other changes; 990 attributes are noisy year to year; window outcomes (36 months) absorb a change over ~3 years.
### Panels, samples and controls come from analysis02 (run up to BUILD_DONE). Not run when written (no R in the sandbox).

source("./RScripts/paths.R")
A02_PATH <- "./RScripts/analysis02_response_by_type.R"
if (exists("BUILD_DONE")) rm(BUILD_DONE)
for (e in parse(A02_PATH, keep.source = FALSE)) { eval(e, envir = globalenv()); if (exists("BUILD_DONE")) break }
stopifnot(exists("BUILD_DONE"), exists("A"), exists("B"))
# MAIN CEO definition from 6 Nov (as analysis02): Panel A uses 990 physician / clinical status in years where the 990 and HIMSS name the
# same CEO (h_ceo_same_990 = 1), NA otherwise. Panel B (incl. for-profits) has no 990 and keeps the combined measure.
A <- A %>% mutate(v_ceo_md_ann = ifelse(h_ceo_same_990 %in% 1, ceo_md, NA), v_ceo_clin_ann = ifelse(h_ceo_same_990 %in% 1, ceo_clinical, NA))
fig_path <- "./Objects/"; tab_path <- "./Tables/"
wins <- function(x, p = c(0.01, 0.99)) { q <- quantile(x, p, na.rm = TRUE); pmin(pmax(x, q[1]), q[2]) }

GAIN_YEARS <- 2010:2016
WIN <- 4
# CEO attributes = combined measure (3 Nov): CEO identity, and so CEO changes, come from HIMSS (h_ceo_change) in both panels.
# Uses the version WITHOUT the 990 fallback (*_nofb): in annual data a switch between sources would otherwise look like a gain or loss.
ATTRS <- tribble(~panel, ~var, ~label, ~ceo_change_var,
                 "A", "v_ceo_md_ann",        "Physician CEO (agreement)",               "h_ceo_change",   # main definition (6 Nov)
                 "A", "v_ceo_clin_ann",      "Clinical CEO (agreement)",                "h_ceo_change",
                 "B", "c_ceo_md_nofb",       "Physician CEO (combined, incl. for-profits)", "h_ceo_change",
                 "B", "c_ceo_clinical_nofb", "Clinical CEO (combined, incl. for-profits)",  "h_ceo_change",
                 # "A", "ceo_md",         "Physician CEO (990 measure)",          "ceo_entry",       # comparison, uncomment to restore
                 # "A", "ceo_clinical",   "Clinical CEO (990 measure)",           "ceo_entry",
                 "A", "has_md",         "Physician executive (990)",               NA,
                 "A", "has_cmo",        "CMO in place (990)",                      NA,
                 "B", "h_cmo_title",    "CMO-titled executive (HIMSS)",            NA)
OUT <- tribble(~y, ~label, ~tf, ~to, ~window,
               "tot_operating_exp", "log operating expenses",          "log",  2017, FALSE,
               "tot_pat_rev",       "log patient revenue",             "log",  2017, FALSE,
               "oper_margin",       "Operating margin (winsorised)",   "wins", 2017, FALSE,
               "tot_discharges",    "log total discharges",            "log",  2017, FALSE,
               "cmi",               "Case mix index",                  "none", 2017, FALSE,
               "overall_rating",    "HCAHPS overall rating",           "none", 2017, FALSE,
               "recommend_hospital","HCAHPS recommend hospital",       "none", 2017, FALSE,
               "all_read",          "Readmission, all conditions",     "none", 2014, TRUE,
               "hf_readmission",    "HF readmission",                  "none", 2017, TRUE,
               "mort_ap",           "Mortality, AMI + HF",             "none", 2017, TRUE) %>%
  filter(y %in% names(A))
wald_p <- function(wt) if (is.list(wt) && !is.null(wt$p)) wt$p else NA_real_   # wald() returns NA (not a list) when nothing matches
prep_y <- function(x, tf) switch(tf, none = x, wins = wins(x), log = ifelse(x > 0, log(x), NA_real_))

# ---- events: sustained gains, never-treated controls --------------------------------------------------------------
events <- function(d, v) {
  d %>% filter(!is.na(.data[[v]])) %>% arrange(id, TaxYr) %>% group_by(id) %>%
    mutate(x = as.integer(.data[[v]] > 0), prev = lag(x), nxt = lead(x), prev_yr = lag(TaxYr), next_yr = lead(TaxYr),
           gain = x == 1 & !is.na(prev) & prev == 0 & prev_yr == TaxYr - 1 & (is.na(nxt) | (nxt == 1 & next_yr == TaxYr + 1))) %>%
    summarise(start0 = x[1] == 0, n_switch = sum(x != lag(x), na.rm = TRUE), ever1 = any(x == 1),
              g = { gy <- TaxYr[gain & TaxYr %in% GAIN_YEARS]; if (length(gy)) min(gy) else NA_real_ }, .groups = "drop") %>%
    mutate(group = case_when(!ever1 ~ "never", start0 & !is.na(g) & n_switch == 1 ~ "gain", TRUE ~ "drop"))
}
ev_tab <- pmap_dfr(ATTRS, function(panel, var, label, ceo_change_var) {
  e <- events(if (panel == "A") A else B, var); e %>% count(group) %>% mutate(attribute = label) %>%
    bind_rows(e %>% filter(group == "gain") %>% count(g) %>% transmute(group = paste0("gain in ", g), n, attribute = label))
})
cat("\n================ Hospitals by event group (gain = one sustained 0 -> 1 switch in 2010-2016) ================\n")
print(ev_tab %>% pivot_wider(names_from = group, values_from = n, values_fill = 0), width = Inf)

# ---- D1: Sun & Abraham, gain vs never ------------------------------------------------------------------------------
d1_fit <- function(panel, var, label, y, ylab, tf, to, window) {
  d <- if (panel == "A") A else B
  e <- events(d, var) %>% filter(group %in% c("gain", "never")) %>% mutate(cohort = if_else(group == "gain", g, 10000))
  dd <- d %>% inner_join(e %>% select(id, cohort), by = "id") %>% filter(TaxYr <= to) %>%
    mutate(yv = prep_y(suppressWarnings(as.numeric(.data[[y]])), tf), rel = if_else(cohort < 10000, TaxYr - cohort, NA_real_)) %>%
    filter(!is.na(yv), is.na(rel) | abs(rel) <= WIN)
  if (sum(dd$cohort < 10000 & dd$rel >= 0, na.rm = TRUE) < 30) return(NULL)
  aca <- if (window) "mcaid_exp_win" else "mcaid_exp"
  m <- try(feols(as.formula(paste0("yv ~ sunab(cohort, TaxYr) + post_sys_change + ", aca, " | id + TaxYr")), data = dd, cluster = ~id), silent = TRUE)
  if (inherits(m, "try-error")) return(NULL)
  att <- coeftable(summary(m, agg = "ATT")); att <- att[rownames(att) == "ATT", ]
  ct <- as.data.frame(coeftable(m)); ct$term <- rownames(ct)
  es <- ct %>% filter(str_detect(term, "::-?\\d+$")) %>% transmute(rel = as.integer(str_extract(term, "-?\\d+$")), est = Estimate, se = `Std. Error`) %>%
    bind_rows(tibble(rel = -1L, est = 0, se = 0))
  pre <- es$rel[es$rel < -1]
  # pre-trend: the AVERAGE of the pre-period (rel <= -2) cohort-specific coefficients, with its SE (fixest::aggregate). A joint Wald test
  # over dozens of cohort x period terms is unreliable here (near-singular VCOV with few hospitals per cohort).
  ag <- if (length(pre)) try(aggregate(m, c("pre" = paste0("^TaxYr::(", paste(pre, collapse = "|"), "):cohort"))), silent = TRUE) else NULL
  wt <- if (!is.null(ag) && !inherits(ag, "try-error")) list(p = ag[1, 4], est = ag[1, 1]) else NULL
  sdp <- sd(dd$yv[is.na(dd$rel) | dd$rel < 0])
  list(att = tibble(design = "D1 gain vs never (Sun & Abraham)", attribute = label, outcome = ylab, est = att[[1]], se = att[[2]], p = att[[4]],
                    est_sd = att[[1]] / sdp, pretrend_p = wald_p(wt),
                    n_gain = n_distinct(dd$id[dd$cohort < 10000]), n_never = n_distinct(dd$id[dd$cohort == 10000]), N = nobs(m)),
       es = es %>% mutate(design = "D1", attribute = label, outcome = ylab, sd = sdp))
}

# ---- D2: among CEO changes, hired type 1 vs hired type 0 --------------------------------------------------------------
d2_fit <- function(panel, var, label, ceo_change_var, y, ylab, tf, to, window) {
  if (is.na(ceo_change_var)) return(NULL)
  d <- if (panel == "A") A else B
  ch <- d %>% filter(!is.na(.data[[var]]), !is.na(.data[[ceo_change_var]])) %>% arrange(id, TaxYr) %>% group_by(id) %>%
    mutate(x = as.integer(.data[[var]] > 0), prev = lag(x), prev_yr = lag(TaxYr)) %>%
    filter(.data[[ceo_change_var]] %in% 1, TaxYr %in% GAIN_YEARS, prev %in% 0, prev_yr == TaxYr - 1) %>%
    slice_min(TaxYr, n = 1, with_ties = FALSE) %>% ungroup() %>% transmute(id, g = TaxYr, hired1 = x)
  dd <- d %>% inner_join(ch, by = "id") %>% filter(TaxYr <= to) %>%
    mutate(yv = prep_y(suppressWarnings(as.numeric(.data[[y]])), tf), rel = TaxYr - g) %>% filter(!is.na(yv), abs(rel) <= WIN)
  if (n_distinct(dd$id[dd$hired1 == 1]) < 15) return(NULL)
  aca <- if (window) "mcaid_exp_win" else "mcaid_exp"
  m <- try(feols(as.formula(paste0("yv ~ i(rel, hired1, ref = -1) + post_sys_change + ", aca, " | id + TaxYr + rel")), data = dd, cluster = ~id), silent = TRUE)
  mp <- try(feols(as.formula(paste0("yv ~ hired1:I(rel >= 0) + post_sys_change + ", aca, " | id + TaxYr + rel")), data = dd, cluster = ~id), silent = TRUE)
  if (inherits(m, "try-error") || inherits(mp, "try-error")) return(NULL)
  ct <- as.data.frame(coeftable(m)); ct$term <- rownames(ct)
  es <- ct %>% filter(str_detect(term, "^rel::-?\\d+:hired1$")) %>% transmute(rel = as.integer(str_match(term, "^rel::(-?\\d+)")[, 2]), est = Estimate, se = `Std. Error`) %>%
    bind_rows(tibble(rel = -1L, est = 0, se = 0))
  pre <- es$rel[es$rel < -1]
  wt <- if (length(pre)) try(wald(m, keep = paste0("^rel::(", paste(pre, collapse = "|"), "):hired1$"), print = FALSE), silent = TRUE) else NULL
  r <- coeftable(mp); r <- r[str_detect(rownames(r), "hired1"), , drop = FALSE][1, ]
  sdp <- sd(dd$yv[dd$rel < 0])
  list(att = tibble(design = "D2 CEO change: hired type 1 vs type 0", attribute = label, outcome = ylab, est = r[[1]], se = r[[2]], p = r[[4]],
                    est_sd = r[[1]] / sdp, pretrend_p = wald_p(wt),
                    n_gain = n_distinct(dd$id[dd$hired1 == 1]), n_never = n_distinct(dd$id[dd$hired1 == 0]), N = nobs(m)),
       es = es %>% mutate(design = "D2", attribute = label, outcome = ylab, sd = sdp))
}

grid <- expand_grid(ATTRS, OUT %>% rename(ylab = label))
r1 <- pmap(grid, function(panel, var, label, ceo_change_var, y, ylab, tf, to, window) { message("D1 | ", label, " | ", ylab); d1_fit(panel, var, label, y, ylab, tf, to, window) })
r2 <- pmap(grid, function(panel, var, label, ceo_change_var, y, ylab, tf, to, window) { message("D2 | ", label, " | ", ylab); d2_fit(panel, var, label, ceo_change_var, y, ylab, tf, to, window) })
att <- bind_rows(map(c(r1, r2), "att")) %>% group_by(design, attribute) %>% mutate(q_bh = p.adjust(p, method = "BH")) %>% ungroup()
es  <- bind_rows(map(c(r1, r2), "es")) %>% mutate(lo = est - 1.96 * se, hi = est + 1.96 * se)
cat("\n================ Average effect after the change (D1: ATT; D2: hired type 1 minus type 0, after vs before) ================\n")
print(att %>% mutate(across(c(est, se, est_sd), ~ round(.x, 3)), across(c(p, q_bh, pretrend_p), ~ round(.x, 3))), width = Inf, n = 200)
write_csv(att, paste0(tab_path, "leadership_change_att.csv")); write_csv(es, paste0(tab_path, "leadership_change_es.csv"))

pdf(paste0(fig_path, "leadership_change_es.pdf"), width = 11, height = 8)
for (a in setdiff(unique(es$attribute), c("CMO in place (990)", "CMO-titled executive (HIMSS)"))) for (dz in unique(es$design)) {   # CMO: tables only
  x <- es %>% filter(attribute == a, design == dz); if (!nrow(x)) next
  print(ggplot(x, aes(rel, est / sd)) + geom_hline(yintercept = 0, linetype = 2) + geom_vline(xintercept = -0.5, linetype = 3) +
          geom_pointrange(aes(ymin = lo / sd, ymax = hi / sd), size = 0.25) + facet_wrap(~ outcome, ncol = 3) +
          scale_x_continuous(breaks = -WIN:WIN) +
          labs(title = paste(a, "-", if (dz == "D1") "gain vs never (Sun & Abraham)" else "CEO change: hired type 1 vs type 0"),
               x = "Years relative to the change (0 = first year with the new attribute)", y = "Effect, in SD of the pre-period outcome",
               caption = "Descriptive, not causal. Hospital and year FE; controls for system change and Medicaid expansion.") + theme_bw())
}
dev.off()

# =============================== H. SELECTION INTO HIRING (pre-specified 5 Nov before results; DECISIONS.md) ======================
# Question: are physician (clinical) CEOs hired BECAUSE readmissions were worse or getting worse?
# CEO changes and CEO type as in D2: HIMSS CEO change (h_ceo_change; CEO differs from last year's), new CEO's status from the combined
# measure without the 990 fallback (c_ceo_*_nofb in the change year). Change years 2009-2015.
# Performance known before the hire: all_read in the window ending June of t-1 (t = change year), standardised within window year
# across the panel's hospitals (z: + = more readmissions than peers that year). all_read stops at 2014 (pneumonia cohort change).
#   trend_z = z(t-1) - z(t-2) (= one year in minus one year out, /3, relative to peers); timing robustness uses z(t-2).
# Linear probability models, change-year FE, baseline covariates (COVS), SEs clustered by hospital; Panels A and B.
#   H0  descriptive: hospitals typed in 2010 (as analysis02) - share with a CEO change in 2009-10, and readmission z in the 2009 window
#   H1  turnover: P(CEO change in t) ~ z(t-1) [+ trend_z], all hospital-years                    (bad performance -> CEO replaced?)
#   H2  hire:     P(new CEO is physician | change in t) ~ z(t-1) [+ trend_z] + outgoing CEO physician
#   H3  H2 among changes where the outgoing CEO was NOT a physician (the gains that create type-1 hospitals)
#   H4  H2 with z(t-1) x post (change in 2012+): pre-policy hires (2009-11) are the ones that set the 2010 type
#   H5  H2 / H3 with z(t-2) instead of z(t-1) (HIMSS snapshot timing)
#   all repeated for clinical CEO (physician or nurse).
# Reading, set in advance: positive coefficients on z / trend_z in H2-H4 = physicians hired where readmissions were worse / worsening
# (selection on hiring), which could produce the pre-trend; zero with a tight CI argues against it. Physician hires are few: report CIs.
SEL_YEARS <- 2010:2015   # analysis02 windows start in 2009, so z(t-1) exists from 2010 hires
sel_build <- function(d, panel) {
  # Panel A on the main (agreement) definition (6 Nov): the c_ceo_* names below then carry the agreement versions
  if (panel == "A") d <- d %>% mutate(c_ceo_md_nofb = v_ceo_md_ann, c_ceo_clinical_nofb = v_ceo_clin_ann, c_ceo_md0 = v_ceo_md0, c_ceo_clinical0 = v_ceo_clinical0)
  d %>% distinct(id, TaxYr, .keep_all = TRUE) %>%
    select(id, TaxYr, all_read, h_ceo_change, c_ceo_md_nofb, c_ceo_clinical_nofb, c_ceo_md0, c_ceo_clinical0, all_of(COVS), any_of("fp")) %>%
    mutate(all_read = if_else(TaxYr <= 2014, all_read, NA_real_)) %>%
    group_by(TaxYr) %>% mutate(read_z = (all_read - mean(all_read, na.rm = TRUE)) / sd(all_read, na.rm = TRUE)) %>% ungroup() %>%
    arrange(id, TaxYr) %>% group_by(id) %>%
    mutate(z_l1 = read_z[match(TaxYr - 1, TaxYr)], z_l2 = read_z[match(TaxYr - 2, TaxYr)],
           md_prev = c_ceo_md_nofb[match(TaxYr - 1, TaxYr)], clin_prev = c_ceo_clinical_nofb[match(TaxYr - 1, TaxYr)]) %>% ungroup() %>%
    mutate(trend_z = z_l1 - z_l2, post_hire = as.integer(TaxYr >= 2012), panel = panel)
}
SA <- sel_build(A, "A"); SB <- sel_build(B, "B")
ctrl <- function(panel) paste(c(COVS, if (panel == "B") "fp"), collapse = " + ")

z_at <- function(z, yr, y0) { v <- z[yr == y0 & !is.na(z)]; if (length(v)) v[1] else NA_real_ }   # NA when that window is missing
# H0: the hospitals typed in 2010
cat("\n================ H0. CEO typed in 2010: new in 2009-10? readmission vs peers in the 2009 / 2011 windows (z) ================\n")
h0 <- map_dfr(list(SA, SB), function(S) map_dfr(c(md = "c_ceo_md0", clin = "c_ceo_clinical0"), function(tv) {
  S %>% group_by(id) %>% summarise(t = first(.data[[tv]]), new0910 = any(h_ceo_change %in% 1 & TaxYr %in% 2009:2010),
                                   chg_known = any(!is.na(h_ceo_change) & TaxYr %in% 2009:2010),
                                   z2009 = z_at(read_z, TaxYr, 2009), z2011 = z_at(read_z, TaxYr, 2011), panel = first(panel), .groups = "drop") %>%
    filter(!is.na(t), chg_known) %>% group_by(panel, t, new0910) %>%
    summarise(hospitals = n(), z2009 = mean(z2009, na.rm = TRUE), z2011 = mean(z2011, na.rm = TRUE), .groups = "drop") %>%
    group_by(panel, t) %>% mutate(share = hospitals / sum(hospitals)) %>% ungroup() %>% mutate(type = tv, .before = 1)
}))
print(h0 %>% mutate(across(where(is.double), ~ round(.x, 3))), width = Inf, n = 40)
write_csv(h0, paste0(tab_path, "leadership_selection_type2010.csv"))

sel_fit <- function(S, y, rhs, spec, filt = function(x) x) {
  panel <- S$panel[1]
  dd <- S %>% filter(TaxYr %in% SEL_YEARS) %>% filt() %>% filter(!is.na(.data[[y]]))
  m <- try(feols(as.formula(paste0(y, " ~ ", rhs, " + ", ctrl(panel), " | TaxYr")), data = dd, cluster = ~id), silent = TRUE)
  if (inherits(m, "try-error")) { message("sel_fit failed: ", spec); return(NULL) }
  ct <- coeftable(m); ct <- ct[str_detect(rownames(ct), "z_l1|z_l2|trend_z"), , drop = FALSE]
  used <- dd[obs(m), ]
  tibble(panel = panel, spec = spec, outcome = y, term = rownames(ct), est = ct[, 1], se = ct[, 2], p = ct[, 4],
         ci_lo = ct[, 1] - 1.96 * ct[, 2], ci_hi = ct[, 1] + 1.96 * ct[, 2], mean_y = mean(used[[y]]),
         n_events = sum(used[[y]] == 1), N = nobs(m), n_hosp = n_distinct(used$id))
}
SEL_ROLES <- tribble(~role, ~yv, ~prev,
                     "Physician CEO", "c_ceo_md_nofb",       "md_prev",
                     "Clinical CEO",  "c_ceo_clinical_nofb", "clin_prev")
chg  <- function(x) x %>% filter(h_ceo_change %in% 1)
sel <- bind_rows(map(list(SA, SB), function(S) bind_rows(
  sel_fit(S, "h_ceo_change", "z_l1", "H1 turnover ~ z(t-1)"),
  sel_fit(S, "h_ceo_change", "z_l1 + trend_z", "H1 turnover ~ z(t-1) + trend"),
  pmap_dfr(SEL_ROLES, function(role, yv, prev) bind_rows(
    sel_fit(S, yv, paste("z_l1 +", prev), "H2 hire ~ z(t-1)", chg),
    sel_fit(S, yv, paste("z_l1 + trend_z +", prev), "H2 hire ~ z(t-1) + trend", chg),
    sel_fit(S, yv, "z_l1", "H3 hire ~ z(t-1), outgoing CEO not type 1", function(x) chg(x) %>% filter(.data[[prev]] %in% 0)),
    sel_fit(S, yv, "z_l1 + trend_z", "H3 hire ~ z(t-1) + trend, outgoing CEO not type 1", function(x) chg(x) %>% filter(.data[[prev]] %in% 0)),
    sel_fit(S, yv, paste("z_l1 + z_l1:post_hire +", prev), "H4 hire ~ z(t-1) x post (z_l1 = 2009-11 hires)", chg),
    sel_fit(S, yv, paste("z_l2 +", prev), "H5 hire ~ z(t-2)", chg),
    sel_fit(S, yv, "z_l2", "H5 hire ~ z(t-2), outgoing CEO not type 1", function(x) chg(x) %>% filter(.data[[prev]] %in% 0))) %>%
      mutate(role = role, .before = 1))))) %>%
  mutate(role = coalesce(role, "Any CEO change"))
cat("\n================ H1-H5. Selection into hiring: readmission vs peers (z) before the change ================\n")
print(sel %>% mutate(across(c(est, se, ci_lo, ci_hi, mean_y), ~ round(.x, 3)), p = round(p, 3)) %>%
        select(panel, role, spec, term, est, se, p, ci_lo, ci_hi, mean_y, n_events, N), width = Inf, n = 100)
write_csv(sel, paste0(tab_path, "leadership_selection.csv"))