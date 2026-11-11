library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(purrr)
library(tibble)
library(ggplot2)
library(fixest)
library(knitr)

source("./RScripts/paths.R")

### Did HRRP exposure change hospital leadership?   (pre-specified plan: DECISIONS.md, 2 Nov)
###
### Model, one regression per outcome:   y_it = b * (exposure_i x post_it) + controls + hospital FE + year FE,  SEs clustered by hospital
###   exposure_i = pre-policy excess readmission ratio above 1 (0 if not above 1), in SD units of the panel's hospitals
###   controls   = baseline log(beds) x year, baseline margin rank x year, baseline Medicare-share rank x year, post_sys_change (AHA system change)
###   general medical/surgical hospitals only (judged 2009-2011)
### PANEL A  nonprofits, 990 measures, 2009-2019.  post = first fiscal year BEGINNING on/after the 1 Aug 2011 final rule (fy_post_rule;
###          identical to TaxYr >= 2012 in every hospital-year, so R1 "TaxYr labels" is the same regression and is not repeated)
###          + (11 Nov) the agreement CEO definition (main treatment definition) as outcomes: physician / clinical CEO and gains, 2009-2016
### PANEL B  all general hospitals (nonprofit + for-profit), HIMSS CEO measures, 2009-2016.  post = HIMSS year >= 2012.  Hospital = CCN.
### ROBUSTNESS  R2 share of the fiscal year after the rule | R3 drop the straddling fiscal year | R4 penalty date (Oct 2012) |
###             (a) drop hospitals with any system change 2009-2016 | R5 HIMSS shifted +1 for June year-ends | R6 990 vs HIMSS dating of CEO outcomes |
###             R7 event study in two-year bins (figure).   R8 (hand-dated sample) is parked.
### Not run when written (no R in the sandbox).

# ---- settings ---------------------------------------------------------------------------------------------------
REF_YEAR <- 2011
LAST_990 <- 2019; LAST_HIMSS <- 2016
fig_path <- "./Objects/"; tab_path <- "./Tables/"; dir.create(fig_path, showWarnings = FALSE); dir.create(tab_path, showWarnings = FALSE)
setFixest_notes(FALSE)

hd <- read_rds(paste0(created_data_path, "/hospital_data.rds"))
need <- c("Filer.EIN", "TaxYr", "in_leadership_data", "MCRNUM", "general", "err_wmean_pre", "bed_num", "oper_margin", "mcare_discharges", "tot_discharges",
          "exec_team_size", "ceo_entry", "cmo_entry", "has_cmo", "exec_stability", "n_md_exec", "ceo_md_gain", "ceo_md",
          "fy_post_rule", "fy_share_post_rule", "fy_straddle_rule", "fy_post_penalty", "fye_month", "post_sys_change", "ever_sys_change_0916",
          "h_ceo_change", "h_ceo_md", "h_ceo_change_r5", "h_ceo_md_r5", "ceo_clinical", "h_ceo_same_990")
if (length(miss <- setdiff(need, names(hd)))) stop("Not in hospital_data (run the data02 blocks): ", paste(miss, collapse = ", "))

lag_adj  <- function(x, yr) ifelse(!is.na(dplyr::lag(yr))  & yr - dplyr::lag(yr)  == 1, dplyr::lag(x),  NA)
lead_adj <- function(x, yr) ifelse(!is.na(dplyr::lead(yr)) & dplyr::lead(yr) - yr == 1, dplyr::lead(x), NA)
ctrl <- " + i(TaxYr, lbeds0, ref = REF_YEAR) + i(TaxYr, margin_r, ref = REF_YEAR) + i(TaxYr, mcare_r, ref = REF_YEAR) + post_sys_change"

# baseline characteristics (fixed, 2009-2011), sample filters, and exposure in SD units of THIS panel's hospitals
add_baseline <- function(df) {
  b <- df %>% group_by(id) %>% summarise(
    exposure = { x <- pmax(err_wmean_pre - 1, 0); x <- x[!is.na(x)]; if (length(x)) x[1] else NA_real_ },
    lbeds0   = { v <- mean(bed_num[TaxYr %in% 2009:2011], na.rm = TRUE); if (!is.finite(v)) v <- mean(bed_num[TaxYr %in% 2012:2014], na.rm = TRUE)
    if (is.finite(v) && v > 0) log(v) else NA_real_ },
    margin0  = { x <- oper_margin[which(TaxYr %in% 2009:2011 & is.finite(oper_margin))]; if (length(x)) mean(x) else NA_real_ },
    mcare0   = { x <- (mcare_discharges / tot_discharges)[which(TaxYr %in% 2009:2011 & tot_discharges > 0)]; x <- x[is.finite(x)]; if (length(x)) mean(x) else NA_real_ },
    general0 = any(general[TaxYr %in% 2009:2011] %in% 1),
    ever_chg = any(ever_sys_change_0916 %in% 1), .groups = "drop") %>%
    mutate(margin_r = percent_rank(margin0), mcare_r = percent_rank(mcare0))
  out <- df %>% left_join(b, by = "id") %>% filter(general0, !is.na(lbeds0), !is.na(margin_r), !is.na(mcare_r), !is.na(exposure))
  he  <- out %>% distinct(id, exposure)
  out %>% mutate(exposure_sd = (exposure - mean(he$exposure)) / sd(he$exposure), post_sys_change = coalesce(post_sys_change, 0L))
}

# ---- PANEL A: nonprofits, 990 measures ------------------------------------------------------------------------------
A <- hd %>% filter(in_leadership_data, !is.na(Filer.EIN), TaxYr <= LAST_990) %>% mutate(id = as.character(Filer.EIN)) %>%
  group_by(id) %>% arrange(TaxYr, .by_group = TRUE) %>%
  mutate(has_md = as.integer(n_md_exec > 0),
         cmo_lag = lag_adj(has_cmo, TaxYr), cmo_lead = lead_adj(has_cmo, TaxYr), cmo_ok = !is.na(cmo_lag) & !is.na(cmo_lead),
         cmo_added    = if_else(cmo_ok, as.integer(cmo_lag == 0 & has_cmo == 1 & cmo_lead == 1), NA_integer_),
         cmo_replaced = if_else(cmo_ok, as.integer(cmo_lag == 1 & has_cmo == 1 & coalesce(cmo_entry, 0L) == 1), NA_integer_),
         ceo_not_md_last_yr = lag_adj(ceo_md, TaxYr) == 0,
         h_not_md_last    = lag_adj(h_ceo_md, TaxYr) == 0,    h_ceo_md_gain    = if_else(h_not_md_last %in% TRUE, as.integer(h_ceo_md == 1), NA_integer_),
         h_not_md_last_r5 = lag_adj(h_ceo_md_r5, TaxYr) == 0, h_ceo_md_gain_r5 = if_else(h_not_md_last_r5 %in% TRUE, as.integer(h_ceo_md_r5 == 1), NA_integer_),
         post_h = as.integer(TaxYr >= 2012),
         # 11 Nov: agreement definition (main CEO definition, 6 Nov) as an outcome - 990 status where the 990 CEO = the HIMSS CEO that year
         # (NA where they disagree or HIMSS is missing, so 2009-2016 only)
         v_ceo_md_ann   = ifelse(h_ceo_same_990 %in% 1, ceo_md, NA),
         v_ceo_clin_ann = ifelse(h_ceo_same_990 %in% 1, ceo_clinical, NA),
         v_not_md_last   = lag_adj(v_ceo_md_ann, TaxYr) == 0,   v_ceo_md_gain   = if_else(v_not_md_last %in% TRUE,   as.integer(v_ceo_md_ann == 1),   NA_integer_),
         v_not_clin_last = lag_adj(v_ceo_clin_ann, TaxYr) == 0, v_ceo_clin_gain = if_else(v_not_clin_last %in% TRUE, as.integer(v_ceo_clin_ann == 1), NA_integer_)) %>%
  ungroup() %>% select(-cmo_ok) %>% add_baseline()

# ---- PANEL B: all general hospitals, HIMSS CEO measures (hospital = CCN) ----------------------------------------------
B <- hd %>% filter(!is.na(MCRNUM), TaxYr %in% 2008:LAST_HIMSS) %>% group_by(MCRNUM, TaxYr) %>% slice(1) %>% ungroup() %>%
  mutate(id = as.character(MCRNUM), owner = if_else(in_leadership_data, "990 filer", "for-profit"), fp = as.integer(owner == "for-profit")) %>%
  group_by(id) %>% arrange(TaxYr, .by_group = TRUE) %>%
  mutate(owner = names(sort(table(owner), decreasing = TRUE))[1], fp = as.integer(owner == "for-profit"),     # ownership fixed per hospital (modal)
         h_not_md_last = lag_adj(h_ceo_md, TaxYr) == 0, h_ceo_md_gain = if_else(h_not_md_last %in% TRUE, as.integer(h_ceo_md == 1), NA_integer_),
         post_h = as.integer(TaxYr >= 2012)) %>%
  ungroup() %>% add_baseline()

cat("PANEL A hospitals:", n_distinct(A$id), "| exposure = 0:", n_distinct(A$id[A$exposure == 0]), "| any system change 2009-16:", n_distinct(A$id[A$ever_chg]), "\n")
cat("PANEL B hospitals by ownership:\n"); print(B %>% distinct(id, owner, exposure, ever_chg) %>% group_by(owner) %>%
                                                  summarise(hospitals = n(), exposure_zero = mean(exposure == 0), any_system_change = mean(ever_chg)))

# ---- estimation helpers ---------------------------------------------------------------------------------------------
fit <- function(dd, y, treat, label, spec, panel) {
  dd <- dd %>% filter(!is.na(.data[[y]]), !is.na(.data[[treat]])) %>% mutate(yv = .data[[y]], tr = exposure_sd * .data[[treat]])
  if (nrow(dd) == 0 || n_distinct(dd$yv) < 2) return(NULL)
  m  <- feols(as.formula(paste0("yv ~ tr", ctrl, " | id + TaxYr")), data = dd, cluster = ~id)
  r  <- coeftable(m)["tr", ]; mp <- mean(dd$yv[dd$TaxYr < 2012])
  tibble(panel = panel, Outcome = label, spec = spec, mean_pre = mp, est = r[[1]], se = r[[2]], p = r[[4]],
         ci_lo = r[[1]] - 1.96 * r[[2]], ci_hi = r[[1]] + 1.96 * r[[2]], pct_mean = 100 * r[[1]] / mp,
         ci_pct_mean = sprintf("[%.0f%%, %.0f%%]", 100 * (r[[1]] - 1.96 * r[[2]]) / mp, 100 * (r[[1]] + 1.96 * r[[2]]) / mp),
         N = nobs(m), Hospitals = n_distinct(dd$id))
}
es_fit <- function(dd, y, label, binned = FALSE) {
  dd <- dd %>% filter(!is.na(.data[[y]])) %>% mutate(yv = .data[[y]],
                                                     tb = if (binned) case_when(TaxYr <= 2010 ~ 2009L, TaxYr == 2011 ~ 2011L, TRUE ~ as.integer(2012 + 2 * ((TaxYr - 2012) %/% 2))) else as.integer(TaxYr))
  m  <- feols(as.formula(paste0("yv ~ i(tb, exposure_sd, ref = 2011)", ctrl, " | id + TaxYr")), data = dd, cluster = ~id)
  ct <- as.data.frame(coeftable(m)); ci <- as.data.frame(confint(m))
  pre <- sort(setdiff(unique(dd$tb[dd$tb < 2011]), 2011))
  w <- if (length(pre)) try(wald(m, keep = paste0("^tb::(", paste(pre, collapse = "|"), "):exposure_sd"), print = FALSE), silent = TRUE) else NULL
  tibble(term = rownames(ct), est = ct$Estimate, lo = ci[[1]], hi = ci[[2]]) %>%
    mutate(year = as.integer(str_match(term, "^tb::(\\d{4}):exposure_sd")[, 2])) %>% filter(!is.na(year)) %>% select(-term) %>%
    bind_rows(tibble(year = 2011L, est = 0, lo = 0, hi = 0)) %>%
    mutate(Outcome = label, pretrend_p = if (is.null(w) || inherits(w, "try-error")) NA_real_ else w$p)
}
# renamed 11 Nov from sub(): the old name masked base::sub() for everything run later in the same session
sub_years <- function(dd, from, to, flag = NA) { dd <- dd %>% filter(TaxYr >= from, TaxYr <= to); if (!is.na(flag)) dd <- dd %>% filter(.data[[flag]] %in% TRUE); dd }

# ---- PANEL A: main + robustness ---------------------------------------------------------------------------------------
specsA <- tribble(
  ~y,               ~label,                       ~from, ~flag,
  "exec_team_size", "Number of executives",       2009,  NA_character_,
  "ceo_entry",      "CEO replaced",               2010,  NA_character_,
  "cmo_replaced",   "CMO replaced",               2010,  NA_character_,
  "cmo_added",      "CMO added",                  2010,  NA_character_,
  "exec_stability", "Executive team stability",   2010,  NA_character_,
  "has_md",         "Has a physician executive",  2009,  NA_character_,
  "ceo_md_gain",    "CEO becomes a physician",    2010,  "ceo_not_md_last_yr",
  # 11 Nov: the treatment definition itself (agreement: 990 CEO = HIMSS CEO), 2009-2016
  "v_ceo_md_ann",   "Physician CEO (agreement)",            2009, NA_character_,
  "v_ceo_clin_ann", "Clinical CEO (agreement)",             2009, NA_character_,
  "v_ceo_md_gain",  "CEO becomes a physician (agreement)",  2010, "v_not_md_last",
  "v_ceo_clin_gain","CEO becomes clinical (agreement)",     2010, "v_not_clin_last")

resA <- pmap_dfr(specsA, function(y, label, from, flag) {
  dd <- sub_years(A, from, LAST_990, flag)
  bind_rows(fit(dd, y, "fy_post_rule", label, "MAIN", "A"),
            fit(dd, y, "fy_share_post_rule", label, "R2 share of FY after rule", "A"),
            fit(dd %>% filter(fy_straddle_rule %in% 0), y, "fy_post_rule", label, "R3 drop straddling FY", "A"),
            fit(dd, y, "fy_post_penalty", label, "R4 penalty date (Oct 2012)", "A"),
            fit(dd %>% filter(!ever_chg), y, "fy_post_rule", label, "(a) no system change 2009-16", "A"))
})
esA  <- pmap_dfr(specsA, function(y, label, from, flag) es_fit(sub_years(A, from, LAST_990, flag), y, label))
esA7 <- pmap_dfr(specsA, function(y, label, from, flag) es_fit(sub_years(A, from, LAST_990, flag), y, label, binned = TRUE))

# R5 / R6: nonprofit CEO outcomes dated by the 990 vs by HIMSS, same window (2010-2016)
specs6 <- tribble(
  ~y,                 ~label,                                   ~flag,               ~treat,
  "ceo_entry",        "CEO replaced: 990",                      NA_character_,       "fy_post_rule",
  "h_ceo_change",     "CEO replaced: HIMSS",                    NA_character_,       "post_h",
  "h_ceo_change_r5",  "CEO replaced: HIMSS, R5 shift",          NA_character_,       "post_h",
  "ceo_md_gain",      "CEO becomes a physician: 990",           "ceo_not_md_last_yr", "fy_post_rule",
  "h_ceo_md_gain",    "CEO becomes a physician: HIMSS",         "h_not_md_last",      "post_h",
  "h_ceo_md_gain_r5", "CEO becomes a physician: HIMSS, R5 shift", "h_not_md_last_r5",  "post_h")
res6 <- pmap_dfr(specs6, function(y, label, flag, treat) fit(sub_years(A, 2010, LAST_HIMSS, flag), y, treat, label, "R5/R6 nonprofits 2010-16", "A"))

# ---- PANEL B: pooled ownership (HIMSS) ----------------------------------------------------------------------------------
specsB <- tribble(
  ~y,              ~label,                               ~from, ~flag,
  "h_ceo_change",  "CEO replaced (HIMSS)",               2009,  NA_character_,
  "h_ceo_md",      "Physician CEO (HIMSS)",              2008,  NA_character_,
  "h_ceo_md_gain", "CEO becomes a physician (HIMSS)",    2009,  "h_not_md_last")
diff_fit <- function(dd, y, label) {            # does the exposure effect differ for for-profits?  (ownership-specific year effects)
  dd <- dd %>% filter(!is.na(.data[[y]])) %>% mutate(yv = .data[[y]], tr = exposure_sd * post_h)
  m  <- feols(as.formula(paste0("yv ~ tr + tr:fp", ctrl, " | id + TaxYr^fp")), data = dd, cluster = ~id)
  r  <- coeftable(m)["tr:fp", ]
  tibble(panel = "B", Outcome = label, spec = "for-profit minus nonprofit", mean_pre = mean(dd$yv[dd$TaxYr < 2012 & dd$fp == 1]),
         est = r[[1]], se = r[[2]], p = r[[4]], ci_lo = r[[1]] - 1.96 * r[[2]], ci_hi = r[[1]] + 1.96 * r[[2]], N = nobs(m), Hospitals = n_distinct(dd$id))
}
resB <- pmap_dfr(specsB, function(y, label, from, flag) {
  dd <- sub_years(B, from, LAST_HIMSS, flag)
  bind_rows(fit(dd, y, "post_h", label, "MAIN", "B"),
            fit(dd %>% filter(fp == 0), y, "post_h", label, "nonprofits only", "B"),
            fit(dd %>% filter(fp == 1), y, "post_h", label, "for-profits only", "B"),
            diff_fit(dd, y, label),
            fit(dd %>% filter(!ever_chg), y, "post_h", label, "(a) no system change 2009-16", "B"))
})
esB <- pmap_dfr(specsB, function(y, label, from, flag) es_fit(sub_years(B, from, LAST_HIMSS, flag), y, label))

# ---- tables ------------------------------------------------------------------------------------------------------------
pre_p <- bind_rows(esA %>% mutate(panel = "A"), esB %>% mutate(panel = "B")) %>% distinct(panel, Outcome, pretrend_p)
main  <- bind_rows(resA, resB) %>% filter(spec == "MAIN") %>% left_join(pre_p, by = c("panel", "Outcome"))
robust <- bind_rows(resA, res6, resB) %>% filter(spec != "MAIN")
fmt <- function(t) t %>% mutate(across(c(mean_pre, est, se, ci_lo, ci_hi), ~ round(.x, 4)), across(any_of(c("p", "pretrend_p")), ~ round(.x, 3)),
                                across(any_of("pct_mean"), ~ round(.x, 1)))
cat("\n================ MAIN (A: nonprofits, 990, fiscal-date post; B: all general hospitals, HIMSS) ================\n"); print(fmt(main), width = Inf)
cat("\n================ ROBUSTNESS ================\n"); print(fmt(robust) %>% arrange(panel, Outcome, spec), width = Inf, n = 100)
write_csv(main, paste0(tab_path, "did_leadership_main.csv")); write_csv(robust, paste0(tab_path, "did_leadership_robustness.csv"))
writeLines(as.character(kable(fmt(main) %>% select(panel, Outcome, mean_pre, est, se, p, ci_pct_mean, pretrend_p, N, Hospitals),
                              format = "latex", booktabs = TRUE)), paste0(tab_path, "did_leadership_main.tex"))

# ---- figures -----------------------------------------------------------------------------------------------------------
es_plot <- function(es, levels, file, cap) {
  es <- es %>% mutate(Outcome = factor(Outcome, levels = levels))
  p <- ggplot(es, aes(year, est)) + geom_hline(yintercept = 0, linetype = 2) + geom_vline(xintercept = 2011.5, linetype = 3) +
    geom_pointrange(aes(ymin = lo, ymax = hi), size = 0.3) + facet_wrap(~ Outcome, scales = "free_y", ncol = 2) +
    scale_x_continuous(breaks = sort(unique(es$year))) +
    labs(x = NULL, y = "Effect of +1 SD pre-policy exposure (95% CI)", caption = cap) +
    theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  ggsave(paste0(fig_path, file), p, width = 10, height = 2.6 * ceiling(length(levels) / 2) + 1); print(p)
}
es_plot(esA,  specsA$label, "es_leadership_990.pdf",      "Panel A: nonprofits, 990 measures. Reference 2011; hospital and year FE; controls incl. system change.")
es_plot(esA7, specsA$label, "es_leadership_990_bins.pdf", "R7: two-year bins (2009-10, 2012-13, ...); reference 2011.")
es_plot(esB,  specsB$label, "es_leadership_himss.pdf",    "Panel B: all general hospitals incl. for-profits, HIMSS CEO measures, 2008-2016. Reference 2011.")