library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(purrr)
library(tibble)
library(ggplot2)
library(fixest)
library(WeightIt)

### Second and third HRRP "events": conditions added to the penalty after the original AMI / HF / pneumonia.
###   E2013  FY2014 IPPS final rule (2 Aug 2013): COPD and elective hip/knee (THA/TKA) penalised from FY2015, on discharges Jul 2010 - Jun 2013
###          (the first penalty window had already closed when the rule was announced, as in 2011).
###   E2014  FY2015 IPPS final rule (4 Aug 2014): CABG penalised from FY2017.
### Control condition: stroke, never in HRRP. Hospital-wide readmission is NOT a control (it contains the targeted conditions).
### Design - triple difference within hospital, on Hospital Compare 36-month windows ending June:
###   OVERALL   y_hct = g (target_c x pv_ct) + hospital x condition FE + hospital x year FE                     (all hospitals, unweighted)
###   BY TYPE   y_hct = b (target_c x pv_ct x type_h) + hospital x condition FE + hospital x year FE + condition x year FE
###             entropy-balancing weights as in analysis02 (type 0 reweighted to type 1), SEs clustered by hospital.
###   pv_ct = share of the window after the event (windows overlap by 2 of 3 years, so the first post window is only partly treated).
###   Hospital x year FE absorb everything that varies by hospital-year: Medicaid expansion, system changes, VBP, the original HRRP.
### Leadership types, covariates and weights come from analysis02 (run up to BUILD_DONE), so the samples and types are identical.
### Data limits (data00 audit, 2 Nov): COPD and stroke from the window ending 2013, hip/knee from 2012, CABG from 2014; no window ending 2016
###   for any readmission measure (2017 release file has 11 rows). So E2013 has ONE pre window (two for hip/knee alone) and no pre-trend
###   test for the triple difference; E2014 has one pre window. Anticipation: hip/knee dry-run reports went to hospitals in Sep 2012 (D1 below).
### Not run when written (no R in the sandbox).

source("./RScripts/paths.R")
A02_PATH <- "./RScripts/analysis02_response_by_type.R"     # where analysis02 lives
if (exists("BUILD_DONE")) rm(BUILD_DONE)
for (e in parse(A02_PATH, keep.source = FALSE)) { eval(e, envir = globalenv()); if (exists("BUILD_DONE")) break }
stopifnot(exists("BUILD_DONE"), exists("A"), exists("B"), exists("TYPES"), exists("get_w"))
if (!all(c("mvt_md0", "mvt_clin0") %in% names(A)))   # 6 Nov: catches an outdated analysis02 at A02_PATH
  stop("analysis02 at ", A02_PATH, " is an older version (no managing-vs-type groups). Replace it with the latest analysis02.")

fig_path <- "./Objects/"; tab_path <- "./Tables/"
CONDS <- c("copd", "hip_knee", "cabg", "stk")
need4 <- c(paste0(CONDS, "_readmission"), paste0(c("copd", "cabg", "stk"), "_mortality"))
if (length(miss <- setdiff(need4, names(A)))) stop("Not in hospital_data: ", paste(miss, collapse = ", "), " - apply patch_other_conditions.R and rebuild data00/data02")

EVENTS <- list(
  E2013 = list(label = "E2013: COPD + hip/knee added (rule 2 Aug 2013)", date = as.Date("2013-08-02"), targets = c("copd", "hip_knee"),
               years = 2012:2017, ref = 2013),
  E2014 = list(label = "E2014: CABG added (rule 4 Aug 2014)", date = as.Date("2014-08-04"), targets = "cabg", years = 2013:2017, ref = 2014))
DRY_RUN <- c(hip_knee = as.Date("2012-09-01"))   # D1: date hip/knee from the confidential dry-run reports
CONTROL <- "stk"

win_share <- function(yr, date) {           # share of the 36-month window ending 30 Jun yr that falls after `date`
  we <- as.Date(paste0(yr, "-06-30")); ws <- as.Date(paste0(yr - 3, "-07-01"))
  pmax(0, pmin(1, as.numeric(we - date) / as.numeric(we - ws)))
}
# long panel: one row per hospital x condition x window year
to_long <- function(d, ev, measure, targets = ev$targets, dates = NULL) {
  conds <- c(targets, CONTROL)
  d %>% filter(TaxYr %in% ev$years) %>% select(-any_of(c("pv", "tg"))) %>%
    pivot_longer(any_of(paste0(conds, "_", measure)), names_to = "cond", values_to = "y") %>%
    mutate(cond = str_remove(cond, paste0("_", measure, "$"))) %>% filter(!is.na(y)) %>%
    mutate(tg = as.integer(cond %in% targets),
           edate = if (is.null(dates)) ev$date else coalesce(unname(dates[cond]), ev$date),
           pv = win_share(TaxYr, edate))
}
pick <- function(m, vars) {                 # coefficient whose name is exactly these variables, in any order
  ct <- coeftable(m); hit <- which(map_lgl(strsplit(rownames(ct), ":"), ~ setequal(.x, vars)))
  if (length(hit) != 1) return(NULL); ct[hit, ]
}
row_out <- function(m, vars, ...) {
  r <- pick(m, vars); if (is.null(r)) return(tibble(..., note = "coefficient not found"))
  tibble(..., est = r[[1]], se = r[[2]], p = r[[4]], ci_lo = r[[1]] - 1.96 * r[[2]], ci_hi = r[[1]] + 1.96 * r[[2]], N = nobs(m), note = NA_character_)
}

# ---- coverage ---------------------------------------------------------------------------------------------------
cat("\n================ Hospitals with each measure, by window end year (Panel B) ================\n")
print(B %>% filter(TaxYr %in% 2011:2017) %>% group_by(TaxYr) %>% summarise(across(all_of(need4), ~ sum(!is.na(.x))), .groups = "drop"), width = Inf)

# ---- OVERALL: did the newly targeted conditions improve relative to stroke after they were added? (all hospitals) ---------------
overall <- list(); es_overall <- list(); raw <- list()
for (en in names(EVENTS)) for (measure in c("readmission", "mortality")) {
  ev <- EVENTS[[en]]; tsets <- c(list(pooled = ev$targets), setNames(as.list(ev$targets), ev$targets))
  tsets <- tsets[map_lgl(tsets, ~ all(paste0(.x, "_", measure) %in% names(B)))]
  if (length(ev$targets) == 1) tsets <- tsets[1]
  for (ts in names(tsets)) {
    L <- to_long(B, ev, measure, targets = tsets[[ts]])
    m <- feols(y ~ tg:pv | id^cond + id^TaxYr, data = L, cluster = ~id)
    overall[[length(overall) + 1]] <- row_out(m, c("tg", "pv"), event = en, measure = measure, targets = ts, spec = "MAIN share of window after event",
                                              hospitals = n_distinct(L$id))
    mb <- feols(y ~ tg:post | id^cond + id^TaxYr, data = L %>% mutate(post = as.integer(pv > 0)), cluster = ~id)
    overall[[length(overall) + 1]] <- row_out(mb, c("tg", "post"), event = en, measure = measure, targets = ts, spec = "P1 binary post",
                                              hospitals = n_distinct(L$id))
    # conditions have very different base rates (e.g. hip/knee ~5%, stroke ~13%, COPD ~20%), so pp and proportional changes can disagree
    ml <- feols(log(y) ~ tg:pv | id^cond + id^TaxYr, data = L %>% filter(y > 0), cluster = ~id)
    overall[[length(overall) + 1]] <- row_out(ml, c("tg", "pv"), event = en, measure = measure, targets = ts, spec = "L1 log outcome",
                                              hospitals = n_distinct(L$id))
    if (ts == names(tsets)[1]) {        # event study and raw means for the first target set (pooled, or COPD alone for E2013 mortality)
      me <- feols(as.formula(paste0("y ~ i(TaxYr, tg, ref = ", ev$ref, ") | id^cond + id^TaxYr")), data = L, cluster = ~id)   # fixest needs the ref value written in
      ct <- as.data.frame(coeftable(me)); ci <- as.data.frame(confint(me))
      es_overall[[length(es_overall) + 1]] <- tibble(term = rownames(ct), est = ct$Estimate, lo = ci[[1]], hi = ci[[2]]) %>%
        mutate(year = as.integer(str_match(term, "^TaxYr::(\\d{4}):tg$")[, 2])) %>% filter(!is.na(year)) %>% select(-term) %>%
        bind_rows(tibble(year = ev$ref, est = 0, lo = 0, hi = 0)) %>% mutate(event = ev$label, measure = measure)
      raw[[length(raw) + 1]] <- L %>% group_by(TaxYr, cond) %>% summarise(mean_y = mean(y), n = n(), .groups = "drop") %>% mutate(event = ev$label, measure = measure)
    }
  }
}
overall <- bind_rows(overall)
cat("\n================ OVERALL (all hospitals): targeted condition minus stroke, after vs before ================\n")
print(overall %>% mutate(across(c(est, se, ci_lo, ci_hi), ~ round(.x, 3)), p = round(p, 3)), width = Inf, n = 100)
write_csv(overall, paste0(tab_path, "added_conditions_overall.csv"))

# ---- BY TYPE: does the targeted-minus-stroke change differ by leadership type? ----------------------------------------------
fit_type <- function(L, w, spec, log_y = FALSE, weighted = TRUE) {
  L <- L %>% inner_join(w, by = "id") %>% mutate(w = if (weighted) w else 1, yv = if (log_y) ifelse(y > 0, log(y), NA_real_) else y) %>% filter(!is.na(yv))
  m <- try(feols(yv ~ tg:pv:t | id^cond + id^TaxYr + cond^TaxYr, data = L, weights = ~w, cluster = ~id), silent = TRUE)
  if (inherits(m, "try-error")) return(tibble(spec = spec, note = "model failed"))
  row_out(m, c("tg", "pv", "t"), spec = spec, n_type1 = n_distinct(L$id[L$t == 1]), n_type0 = n_distinct(L$id[L$t == 0]))
}
bytype <- list(); es_type <- list()
for (en in names(EVENTS)) for (measure in c("readmission", "mortality")) {
  ev <- EVENTS[[en]]; tsets <- c(list(pooled = ev$targets), setNames(as.list(ev$targets), ev$targets))
  tsets <- tsets[map_lgl(tsets, ~ all(paste0(.x, "_", measure) %in% names(B)))]
  if (length(ev$targets) == 1) tsets <- tsets[1]
  for (i in seq_len(nrow(TYPES))) {
    panel <- TYPES$panel[i]; type <- TYPES$type[i]; label <- TYPES$label[i]
    d <- (if (panel == "A") A else B) %>% filter(!is.na(.data[[type]])) %>% mutate(t = .data[[type]])
    w  <- get_w(d, "t", covs_for(panel)); w_c0 <- get_w(d, "t", setdiff(covs_for(panel), "cmi0"))
    if (is.null(w)) next
    for (ts in names(tsets)) {
      L <- to_long(d, ev, measure, targets = tsets[[ts]])
      rows <- list(
        fit_type(L, w, "MAIN share of window after event"),
        fit_type(L %>% mutate(pv = as.integer(pv > 0)), w, "P1 binary post"),
        fit_type(L %>% filter(TaxYr != ev$ref + 1), w, "P2 drop the first, partly post window"),
        fit_type(L, w, "L1 log outcome", log_y = TRUE),
        fit_type(L, w, "W0 unweighted", weighted = FALSE),
        if (!is.null(w_c0)) fit_type(L, w_c0, "C0 weights without baseline case mix"),
        if (en == "E2013" && "hip_knee" %in% tsets[[ts]]) fit_type(to_long(d, ev, measure, targets = tsets[[ts]], dates = DRY_RUN), w,
                                                                   "D1 hip/knee dated from the Sep 2012 dry run"))
      bytype[[length(bytype) + 1]] <- bind_rows(rows) %>% mutate(event = en, measure = measure, targets = ts, type = label, panel = panel, .before = 1)
      if (ts == names(tsets)[1]) {
        Lw <- L %>% inner_join(w, by = "id") %>% mutate(tg_t = tg * t)
        me <- try(feols(as.formula(paste0("y ~ i(TaxYr, tg_t, ref = ", ev$ref, ") | id^cond + id^TaxYr + cond^TaxYr")), data = Lw, weights = ~w, cluster = ~id), silent = TRUE)
        if (!inherits(me, "try-error")) {
          ct <- as.data.frame(coeftable(me)); ci <- as.data.frame(confint(me))
          es_type[[length(es_type) + 1]] <- tibble(term = rownames(ct), est = ct$Estimate, lo = ci[[1]], hi = ci[[2]]) %>%
            mutate(year = as.integer(str_match(term, "^TaxYr::(\\d{4}):tg_t$")[, 2])) %>% filter(!is.na(year)) %>% select(-term) %>%
            bind_rows(tibble(year = ev$ref, est = 0, lo = 0, hi = 0)) %>% mutate(event = ev$label, measure = measure, type = label)
        }
      }
    }
  }
}
bytype <- bind_rows(bytype) %>% group_by(event, measure, spec, type) %>% mutate(q_bh = p.adjust(p, method = "BH")) %>% ungroup()  # family: targets within type x spec
cat("\n================ BY TYPE (MAIN): type 1 minus type 0 in the targeted-minus-stroke change ================\n")
print(bytype %>% filter(spec == "MAIN share of window after event") %>% mutate(across(c(est, se, ci_lo, ci_hi), ~ round(.x, 3)), across(c(p, q_bh), ~ round(.x, 3))) %>%
        select(event, measure, targets, type, est, se, p, q_bh, n_type1, n_type0, N, note), width = Inf, n = 200)
write_csv(bytype, paste0(tab_path, "added_conditions_by_type.csv"))

# ---- MANAGING VS ORGANISATIONAL TYPE in the E2013 triple (item C; pre-specified 3 Nov, coded 6 Nov before results) ------------------
# Same groups as analysis02 (mvt_group(): AT = clinical / physician CEO at the 2011 rule, BEFORE, AFTER, NEVER = reference), all in one
# model: y ~ i(group, targeted x share after the event) | hospital x condition + hospital x year + condition x year, unweighted, with
# baseline covariates x targeted x post, clustered by hospital. Note for E2013 (Aug 2013): AFTER = clinical CEO only from 2013 on, so
# many AFTER hospitals have the clinical leader in post at THIS event - AFTER responding here but not in 2011 also points to managing.
mvt4 <- list()
for (measure in c("readmission", "mortality")) {
  ev <- EVENTS$E2013; tg_m <- ev$targets[paste0(ev$targets, "_", measure) %in% names(B)]
  if (!length(tg_m)) next
  for (i in seq_len(nrow(MVT_TYPES))) for (pooled in c(FALSE, TRUE)) {   # pooled = BY (AT + BEFORE) / AFTER, added 6 Nov after the analysis02 results
    panel <- MVT_TYPES$panel[i]; gvar <- MVT_TYPES$gvar[i]; label <- MVT_TYPES$label[i]; sp <- mvt_spec(pooled)
    d <- (if (panel == "A") A else B) %>% filter(!is.na(.data[[gvar]]))
    L <- to_long(d, ev, measure, targets = tg_m) %>%
      mutate(g = factor(if (pooled) unname(MVT_POOL[.data[[gvar]]]) else .data[[gvar]], levels = sp$lv), tpv = tg * pv)
    m <- try(feols(as.formula(paste0("y ~ i(g, tpv, ref = 'NEVER') + tpv:(", paste(covs_for(panel), collapse = " + "),
                                     ") | id^cond + id^TaxYr + cond^TaxYr")), data = L, cluster = ~id), silent = TRUE)
    if (inherits(m, "try-error")) { message("mvt4 failed: ", label, " | ", measure); next }
    b <- coef(m); V <- vcov(m); nm <- function(g) paste0("g::", g, ":tpv")
    rows <- map_dfr(sp$lv[-1], function(g) if (nm(g) %in% names(b)) tibble(term = paste(g, "vs NEVER"), est = b[[nm(g)]], se = sqrt(V[nm(g), nm(g)])))
    rows <- bind_rows(rows, map_dfr(sp$contr, function(cc) { k <- nm(cc)
    if (!all(k %in% names(b))) return(NULL)
    tibble(term = paste(cc[1], "minus", cc[2]), est = b[[k[1]]] - b[[k[2]]], se = sqrt(V[k[1], k[1]] + V[k[2], k[2]] - 2 * V[k[1], k[2]])) }))
    n_g <- L %>% distinct(id, g) %>% count(g)
    mvt4[[length(mvt4) + 1]] <- rows %>% mutate(p = 2 * pnorm(-abs(est / se)), ci_lo = est - 1.96 * se, ci_hi = est + 1.96 * se,
                                                grouping = if (pooled) "pooled: BY (AT + BEFORE) / AFTER" else "four groups (pre-specified)",
                                                event = "E2013", measure = measure, targets = paste(tg_m, collapse = "+"), panel = panel, type = label,
                                                n_by_group = paste(paste0(n_g$g, "=", n_g$n), collapse = ", "), N = nobs(m), .before = 1)
  }
}
mvt4 <- bind_rows(mvt4)
cat("\n================ MANAGING VS TYPE, E2013 triple (targeted minus stroke, group vs NEVER) ================\n")
print(mvt4 %>% mutate(across(c(est, se, ci_lo, ci_hi), ~ round(.x, 3)), p = round(p, 3)) %>%
        select(measure, targets, type, grouping, term, est, se, p, n_by_group), width = Inf, n = 200)
write_csv(mvt4, paste0(tab_path, "added_conditions_managing.csv"))

# ---- hip/knee on its own: two pre windows (2012, 2013), so one pre-trend coefficient by type (no stroke control) --------------
hk <- pmap_dfr(TYPES, function(panel, type, label) {
  d <- (if (panel == "A") A else B) %>% filter(TaxYr %in% 2012:2017, !is.na(hip_knee_readmission), !is.na(.data[[type]])) %>%
    mutate(t = .data[[type]], yv = hip_knee_readmission)
  w <- get_w(d, "t", covs_for(panel)); if (is.null(w)) return(NULL)
  m <- feols(yv ~ i(TaxYr, t, ref = 2013) + post_sys_change + mcaid_exp_win | id + TaxYr, data = d %>% inner_join(w, by = "id"), weights = ~w, cluster = ~id)
  ct <- as.data.frame(coeftable(m)); ct %>% mutate(term = rownames(ct)) %>% filter(str_detect(term, "^TaxYr::")) %>%
    transmute(type = label, year = as.integer(str_match(term, "\\d{4}")[, 1]), est = Estimate, se = `Std. Error`, p = `Pr(>|t|)`)
})
cat("\n================ Hip/knee readmission alone, type 1 minus type 0 relative to the window ending 2013 (2012 = pre-trend) ================\n")
hk <- as_tibble(hk)   # built from coeftable data frames; a plain data.frame cannot take print(n = )
print(hk %>% mutate(across(c(est, se, p), ~ round(.x, 3))), n = 100)
write_csv(hk, paste0(tab_path, "added_conditions_hipknee_es.csv"))

# ---- figures ------------------------------------------------------------------------------------------------------
raw <- bind_rows(raw)
p0 <- ggplot(raw, aes(TaxYr, mean_y, colour = cond)) + geom_line() + geom_point() + facet_wrap(event ~ measure, scales = "free_y", ncol = 2) +
  labs(x = "Year the 36-month window ends (June)", y = "Unweighted mean rate (%)", colour = NULL,
       caption = "Hospital Compare; no window ending 2016 for readmission measures.") + theme_bw()
ggsave(paste0(fig_path, "added_conditions_raw.pdf"), p0, width = 10, height = 7); print(p0)
es_overall <- bind_rows(es_overall)
p1 <- ggplot(es_overall, aes(year, est)) + geom_hline(yintercept = 0, linetype = 2) + geom_pointrange(aes(ymin = lo, ymax = hi), size = 0.3) +
  facet_wrap(event ~ measure, scales = "free_y", ncol = 2) +
  labs(x = "Year the 36-month window ends (June)", y = "Targeted condition minus stroke, relative to the last pre window",
       caption = "All hospitals; hospital x condition and hospital x year FE.") + theme_bw()
ggsave(paste0(fig_path, "added_conditions_es_overall.pdf"), p1, width = 10, height = 7); print(p1)
es_type <- bind_rows(es_type)
for (en in unique(es_type$event)) {
  p2 <- ggplot(es_type %>% filter(event == en, type %in% FIG_LABELS), aes(year, est)) + geom_hline(yintercept = 0, linetype = 2) +
    geom_pointrange(aes(ymin = lo, ymax = hi), size = 0.3) + facet_grid(type ~ measure, scales = "free_y") +
    labs(x = "Year the 36-month window ends (June)", y = "Type 1 minus type 0: targeted minus stroke, relative to the last pre window",
         title = en, caption = "Entropy-balanced as in analysis02; hospital x condition, hospital x year and condition x year FE.") +
    theme_bw() + theme(strip.text.y = element_text(angle = 0))
  ggsave(paste0(fig_path, "added_conditions_es_by_type_", str_extract(en, "^E\\d{4}"), ".pdf"), p2, width = 10, height = 14); print(p2)
}