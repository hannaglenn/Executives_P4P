library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(purrr)
library(stringr)

source("./RScripts/paths.R")

### HIMSS: one script for everything (replaces himss01-himss05).
###   PART 1  stack the yearly HIMSS tables (Contact, HAEntityContact, ContactSource, HAEntity), 2008-2016
###   PART 2  hospital contacts: harmonised executive slots, CMO-like titles, credentials, a person id across years
###   PART 3  physicians among executives: HIMSS MD/DO credential, or linked to a 990 executive with your verified doctor flag
###           (NPI name matching was tried and dropped: among executives without an MD credential only ~0.2% are physicians, so name matches were ~99% false)
###   PART 4  hospital-year leadership variables for ALL general hospitals (990 filers and for-profits)
###   PART 5  validation diagnostics against the 990s (one text file)
### Decisions (see DECISIONS.md): HIMSS year = TaxYr; general medical/surgical hospitals only; Medical Staff Chief is NOT an executive
### (it is ~100% MD, the elected chief of staff or sometimes the CMO), so it is excluded from the has_md proxies; CMOs exist only via free-text Title.
### Outputs (CreatedData): himss_hospital_year.rds, himss_persons.rds, himss_hospital_contacts.rds, himss_diagnostics.txt, himss_timing.txt (+ sample),
###   PART 7: himss_ceo_combined.rds, ceo_combined_diagnostics.txt, ceo_handcheck.csv (combined CEO measure + hand-check list).
### Phone and email are never kept.   Not run when written (no R in the sandbox).

# ---- settings -------------------------------------------------------------------------------------------------
HIMSS_DIR       <- file.path(raw_data_path, "HIMSS")
YEARS           <- 2008:2016          # Contact files exist 2008-2016 (2008 has no person UniqueID)
RUN_DIAGNOSTICS <- TRUE

# ---- helpers --------------------------------------------------------------------------------------------------
pad_ccn6 <- function(x) { x <- str_trim(as.character(x)); x[x == ""] <- NA; str_pad(x, 6, pad = "0") }
first_nn <- function(x) { x <- x[!is.na(x)]; if (length(x)) x[[1]] else NA }   # first non-missing value; always length 1 (another package masks dplyr::first)
md_regex <- "\\bM\\.?D\\b|\\bD\\.?O\\b"
read_tab <- function(yr, name) {
  f <- list.files(file.path(HIMSS_DIR, paste0("HIMSS", yr)), pattern = paste0("^(", yr, ")?", name, "\\.xlsx$"), full.names = TRUE, ignore.case = TRUE)
  if (!length(f)) { warning("No ", name, " file for ", yr); return(NULL) }
  read_excel(f[1], sheet = 1, col_types = "text") %>% mutate(himss_year = yr)
}
exec_slot <- function(r) case_when(str_detect(r, "^CEO") ~ "CEO", str_detect(r, "^CFO") ~ "CFO", str_detect(r, "^COO") ~ "COO",
                                   str_detect(r, "^CIO$|^CIO:") ~ "CIO", r == "Chief Nursing Head" ~ "CNO",
                                   r == "Chief Medical Information Officer" ~ "CMIO", r == "Medical Staff Chief" ~ "MSC", TRUE ~ "other")
title_class <- function(t) {
  t <- str_to_lower(coalesce(t, ""))
  case_when(str_detect(t, "chief medical officer|\\bcmo\\b|chief medical off") ~ "1 CMO",
            str_detect(t, "(\\bvp\\b|vice president|\\bsvp\\b|\\bevp\\b).{0,25}(medical affairs|medical staff affairs|medical operations|physician)") ~ "2 VP medical affairs",
            str_detect(t, "medical affairs") ~ "3 other medical affairs",
            str_detect(t, "chief clinical officer|chief physician|chief quality officer|chief medical quality") ~ "4 chief clinical / quality",
            str_detect(t, "^\\s*(hospital |facility )?medical director\\s*$") ~ "5 medical director (hospital-wide)",
            str_detect(t, "chief of staff|chief of the medical staff|president of the medical staff|medical staff president") ~ "6 chief of staff (elected)",
            TRUE ~ NA_character_)
}
EXEC_SLOTS <- c("CEO", "CFO", "COO", "CIO", "CNO", "CMIO")
MD_SLOTS   <- c("CEO", "CFO", "COO", "CIO", "CNO")          # slots used for has_md: CMIO left out (the slot only exists from 2015 -> level break)
CMO_CLASS  <- c("1 CMO", "2 VP medical affairs")

# order-free name keys (990 names come in both orders): initial of one token + another token
drop_tok <- c("jr", "sr", "ii", "iii", "iv", "md", "do", "phd", "rn", "mba", "fache", "mph", "msn", "bsn", "cpa", "esq", "dr", "mr", "mrs", "ms", "facs", "facp", "mha", "jd")
toks_of <- function(x) map(str_split(str_squish(str_replace_all(str_to_lower(coalesce(x, "")), "[^a-z ]", " ")), " "), ~ unique(.x[.x != "" & !.x %in% drop_tok]))
cand_keys <- function(tk) { if (length(tk) < 2) return(character(0)); g <- expand.grid(i = seq_along(tk), j = seq_along(tk)); g <- g[g$i != g$j, ]; unique(paste(substr(tk[g$j], 1, 1), tk[g$i])) }
himss_keys <- function(fn, ln) map2(toks_of(fn), toks_of(ln), ~ if (length(.x) && length(.y)) unique(paste(substr(.x[1], 1, 1), .y)) else character(0))

# same person despite a spelling change? (HIMSS sometimes issues a new id when a name is corrected, e.g. "Cindy Ubaniak" -> "Cindy Urbaniak")
same_name <- function(f1, l1, f2, l2) {
  f1 <- str_to_lower(coalesce(f1, "")); f2 <- str_to_lower(coalesce(f2, "")); l1 <- str_to_lower(str_remove_all(coalesce(l1, ""), "[^A-Za-z]")); l2 <- str_to_lower(str_remove_all(coalesce(l2, ""), "[^A-Za-z]"))
  ok <- nchar(l1) > 0 & nchar(l2) > 0
  ok & ((l1 == l2 & substr(f1, 1, 1) == substr(f2, 1, 1)) |                                                   # same surname + initial
          (substr(f1, 1, 1) == substr(f2, 1, 1) & mapply(function(a, b) adist(a, b)[1, 1], l1, l2) <= ifelse(pmin(nchar(l1), nchar(l2)) >= 5, 2, 1)) |   # surname typo
          (f1 == f2 & nchar(f1) > 2 & mapply(function(a, b) adist(a, b)[1, 1], l1, l2) <= 2))                   # same first name, close surname
}

# =============================== PART 1: stack the HIMSS tables =====================================================
contact <- map_dfr(YEARS, read_tab, name = "Contact")
hec     <- map_dfr(YEARS, read_tab, name = "HAEntityContact")
src     <- map_dfr(YEARS, read_tab, name = "ContactSource")
ent     <- map_dfr(YEARS, read_tab, name = "HAEntity")
stopifnot(!anyDuplicated(contact[, c("himss_year", "ContactId")]), !anyDuplicated(src[, c("himss_year", "ContactSourceId")]),
          !anyDuplicated(ent[, c("himss_year", "HAEntityId")]))

ent <- ent %>% select(any_of(c("himss_year", "HAEntityId", "UniqueId", "ParentId", "Name", "HAEntityType", "Type", "MedicareNumber", "City", "State", "Zip"))) %>%
  rename(hosp_uid = UniqueId, hosp_name = Name, hosp_type = Type) %>% mutate(ccn = pad_ccn6(MedicareNumber))

people <- hec %>%
  left_join(src %>% select(himss_year, HAContactSourceId = ContactSourceId, role = Name), by = c("himss_year", "HAContactSourceId")) %>%
  left_join(contact %>% select(himss_year, ContactId, person_uid = UniqueID, FirstName, MiddleInitial, LastName, Title, Credentials), by = c("himss_year", "ContactId")) %>%
  left_join(ent, by = c("himss_year", "HAEntityId"))
rm(contact, hec, src)

# =============================== PART 2: hospital contacts ============================================================
hp <- people %>% filter(HAEntityType == "Hospital", TypeId == "1", Status == "Active", !is.na(ContactId), !is.na(ccn), !is.na(LastName)) %>%
  mutate(slot = exec_slot(role), tclass = title_class(Title),
         cred = str_to_upper(coalesce(Credentials, "")),
         cred_md = str_detect(cred, md_regex) | str_detect(coalesce(Title, ""), md_regex),
         cred_rn = str_detect(cred, "\\bRN\\b|\\bBSN\\b|\\bMSN\\b|\\bDNP\\b|\\bNP\\b|\\bAPRN\\b|\\bCRNA\\b"),   # nursing credential
         cred_other = cred != "" & !cred_md,                                   # e.g. RN, CPA, PharmD: a credential that is not MD/DO
         nm = paste(str_to_lower(str_squish(FirstName)), str_to_lower(str_squish(LastName))))
# person id: HIMSS UniqueID (2009+); 2008 has none, so 2008 people take the 2009 id of the same name at the same hospital
id09 <- hp %>% filter(himss_year == 2009, !is.na(person_uid)) %>% distinct(hosp_uid, nm, person_uid) %>% group_by(hosp_uid, nm) %>% slice(1) %>% ungroup() %>% rename(uid09 = person_uid)
hp <- hp %>% left_join(id09, by = c("hosp_uid", "nm")) %>%
  mutate(pid = coalesce(person_uid, if_else(himss_year == 2008, uid09, NA_character_), paste0("n_", hosp_uid, "_", nm))) %>% select(-uid09)

# general medical/surgical hospitals, both ownership groups, with the 990 measures used for validation
hd <- read_rds(paste0(created_data_path, "/hospital_data.rds"))
gen <- hd %>% filter(general %in% 1, !is.na(MCRNUM), TaxYr %in% YEARS) %>% mutate(MCRNUM = pad_ccn6(MCRNUM)) %>%
  group_by(MCRNUM, TaxYr) %>% slice(1) %>% ungroup() %>%
  transmute(ccn = MCRNUM, himss_year = TaxYr, Filer.EIN, owner = if_else(in_leadership_data, "990 filer", "for-profit"), independent,
            ceo_md_990    = if ("ceo_md" %in% names(hd)) ceo_md else NA_real_,
            ceo_entry_990 = if ("ceo_entry" %in% names(hd)) ceo_entry else NA_real_,
            has_md_990    = as.integer(n_md_exec > 0))
hpg <- hp %>% inner_join(gen, by = c("ccn", "himss_year")) %>%
  mutate(in_scope = slot %in% EXEC_SLOTS | tclass %in% CMO_CLASS)                # executives + anyone titled CMO / VP medical affairs

# =============================== PART 3: physicians among executives ===================================================
persons <- hpg %>% filter(in_scope) %>% group_by(pid) %>%
  summarise(FirstName = first_nn(FirstName), MiddleInitial = first_nn((MiddleInitial)), LastName = first_nn(LastName),
            states = list(unique(na.omit(State))), owners = paste(sort(unique(owner)), collapse = "+"),
            main_slot = names(sort(table(slot), decreasing = TRUE))[1], slots = paste(sort(unique(slot)), collapse = "+"),
            hosp = paste(unique(hosp_name), collapse = " ; "), years = paste(range(himss_year), collapse = "-"),
            titles = paste(unique(na.omit(Title)), collapse = " ; "), creds = paste(unique(na.omit(Credentials)), collapse = " ; "),
            cred_md = any(cred_md), cred_other = any(cred_other) & !any(cred_md), .groups = "drop") %>%
  mutate(full_name = str_squish(paste(FirstName, coalesce(MiddleInitial, ""), LastName)))

# E2: link to 990 executives at the same hospital-year; carry over your verified doctor flag
ex990 <- read_rds(paste0(created_data_path, "/exec_individual_data.rds"))
n990 <- ex990 %>% filter(!is.na(name_cleaned)) %>%
  transmute(Filer.EIN, TaxYr, name_cleaned, position,
            doc990 = if ("doctor" %in% names(ex990)) suppressWarnings(as.numeric(doctor)) else NA_real_,
            npi990 = if ("npi" %in% names(ex990)) as.character(npi) else NA_character_) %>%
  distinct() %>% mutate(tk = toks_of(name_cleaned)) %>% filter(map_lgl(tk, ~ length(.x) >= 2 && !"unknown" %in% .x)) %>% mutate(keys = map(tk, cand_keys))
n990_long <- n990 %>% select(Filer.EIN, TaxYr, name_cleaned, position, doc990, npi990, keys) %>% unnest(keys) %>% rename(key = keys)
e2 <- hpg %>% filter(in_scope, owner == "990 filer") %>% distinct(pid, himss_year, Filer.EIN, FirstName, LastName) %>%
  mutate(key = himss_keys(FirstName, LastName)) %>% select(pid, himss_year, Filer.EIN, key) %>% unnest(key) %>%
  inner_join(n990_long, by = c("Filer.EIN", "himss_year" = "TaxYr", "key"), relationship = "many-to-many") %>%
  group_by(pid) %>% summarise(linked_990 = TRUE, doc990 = any(doc990 %in% 1), npi990 = first_nn((npi990)), pos990 = paste(sort(unique(position)), collapse = "; "), .groups = "drop")
persons <- persons %>% left_join(e2, by = "pid") %>% mutate(linked_990 = coalesce(linked_990, FALSE), doc990 = coalesce(doc990, FALSE))

persons <- persons %>% mutate(
  status = case_when(cred_md              ~ "physician: HIMSS credential/title",
                     linked_990 & doc990  ~ "physician: 990 verified",
                     cred_other           ~ "not physician: non-MD credential",
                     linked_990           ~ "not physician: 990",
                     TRUE                 ~ "not physician: no evidence (credential blank)"),
  physician = str_starts(status, "physician"))

# =============================== PART 4: hospital-year leadership variables =========================================
hpg <- hpg %>% left_join(persons %>% select(pid, physician), by = "pid") %>% mutate(physician = coalesce(physician, FALSE) | cred_md)
hy <- hpg %>% group_by(ccn, himss_year, owner, Filer.EIN, independent, ceo_md_990, ceo_entry_990, has_md_990) %>%
  summarise(ceo_pid            = first_nn(pid[slot == "CEO"]),
            ceo_first          = first_nn(FirstName[slot == "CEO"]),
            ceo_last           = first_nn(LastName[slot == "CEO"]),
            ceo_md_himss       = if (any(slot == "CEO")) any(physician[slot == "CEO"]) else NA,
            ceo_clinical_himss = if (any(slot == "CEO")) any((physician | cred_rn)[slot == "CEO"]) else NA,          # physician or nurse CEO
            has_md_exec        = any(physician[slot %in% MD_SLOTS]),                                        # physician CEO/CFO/COO/CIO/CNO (no MSC, no CMIO)
            has_md_exec_cmo    = any(physician[slot %in% MD_SLOTS | tclass %in% CMO_CLASS]),                # + anyone titled CMO / VP med affairs
            cmo_title          = any(tclass %in% CMO_CLASS),
            n_exec_slots       = n_distinct(slot[slot %in% MD_SLOTS]), .groups = "drop") %>%
  arrange(ccn, himss_year) %>% group_by(ccn) %>%
  mutate(consec    = !is.na(lag(himss_year)) & lag(himss_year) == himss_year - 1,
         prev_pid  = if_else(consec, lag(ceo_pid), NA_character_),
         prev_first = if_else(consec, lag(ceo_first), NA_character_), prev_last = if_else(consec, lag(ceo_last), NA_character_)) %>% ungroup() %>%
  mutate(id_changed       = !is.na(ceo_pid) & !is.na(prev_pid) & ceo_pid != prev_pid,
         spelling_only    = id_changed & same_name(ceo_first, ceo_last, prev_first, prev_last),          # new id but the same person
         ceo_change_himss = if_else(!is.na(ceo_pid) & !is.na(prev_pid), as.integer(id_changed & !spelling_only), NA_integer_),
         ceo_name         = paste(ceo_first, ceo_last)) %>%
  select(-consec, -prev_pid, -prev_first, -prev_last, -id_changed)

saveRDS(hy, paste0(created_data_path, "/himss_hospital_year.rds"))
saveRDS(persons, paste0(created_data_path, "/himss_persons.rds"))
saveRDS(hpg %>% select(himss_year, ccn, hosp_uid, hosp_name, State, owner, Filer.EIN, role, slot, tclass, pid, FirstName, MiddleInitial, LastName,
                       Title, Credentials, cred_md, physician), paste0(created_data_path, "/himss_hospital_contacts.rds"))

# =============================== PART 5: validation diagnostics ======================================================
if (RUN_DIAGNOSTICS) {
  out <- file(paste0(created_data_path, "/himss_diagnostics.txt"), open = "wt"); sink(out, split = TRUE)
  cat("HIMSS CEO id changes that are only a spelling change (not counted as CEO changes):", sum(hy$spelling_only, na.rm = TRUE), "\n")
  
  cat("\n== 1. Coverage: general hospital-years found in HIMSS ==\n")
  print(gen %>% left_join(hpg %>% distinct(ccn, himss_year) %>% mutate(found = TRUE), by = c("ccn", "himss_year")) %>%
          group_by(owner) %>% summarise(hosp_years = n(), share_found = round(mean(found %in% TRUE), 3)))
  cat("\nFilled executive slots by year (general hospitals):\n")
  print(hpg %>% filter(slot %in% c(EXEC_SLOTS, "MSC")) %>% distinct(himss_year, ccn, slot) %>% count(slot, himss_year) %>% pivot_wider(names_from = himss_year, values_from = n, values_fill = 0))
  
  cat("\n== 2. Person id: same name, hospital and slot in consecutive years -> same id ==\n")
  k <- hpg %>% filter(slot %in% EXEC_SLOTS) %>% distinct(himss_year, ccn, slot, nm, pid)
  print(k %>% inner_join(k %>% mutate(himss_year = himss_year - 1), by = c("himss_year", "ccn", "slot", "nm"), suffix = c("", "_next")) %>%
          group_by(years = paste(himss_year, himss_year + 1, sep = "->")) %>% summarise(pairs = n(), same_id = round(mean(pid == pid_next), 3)))
  
  # ---- CEO identity agreement ----
  gen990 <- gen %>% filter(owner == "990 filer")
  ceo990 <- n990 %>% filter(position == "ceo") %>% inner_join(gen990 %>% select(Filer.EIN, TaxYr = himss_year, ccn), by = c("Filer.EIN", "TaxYr"), relationship = "many-to-many")
  f990 <- ceo990 %>% select(ccn, TaxYr, keys) %>% unnest(keys) %>% rename(key = keys) %>% distinct()
  ceoH <- hpg %>% filter(slot == "CEO", owner == "990 filer") %>% mutate(hkeys = himss_keys(FirstName, LastName))
  hk <- ceoH %>% select(ccn, himss_year, hkeys) %>% unnest(hkeys) %>% rename(hkey = hkeys) %>% distinct()
  cat("\n== 3. CEO identity agreement by alignment (HIMSS year = TaxYr + lag) ==\n")
  print(map_dfr(-1:1, function(L) {
    base <- f990 %>% distinct(ccn, TaxYr) %>% mutate(hy = TaxYr + L) %>% inner_join(hk %>% distinct(ccn, himss_year), by = c("ccn", "hy" = "himss_year"))
    hit  <- f990 %>% mutate(hy = TaxYr + L) %>% inner_join(hk, by = c("ccn", "hy" = "himss_year", "key" = "hkey")) %>% distinct(ccn, TaxYr)
    tibble(lag = L, ccn_years = nrow(base), agree = round(nrow(semi_join(base, hit, by = c("ccn", "TaxYr"))) / nrow(base), 3))
  }))
  
  cat("\n== 4. Why CEOs disagree (lag 0; flags assigned in order) ==\n")
  cy <- f990 %>% distinct(ccn, TaxYr) %>% inner_join(hk %>% distinct(ccn, himss_year), by = c("ccn", "TaxYr" = "himss_year")) %>%
    left_join(f990 %>% inner_join(hk, by = c("ccn", "TaxYr" = "himss_year", "key" = "hkey")) %>% distinct(ccn, TaxYr) %>% mutate(agree = TRUE), by = c("ccn", "TaxYr")) %>%
    mutate(agree = coalesce(agree, FALSE)) %>% left_join(gen990 %>% select(ccn, TaxYr = himss_year, Filer.EIN, independent), by = c("ccn", "TaxYr")) %>%
    left_join(ceoH %>% group_by(ccn, himss_year) %>% summarise(State = first_nn(State), .groups = "drop"), by = c("ccn", "TaxYr" = "himss_year"))
  var_fun <- function(tk, hf, hl) {
    if (!length(tk) || !length(hl) || is.na(hf)) return(FALSE)
    ini <- substr(hf, 1, 1); hl4 <- hl[nchar(hl) >= 4]; hl_c <- paste(hl, collapse = "")
    any(hl %in% tk) ||
      (length(hl4) > 0 && any(adist(hl4, tk) <= ifelse(nchar(hl4) >= 5, 2, 1)) && ini %in% substr(tk, 1, 1)) ||
      (nchar(hl_c) >= 3 && any(tk == hl_c) && ini %in% substr(setdiff(tk, hl_c), 1, 1)) ||
      (nchar(hl_c) >= 3 && any(startsWith(tk, hl_c) & nchar(tk) > nchar(hl_c) & substr(tk, nchar(hl_c) + 1, nchar(hl_c) + 1) == ini))
  }
  # save, per hospital-year, whether HIMSS and the 990 name the same CEO (validation subsample; NA = no usable 990 CEO name)
  hy <- hy %>% select(-any_of("ceo_same_990")) %>% left_join(cy %>% select(ccn, himss_year = TaxYr, ceo_same_990 = agree), by = c("ccn", "himss_year"))
  saveRDS(hy, paste0(created_data_path, "/himss_hospital_year.rds"))
  mm0 <- cy %>% filter(!agree)
  fs  <- function(d) d %>% distinct(ccn, TaxYr) %>% mutate(hit = TRUE)
  variant <- ceo990 %>% semi_join(mm0, by = c("ccn", "TaxYr")) %>% select(ccn, TaxYr, tk) %>%
    inner_join(ceoH %>% transmute(ccn, himss_year, hf = map_chr(toks_of(FirstName), ~ if (length(.x)) .x[1] else NA_character_), hl = toks_of(LastName)),
               by = c("ccn", "TaxYr" = "himss_year"), relationship = "many-to-many") %>%
    filter(pmap_lgl(list(tk, hf, hl), var_fun)) %>% fs()
  lags  <- map_dfr(1:2, ~ f990 %>% mutate(t2 = TaxYr + .x) %>% semi_join(hk, by = c("ccn", "t2" = "himss_year", "key" = "hkey"))) %>% fs()
  leads <- map_dfr(1:2, ~ f990 %>% mutate(t2 = TaxYr - .x) %>% semi_join(hk, by = c("ccn", "t2" = "himss_year", "key" = "hkey"))) %>% fs()
  hk_any <- hpg %>% filter(slot != "CEO", role != "CIO Reports to") %>% mutate(key = himss_keys(FirstName, LastName)) %>% select(ccn, himss_year, key) %>% unnest(key) %>% distinct()
  other_role <- f990 %>% semi_join(hk_any, by = c("ccn", "TaxYr" = "himss_year", "key")) %>% fs()
  in990 <- cy %>% select(Filer.EIN, ccn, TaxYr) %>% inner_join(hk, by = c("ccn", "TaxYr" = "himss_year"), relationship = "many-to-many") %>%
    semi_join(n990_long %>% filter(position != "ceo"), by = c("Filer.EIN", "TaxYr", "hkey" = "key")) %>% fs()
  pk <- people %>% filter(TypeId == "1", Status == "Active", !is.na(ContactId), !is.na(LastName), role != "CIO Reports to") %>%
    distinct(himss_year, ccn, State, FirstName, LastName) %>% mutate(key = himss_keys(FirstName, LastName)) %>% select(himss_year, ccn, State, key) %>% unnest(key) %>% distinct()
  system_level <- f990 %>% inner_join(cy %>% select(ccn, TaxYr, State), by = c("ccn", "TaxYr")) %>%
    inner_join(pk %>% rename(ccn2 = ccn), by = c("State", "TaxYr" = "himss_year", "key"), relationship = "many-to-many") %>% filter(is.na(ccn2) | ccn2 != ccn) %>% fs()
  addf <- function(d, f, nm) d %>% left_join(f %>% rename(!!nm := hit), by = c("ccn", "TaxYr")) %>% mutate(!!nm := coalesce(.data[[nm]], FALSE))
  mm <- mm0 %>% addf(variant, "name_variant") %>% addf(lags, "himss_lags") %>% addf(leads, "himss_leads") %>% addf(other_role, "other_role") %>%
    addf(in990, "in_990_other") %>% addf(system_level, "system_level") %>%
    mutate(class = case_when(name_variant ~ "1 name_variant", himss_lags ~ "2 himss_lags", himss_leads ~ "3 himss_leads", other_role ~ "4 other_role",
                             in_990_other ~ "5 in_990_other", system_level ~ "6 system_level", TRUE ~ "7 unexplained"))
  cat("CCN-years:", nrow(cy), "| exact agreement:", round(mean(cy$agree), 3), "\n")
  print(mm %>% count(class) %>% mutate(share_of_disagreements = round(n / sum(n), 3), share_of_all = round(n / nrow(cy), 3)))
  cat("Agreement counting name variants and 1-2 year timing:", round(mean(cy$agree | paste(cy$ccn, cy$TaxYr) %in% paste(mm$ccn, mm$TaxYr)[mm$class %in% c("1 name_variant", "2 himss_lags", "3 himss_leads")]), 3), "\n")
  print(cy %>% group_by(independent) %>% summarise(ccn_years = n(), agree = round(mean(agree), 3)))
  
  cat("\n== 5. CMO-like titles: share of 990 general hospital-years with a CMO, HIMSS vs 990 ==\n")
  cmo990 <- ex990 %>% filter(position == "cmo") %>% distinct(Filer.EIN, TaxYr) %>% mutate(cmo_990 = TRUE)
  cv <- hy %>% filter(owner == "990 filer") %>% left_join(cmo990, by = c("Filer.EIN", "himss_year" = "TaxYr")) %>% mutate(cmo_990 = coalesce(cmo_990, FALSE))
  print(cv %>% group_by(himss_year) %>% summarise(n = n(), cmo_himss = round(mean(cmo_title), 3), cmo_990 = round(mean(cmo_990), 3)))
  print(cv %>% count(cmo_990, cmo_title))
  
  cat("\n== 6. Physician evidence: is a blank HIMSS credential informative?  (990-linked executives; 990 doctor flag = reference) ==\n")
  print(persons %>% filter(linked_990) %>% group_by(main_slot) %>%
          summarise(people = n(), share_990_doctor = round(mean(doc990), 3),
                    doctor_if_HIMSS_MD = round(mean(doc990[cred_md]), 3), doctor_if_credential_blank = round(mean(doc990[!cred_md & !cred_other]), 3),
                    HIMSS_MD_if_990_doctor = round(mean(cred_md[doc990]), 3), .groups = "drop"))
  cat("\nEvidence class by main slot and ownership:\n")
  print(persons %>% count(main_slot, owners, status) %>% pivot_wider(names_from = status, values_from = n, values_fill = 0), width = Inf, n = 40)
  
  cat("\n== 7. Validation of the leadership variables against the 990s (990 filers, same hospital-year) ==\n")
  v <- hy %>% filter(owner == "990 filer")
  tab2 <- function(a, b, la, lb) { d <- tibble(a = as.integer(a), b = as.integer(b)) %>% filter(!is.na(a), !is.na(b))
  cat("\n", la, "vs", lb, ": n =", nrow(d), "| agree =", round(mean(d$a == d$b), 3), "| share 1:", round(mean(d$a), 3), "vs", round(mean(d$b), 3),
      "| P(", lb, "=1 |", la, "=1) =", round(mean(d$b[d$a == 1]), 3), "\n"); print(count(d, a, b)) }
  tab2(v$ceo_md_990, v$ceo_md_himss,      "ceo_md (990)", "ceo_md_himss")
  # where they disagree on a physician CEO: do the two sources even name the same CEO?  (cy = CEO identity agreement, lag 0)
  dis <- v %>% filter(!is.na(ceo_md_990), !is.na(ceo_md_himss)) %>% mutate(cell = paste0("990=", ceo_md_990, " HIMSS=", as.integer(ceo_md_himss))) %>%
    left_join(cy %>% select(ccn, TaxYr, same_ceo = agree), by = c("ccn", "himss_year" = "TaxYr"))
  cat("\nPhysician-CEO cells by whether both sources name the SAME CEO (NA = no usable 990 CEO name):\n")
  print(dis %>% count(cell, same_ceo) %>% group_by(cell) %>% mutate(share = round(n / sum(n), 3)))
  cat("Physician-CEO agreement when both sources name the same CEO:", round(with(dis %>% filter(same_ceo %in% TRUE), mean(ceo_md_990 == as.integer(ceo_md_himss))), 3), "\n")
  tab2(v$has_md_990, v$has_md_exec,       "has_md (990)", "has_md_exec")
  tab2(v$has_md_990, v$has_md_exec_cmo,   "has_md (990)", "has_md_exec_cmo")
  tab2(v$ceo_entry_990, v$ceo_change_himss, "ceo_entry (990)", "ceo_change_himss")
  near <- v %>% arrange(ccn, himss_year) %>% group_by(ccn) %>%
    mutate(chg_pm1 = pmax(ceo_change_himss, lag(ceo_change_himss), lead(ceo_change_himss), na.rm = TRUE)) %>% ungroup()
  tab2(near$ceo_entry_990, near$chg_pm1, "ceo_entry (990)", "HIMSS CEO change within +-1 year")
  
  cat("\n== 8. Leadership variables by ownership and year (all general hospitals) ==\n")
  print(hy %>% group_by(owner, himss_year) %>%
          summarise(n = n(), ceo_md = round(mean(ceo_md_himss, na.rm = TRUE), 3),
                    has_md_exec = round(mean(has_md_exec), 3), has_md_exec_cmo = round(mean(has_md_exec_cmo), 3),
                    ceo_change = round(mean(ceo_change_himss, na.rm = TRUE), 3), .groups = "drop"), n = 30, width = Inf)
  sink(); close(out)
  cat("\nWrote himss_diagnostics.txt\n")
}

# =============================== PART 6: timing of CEO changes, HIMSS vs 990 ==========================================
# 990 TaxYr = calendar year with the MAJORITY of the fiscal year's days (data01). So TaxYr Y covers:
#   June year-end: Jul Y - Jun Y+1 | September: Oct Y-1 - Sep Y | December: Jan - Dec Y.   HIMSS has no survey dates.
# If HIMSS is a snapshot at a fixed calendar date, the HIMSS-minus-990 gap in the year a new CEO first appears should differ by
# fiscal year-end in a predictable way, which locates the snapshot.  Writes himss_timing.txt and himss_timing_sample.csv.
RUN_TIMING <- TRUE                     # needs RUN_DIAGNOSTICS = TRUE in the same session (uses f990 / hk from PART 5)
if (RUN_TIMING) {
  pad_ein <- function(x) str_pad(str_trim(as.character(x)), 9, pad = "0")
  einp <- read_rds(paste0(created_data_path, "/xml_ein_data_new(with_ind).rds")) %>%
    transmute(Filer.EIN = pad_ein(ein), beg_dt = as.Date(beg_dt), end_dt = as.Date(end_dt)) %>% filter(!is.na(beg_dt), !is.na(end_dt)) %>%
    mutate(end_y = as.integer(format(end_dt, "%Y")), days_all = as.numeric(end_dt - beg_dt) + 1,
           days_end = as.numeric(end_dt - pmax(beg_dt, as.Date(paste0(end_y, "-01-01")))) + 1,
           TaxYr = if_else(days_end >= days_all - days_end, end_y, end_y - 1L)) %>%
    group_by(Filer.EIN, TaxYr) %>% slice_max(days_all, n = 1, with_ties = FALSE) %>% ungroup() %>%
    mutate(fye_month = as.integer(format(end_dt, "%m")),
           fye = case_when(fye_month == 6 ~ "June", fye_month == 9 ~ "September", fye_month == 12 ~ "December", TRUE ~ "other")) %>%
    select(Filer.EIN, TaxYr, beg_dt, end_dt, fye)
  # fiscal dates are missing for ~half of hospital-years (the XML filings do not cover every year); the fiscal year-END MONTH is
  # stable within a hospital, so fill it from the hospital's most common year-end
  fye_e <- einp %>% count(Filer.EIN, fye) %>% group_by(Filer.EIN) %>% slice_max(n, n = 1, with_ties = FALSE) %>% ungroup() %>% select(Filer.EIN, fye_e = fye)
  add_fye <- function(d) d %>% left_join(einp, by = c("Filer.EIN", "TaxYr")) %>% left_join(fye_e, by = "Filer.EIN") %>%
    mutate(fye = coalesce(fye, fye_e)) %>% select(-fye_e)
  gen990 <- gen %>% filter(owner == "990 filer") %>% transmute(Filer.EIN = pad_ein(Filer.EIN), TaxYr = himss_year, ccn, independent)
  
  # new CEOs in the 990: CEO name at TaxYr t, previous year observed, no name overlap with any CEO at t-1
  c990 <- n990 %>% filter(position == "ceo") %>% mutate(Filer.EIN = pad_ein(Filer.EIN)) %>% select(Filer.EIN, TaxYr, name_cleaned, keys) %>% mutate(rid = row_number())
  c990_long <- c990 %>% select(rid, Filer.EIN, TaxYr, keys) %>% unnest(keys) %>% rename(key = keys)
  prev_obs  <- c990 %>% distinct(Filer.EIN, TaxYr) %>% mutate(TaxYr = TaxYr + 1)
  prev_keys <- c990_long %>% distinct(Filer.EIN, TaxYr, key) %>% mutate(TaxYr = TaxYr + 1)
  new990 <- c990 %>% semi_join(prev_obs, by = c("Filer.EIN", "TaxYr")) %>%
    anti_join(c990_long %>% semi_join(prev_keys, by = c("Filer.EIN", "TaxYr", "key")) %>% distinct(rid), by = "rid") %>%
    inner_join(gen990, by = c("Filer.EIN", "TaxYr")) %>% add_fye()
  
  # HIMSS: each CEO's first year as CEO at the hospital; a TRUE change if the hospital had a different CEO the year before
  hc <- hpg %>% filter(slot == "CEO") %>% distinct(ccn, himss_year, pid, FirstName, LastName, hosp_name, City, State)
  fh <- hc %>% group_by(ccn, pid) %>% summarise(fh = min(himss_year), FirstName = first_nn(FirstName), LastName = first_nn(LastName), .groups = "drop")
  prevh <- hc %>% distinct(ccn, himss_year, prev_pid = pid) %>% mutate(himss_year = himss_year + 1)
  newH <- fh %>% inner_join(prevh, by = c("ccn", "fh" = "himss_year"), relationship = "many-to-many") %>%
    group_by(ccn, pid) %>% summarise(true_new = all(prev_pid != pid), prev_pid = first_nn(prev_pid[prev_pid != pid]), .groups = "drop") %>% filter(true_new) %>%
    left_join(fh %>% select(ccn, pid, FirstName, LastName), by = c("ccn", "pid")) %>%
    left_join(fh %>% select(ccn, prev_pid = pid, pf = FirstName, pl = LastName), by = c("ccn", "prev_pid")) %>%
    filter(!same_name(FirstName, LastName, pf, pl)) %>% select(ccn, pid, prev_pid)                 # a new id for the same person is not a change
  hkeys_all <- fh %>% mutate(key = himss_keys(FirstName, LastName)) %>% select(ccn, pid, fh, key) %>% unnest(key)
  
  m <- c990_long %>% semi_join(new990, by = "rid") %>% inner_join(new990 %>% distinct(rid, ccn), by = "rid", relationship = "many-to-many") %>%
    inner_join(hkeys_all, by = c("ccn", "key"), relationship = "many-to-many") %>% distinct(rid, ccn, TaxYr, pid, fh) %>%
    group_by(rid, ccn) %>% slice_min(abs(fh - TaxYr), n = 1, with_ties = FALSE) %>% ungroup() %>%
    mutate(d = fh - TaxYr, himss_true_new = paste(ccn, pid) %in% paste(newH$ccn, newH$pid))
  tm <- new990 %>% left_join(m %>% select(rid, ccn, pid, fh, d, himss_true_new), by = c("rid", "ccn")) %>%
    mutate(found = !is.na(d), both_new = found & himss_true_new, interior = TaxYr >= min(YEARS) + 2 & TaxYr <= max(YEARS) - 2)   # room for +-2 in HIMSS
  
  out <- file(paste0(created_data_path, "/himss_timing.txt"), open = "wt"); sink(out, split = TRUE)
  cat("== T0. 990 fiscal year-end among general 990 hospital-years ==\n"); print(gen990 %>% add_fye() %>% count(fye) %>% mutate(share = round(n / sum(n), 3)))
  cat("\n== T1. New 990 CEOs (TaxYr", min(YEARS) + 2, "-", max(YEARS) - 2, "): found as CEO in HIMSS at the same hospital? ==\n")
  print(tm %>% filter(interior) %>% summarise(new_990_ceos = n(), found_as_ceo_in_himss = round(mean(found), 3), and_himss_shows_a_change = round(mean(both_new), 3)))
  cat("\n== T2. Gap d = (first HIMSS year as CEO) - (first 990 TaxYr as CEO), both sources show a change ==\n")
  print(tm %>% filter(interior, both_new) %>% mutate(d = pmin(pmax(d, -3), 3)) %>% count(d) %>% mutate(share = round(n / sum(n), 3)))
  cat("\n== T3. Same, by 990 fiscal year-end (this locates the HIMSS snapshot) ==\n")
  print(tm %>% filter(interior, both_new) %>% mutate(d = pmin(pmax(d, -2), 2)) %>% count(fye, d) %>% group_by(fye) %>%
          mutate(share = round(n / sum(n), 3), n_fye = sum(n)) %>% select(-n) %>% pivot_wider(names_from = d, values_from = share, names_prefix = "d=", values_fill = 0))
  print(tm %>% filter(interior, both_new) %>% group_by(fye) %>% summarise(n = n(), mean_d = round(mean(d), 2), median_d = median(d)))
  cat("\n== T4. Same, by system membership ==\n")
  print(tm %>% filter(interior, both_new) %>% group_by(independent) %>% summarise(n = n(), mean_d = round(mean(d), 2), share_d0 = round(mean(d == 0), 3)))
  cat("\n== T5. CEO identity agreement by lag and fiscal year-end ==\n")
  f990f <- f990 %>% left_join(gen990 %>% select(ccn, TaxYr, Filer.EIN), by = c("ccn", "TaxYr")) %>% add_fye()
  print(map_dfr(-1:1, function(L) {
    base <- f990f %>% distinct(ccn, TaxYr, fye) %>% mutate(hy = TaxYr + L) %>% inner_join(hk %>% distinct(ccn, himss_year), by = c("ccn", "hy" = "himss_year"))
    hit  <- f990f %>% mutate(hy = TaxYr + L) %>% inner_join(hk, by = c("ccn", "hy" = "himss_year", "key" = "hkey")) %>% distinct(ccn, TaxYr) %>% mutate(a = TRUE)
    base %>% left_join(hit, by = c("ccn", "TaxYr")) %>% group_by(fye) %>% summarise(n = n(), agree = round(mean(a %in% TRUE), 3), .groups = "drop") %>% mutate(lag = L)
  }) %>% pivot_wider(names_from = lag, values_from = c(n, agree), names_prefix = "lag"))
  sink(); close(out)
  
  # sample of matched transitions to date by hand / from public announcements
  set.seed(2)
  nm_of <- hc %>% distinct(ccn, pid, FirstName, LastName) %>% group_by(ccn, pid) %>% slice(1) %>% ungroup() %>% transmute(ccn, pid, nm = paste(FirstName, LastName))
  tm %>% filter(interior, both_new) %>% slice_sample(n = min(60, sum(tm$interior & tm$both_new))) %>%
    left_join(newH %>% select(ccn, pid, prev_pid), by = c("ccn", "pid")) %>%
    left_join(nm_of %>% rename(himss_new_ceo = nm), by = c("ccn", "pid")) %>%
    left_join(nm_of %>% rename(prev_pid = pid, himss_prev_ceo = nm), by = c("ccn", "prev_pid")) %>%
    left_join(hc %>% distinct(ccn, hosp_name, City, State) %>% group_by(ccn) %>% slice(1) %>% ungroup(), by = "ccn") %>%
    transmute(ccn, hosp_name, City, State, himss_prev_ceo, himss_new_ceo, new_ceo_990 = name_cleaned,
              first_990_TaxYr = TaxYr, fiscal_begin = beg_dt, fiscal_end = end_dt, fye, first_himss_year = fh, d,
              announced_month = NA_character_, source_url = NA_character_) %>%
    write_csv(paste0(created_data_path, "/himss_timing_sample.csv"))
  cat("Wrote himss_timing.txt and himss_timing_sample.csv\n")
}

# =============================== PART 7: combined CEO measure (HIMSS identity, 990-validated status) =================
# DECISIONS.md (3 Nov): WHO the CEO is comes from HIMSS (CEO role slot); WHETHER they are a physician / clinician comes from the validated
# 990 data when that person is on a 990, else from the HIMSS credential. Rules, in order, for each CCN-year with a HIMSS CEO:
#   1 "990: same CEO"        HIMSS and the 990 name the same CEO (ceo_same_990)                 -> the 990's ceo_md / ceo_clinical
#   2 "990: other position"  the HIMSS CEO is on a 990 under another name slot/position           -> that person's 990 doctor flag
#   3 "HIMSS credential"     the HIMSS CEO is on no 990 (all for-profits; some nonprofits)          -> HIMSS MD/DO credential
#   Clinical = physician OR nurse in EITHER source (HIMSS nursing credential or 990 nurse flag): 990 nurse status is not verified.
#   No HIMSS CEO that year -> NA (no fallback to the 990's algorithmically assigned CEO).
#   0 "hand-check"           the hand-checked cases listed in the code (tribble `ov`) override rules 1-3.
# Runs on its own after the settings + helpers at the top (reads only saved files; needs PART 5 to have saved ceo_same_990).
# Outputs (CreatedData): himss_ceo_combined.rds, ceo_combined_diagnostics.txt, ceo_handcheck.csv (cases to check by hand).
RUN_COMBINED <- TRUE
if (RUN_COMBINED) {
  TYPE_YEARS7 <- c(2010, 2009)                                    # same type years as analysis02 (2010, else 2009)
  hy7 <- read_rds(paste0(created_data_path, "/himss_hospital_year.rds"))
  if (!"ceo_same_990" %in% names(hy7)) stop("ceo_same_990 missing: run PART 5 (RUN_DIAGNOSTICS = TRUE) first")
  ct7 <- read_rds(paste0(created_data_path, "/himss_hospital_contacts.rds"))
  hd7 <- read_rds(paste0(created_data_path, "/hospital_data.rds"))
  ex7 <- read_rds(paste0(created_data_path, "/exec_individual_data.rds"))
  
  # ---- 990 person flags (validated) ----
  cat("exec_individual_data columns:", paste(names(ex7), collapse = ", "), "\n")
  DOC_COL   <- "doctor"
  NURSE_COL <- first_nn(grep("nurs|^rn$", names(ex7), value = TRUE, ignore.case = TRUE))   # overwrite by hand if this picks the wrong column
  cat("990 person flags used: physician =", DOC_COL, "| nurse =", NURSE_COL, "(NA = none found: rule 2 then takes nursing from the HIMSS credential)\n")
  flag <- function(col) if (!is.na(col) && col %in% names(ex7)) suppressWarnings(as.numeric(ex7[[col]])) %in% 1 else rep(NA, nrow(ex7))
  # physician specialty (and NPI) from your 990 validation: every column whose name looks like a specialty / taxonomy is kept
  spec_all  <- grep("special|taxonom", names(ex7), value = TRUE, ignore.case = TRUE)
  SPEC_COLS <- spec_all[!str_detect(spec_all, regex("source|src|note|flag", ignore_case = TRUE))]   # the specialty itself; overwrite by hand if needed
  SPEC_SRC_COLS <- setdiff(spec_all, SPEC_COLS)                                                      # where it came from (npi_data / review_note)
  cat("990 specialty columns kept:", if (length(SPEC_COLS)) paste(SPEC_COLS, collapse = ", ") else "NONE FOUND - set SPEC_COLS by hand",
      "| specialty source columns:", paste(SPEC_SRC_COLS, collapse = ", "), "\n")
  cat("hospital_data columns that look like CEO specialty:", paste(grep("special|taxonom", names(hd7), value = TRUE, ignore.case = TRUE), collapse = ", "), "\n")
  spec_of <- function(d, cols) {
    if (!length(cols)) return(rep(NA_character_, nrow(d)))
    m <- matrix(sapply(cols, function(v) str_squish(as.character(d[[v]]))), nrow = nrow(d))
    apply(m, 1, function(r) { r <- unique(r[!is.na(r) & r != "" & r != "NA"]); if (length(r)) paste(r, collapse = " / ") else NA_character_ })
  }
  ex7 <- ex7 %>% mutate(doc = flag(DOC_COL), nurse = flag(NURSE_COL), spec = spec_of(ex7, SPEC_COLS), spec_src = spec_of(ex7, SPEC_SRC_COLS),
                        npi = if ("npi" %in% names(ex7)) na_if(as.character(npi), "") else NA_character_)
  p990 <- ex7 %>% filter(!is.na(name_cleaned)) %>% distinct(Filer.EIN, TaxYr, name_cleaned, position, doc, nurse, spec, spec_src, npi) %>%
    mutate(tk = toks_of(name_cleaned)) %>% filter(map_lgl(tk, ~ length(.x) >= 2 && !"unknown" %in% .x)) %>%
    mutate(key = map(tk, cand_keys)) %>% select(-tk) %>% unnest(key)
  
  # ---- link every HIMSS CEO (as a person, any year, any slot) to 990 executives at the same EIN-year ----
  ceo_pids <- ct7 %>% filter(slot == "CEO") %>% distinct(pid)
  lk <- ct7 %>% filter(owner == "990 filer") %>% semi_join(ceo_pids, by = "pid") %>% distinct(pid, himss_year, Filer.EIN, FirstName, LastName) %>%
    mutate(key = himss_keys(FirstName, LastName)) %>% select(pid, himss_year, Filer.EIN, key) %>% unnest(key) %>%
    inner_join(p990, by = c("Filer.EIN", "himss_year" = "TaxYr", "key"), relationship = "many-to-many") %>%
    group_by(pid) %>% summarise(linked = TRUE, doc990 = any(doc %in% TRUE), nurse990 = any(nurse %in% TRUE), nurse_known = any(!is.na(nurse)),
                                pos990 = paste(sort(unique(position)), collapse = "; "), name990 = paste(unique(name_cleaned), collapse = "; "),
                                spec990 = { s <- unique(na.omit(spec)); if (length(s)) paste(s, collapse = "; ") else NA_character_ },
                                spec_src990 = { s <- unique(na.omit(spec_src)); if (length(s)) paste(s, collapse = "; ") else NA_character_ },
                                npi990  = first_nn(npi), .groups = "drop")
  # HIMSS credentials, person level (credentials are often blank in some years)
  hc7 <- ct7 %>% mutate(cr = str_to_upper(coalesce(Credentials, "")), rn = str_detect(cr, "\\bRN\\b|\\bBSN\\b|\\bMSN\\b|\\bDNP\\b|\\bNP\\b|\\bAPRN\\b|\\bCRNA\\b")) %>%
    group_by(pid) %>% summarise(h_md = any(cred_md %in% TRUE), h_rn = any(rn), h_creds = paste(unique(na.omit(Credentials)), collapse = "; "), .groups = "drop")
  # 990 hospital-level CEO status (the measure used so far)
  h990 <- hd7 %>% filter(general %in% 1, in_leadership_data %in% TRUE, !is.na(MCRNUM)) %>% mutate(ccn = pad_ccn6(MCRNUM)) %>%
    group_by(ccn, TaxYr) %>% slice(1) %>% ungroup() %>% transmute(ccn, himss_year = TaxYr, md990 = ceo_md, clin990 = ceo_clinical)
  
  cb <- hy7 %>% filter(!is.na(ceo_pid)) %>% distinct(ccn, himss_year, .keep_all = TRUE) %>%
    select(ccn, himss_year, owner, Filer.EIN, ceo_pid, ceo_name, ceo_same_990, h_md_old = ceo_md_himss, h_clin_old = ceo_clinical_himss) %>%
    left_join(h990, by = c("ccn", "himss_year")) %>% left_join(lk, by = c("ceo_pid" = "pid")) %>% left_join(hc7, by = c("ceo_pid" = "pid")) %>%
    mutate(linked = coalesce(linked, FALSE), h_md = coalesce(h_md, FALSE), h_rn = coalesce(h_rn, FALSE),
           c_source = case_when(owner == "990 filer" & ceo_same_990 %in% TRUE & !is.na(md990) ~ "1 990: same CEO",
                                linked                                                        ~ "2 990: other position",
                                TRUE                                                          ~ "3 HIMSS credential"),
           c_ceo_md = case_when(c_source == "1 990: same CEO"       ~ md990 %in% 1,
                                c_source == "2 990: other position" ~ doc990,
                                TRUE                                ~ h_md),
           # clinical = physician (rules above) OR a nurse in EITHER source: nurse status is not verified in the 990s, so a nursing
           # credential in HIMSS or a nurse flag in the 990 both count (DECISIONS.md, 3 Nov)
           c_ceo_clinical = case_when(c_source == "1 990: same CEO"       ~ coalesce(clin990 == 1, md990 == 1) | h_rn,
                                      c_source == "2 990: other position" ~ doc990 | nurse990 | h_rn,
                                      TRUE                                ~ h_md | h_rn),
           across(c(c_ceo_md, c_ceo_clinical), as.integer),
           c_ceo_specialty = spec990, c_ceo_specialty_src = spec_src990, c_ceo_npi = npi990)          # from the 990 person the HIMSS CEO links to (rules 1-2); NA under rule 3 (HIMSS has no specialty)
  
  # ---- hand-check overrides (checked 3 Nov against public sources; see DECISIONS.md) ----
  # Only physician-type cases where a check could change the type were checked. NA = not overridden.
  ov <- tribble(
    ~ccn,     ~himss_year, ~ov_md, ~ov_clin, ~ov_spec,     ~true_ceo,              ~why,
    "210019", 2010,        0L,     1L,       NA_character_, "Peggy (Margaret) Naleppa", "Peninsula Regional: RN, doctorate in management; HIMSS 'MD' is likely the state. Same person as the 990 CEO.",
    "201300", 2010,        1L,     1L,       NA_character_, "Erik Steele",          "Blue Hill: HIMSS stale (Garrity left Dec 2008); Dr. Steele interim CEO Dec 2008 - Jul 2010 (Bangor Daily News).",
    "140119", 2010,        1L,     1L,       NA_character_, "Larry J. Goodman",     "Rush: Goodman (MD) CEO of Rush University Medical Center; HIMSS CEO slot held the COO (FierceHealthcare, Sep 2010).",
    "390065", 2010,        1L,     1L,       NA_character_, "Kevin Mosser",         "Gettysburg: Dr. Mosser president until Jan 2011 (CPBJ). Confirms.",
    "140202", 2010,        1L,     1L,       NA_character_, "Ann Errichetti",       "Advocate Condell: physician confirmed (Becker's). Confirms.",
    "330169", 2010,        1L,     1L,       NA_character_, "Harris Nagler",        "Beth Israel NY: physician (urologist); 990 CEO ran the parent system. Confirms.")
  cb <- cb %>% mutate(himss_year = as.numeric(himss_year)) %>%
    left_join(ov %>% select(ccn, himss_year, ov_md, ov_clin, ov_spec), by = c("ccn", "himss_year")) %>%
    mutate(c_source = if_else(!is.na(ov_md) | !is.na(ov_clin), "0 hand-check", c_source),
           c_ceo_md = coalesce(ov_md, c_ceo_md),
           c_ceo_clinical = coalesce(ov_clin, if_else(ov_md %in% 1L, 1L, NA_integer_), c_ceo_clinical),
           c_ceo_specialty = coalesce(ov_spec, c_ceo_specialty)) %>% select(-ov_md, -ov_clin, -ov_spec)
  cat("Hand-check overrides applied:", sum(cb$c_source == "0 hand-check"), "of", nrow(ov), "\n")
  saveRDS(cb %>% select(ccn, himss_year, c_ceo_md, c_ceo_clinical, c_ceo_specialty, c_ceo_specialty_src, c_ceo_npi, c_source), paste0(created_data_path, "/himss_ceo_combined.rds"))
  
  # ---- diagnostics ----
  out <- file(paste0(created_data_path, "/ceo_combined_diagnostics.txt"), open = "wt"); sink(out, split = TRUE)
  cat("== C1. Where the combined status comes from (all general hospital-years with a HIMSS CEO) ==\n")
  print(cb %>% count(owner, c_source) %>% group_by(owner) %>% mutate(share = round(n / sum(n), 3)) %>% ungroup())
  cat("\n== C2. Share physician / clinical CEO, 2010, by owner and source ==\n")
  print(cb %>% filter(himss_year == 2010) %>% group_by(owner, c_source) %>%
          summarise(n = n(), c_ceo_md = round(mean(c_ceo_md), 3), c_ceo_clinical = round(mean(c_ceo_clinical), 3), .groups = "drop"))
  # type year per CCN: 2010, else 2009 (as in analysis02)
  ty <- cb %>% filter(himss_year %in% TYPE_YEARS7) %>% arrange(ccn, match(himss_year, TYPE_YEARS7)) %>% group_by(ccn) %>% slice(1) %>% ungroup()
  xt <- function(d, a, b, lab) {
    cat("\n", lab, "\n", sep = "")
    d %>% filter(!is.na(.data[[a]]), !is.na(.data[[b]])) %>% mutate(cell = paste0(a, "=", as.integer(.data[[a]]), " ", b, "=", as.integer(.data[[b]]))) %>%
      count(c_source, cell) %>% pivot_wider(names_from = cell, values_from = n, values_fill = 0) %>% print(width = Inf)
  }
  ty990 <- ty %>% filter(owner == "990 filer")
  xt(ty990, "md990",   "c_ceo_md",       "== C3. Type year, nonprofits: physician CEO, 990 measure vs combined ==")
  xt(ty990, "clin990", "c_ceo_clinical", "== C4. Type year, nonprofits: clinical CEO, 990 measure vs combined ==")
  xt(ty,    "h_md_old",   "c_ceo_md",       "== C5. Type year, all hospitals: physician CEO, HIMSS measure vs combined ==")
  xt(ty,    "h_clin_old", "c_ceo_clinical", "== C6. Type year, all hospitals: clinical CEO, HIMSS measure vs combined ==")
  cat("\n== C7. Nonprofit CCNs with a 990 CEO type in the type years: also typed by the combined measure? ==\n")
  print(h990 %>% filter(himss_year %in% TYPE_YEARS7, !is.na(md990)) %>% distinct(ccn) %>%
          mutate(has_combined = ccn %in% ty$ccn) %>% count(has_combined))
  cat("\n== C8. Type year: specialty of physician CEOs (combined measure), by owner ==\n")
  print(ty %>% filter(c_ceo_md %in% 1) %>% count(owner, c_ceo_specialty = coalesce(c_ceo_specialty, "(none recorded)"), sort = TRUE), n = 60)
  sink(); close(out)
  
  # ---- hand-check file: nonprofit type-year cases where the combined measure changes the type, or both say physician but different people ----
  ceo_nm990 <- ex7 %>% filter(position == "ceo", !is.na(name_cleaned)) %>% group_by(Filer.EIN, TaxYr) %>%
    summarise(ceo_990 = paste(unique(paste0(name_cleaned, if_else(doc %in% TRUE, " [doctor]", ""), if_else(nurse %in% TRUE, " [nurse]", ""),
                                            if_else(!is.na(spec), paste0(" [", spec, "]"), ""))), collapse = "; "), .groups = "drop")
  hceo <- ct7 %>% filter(slot == "CEO") %>% group_by(ccn, himss_year, pid) %>%
    summarise(hosp_name = first_nn(hosp_name), State = first_nn(State), hosp_uid = first_nn(hosp_uid),
              himss_title = paste(unique(na.omit(Title)), collapse = "; "), .groups = "drop")
  hoth <- ct7 %>% filter(slot %in% c("CFO", "COO", "CIO", "CNO", "CMIO")) %>% group_by(ccn, himss_year) %>%
    summarise(himss_other_execs = paste(unique(paste0(slot, ": ", FirstName, " ", LastName)), collapse = "; "), .groups = "drop")
  chk <- ty990 %>%
    mutate(change_md = !is.na(md990) & md990 != c_ceo_md, change_clinical = !is.na(clin990) & clin990 != c_ceo_clinical,
           both_md_diff_person = md990 %in% 1 & c_ceo_md %in% 1 & !(ceo_same_990 %in% TRUE)) %>%
    filter(change_md | change_clinical | both_md_diff_person) %>%
    left_join(ceo_nm990, by = c("Filer.EIN", "himss_year" = "TaxYr")) %>%
    left_join(hceo, by = c("ccn", "himss_year", "ceo_pid" = "pid")) %>% left_join(hoth, by = c("ccn", "himss_year")) %>%
    arrange(desc(change_md), desc(change_clinical), State, hosp_name) %>%
    transmute(ccn, Filer.EIN, himss_year, hosp_name, State, change_md, change_clinical, both_md_diff_person,
              ceo_990, md990, clin990,
              himss_ceo = ceo_name, himss_title, himss_creds = h_creds, himss_ceo_on_990_as = pos990, himss_ceo_990_name = name990,
              same_ceo = ceo_same_990, c_source, c_ceo_md, c_ceo_clinical, c_ceo_specialty, c_ceo_specialty_src, c_ceo_npi, himss_other_execs,
              himss_uid_hospital = hosp_uid, himss_uid_person = ceo_pid,
              propublica = paste0("https://projects.propublica.org/nonprofits/organizations/", str_pad(str_remove_all(as.character(Filer.EIN), "\\D"), 9, pad = "0")),
              true_ceo_name = NA_character_, check_md = NA_integer_, check_clinical = NA_integer_, check_specialty = NA_character_,
              evidence_url = NA_character_, notes = NA_character_)
  write_csv(chk, paste0(created_data_path, "/ceo_handcheck.csv"))
  cat("Wrote himss_ceo_combined.rds, ceo_combined_diagnostics.txt and ceo_handcheck.csv (", nrow(chk), "cases:",
      sum(chk$change_md), "physician-type changes,", sum(chk$change_clinical & !chk$change_md), "clinical-only changes,",
      sum(chk$both_md_diff_person & !chk$change_md & !chk$change_clinical), "both-physician-different-person )\n")
}

# =============================== PART 8: system CEO (the CEO at the top of the hospital's HIMSS parent chain) =============
# Pre-specified 3 Nov (DECISIONS.md) to test whether the leader who matters for a system hospital is the SYSTEM's CEO rather than the
# hospital's own CEO. For each general hospital-year: walk the HIMSS ParentId chain upwards (up to 8 levels); the system CEO is the CEO
# (CEO role slot) of the HIGHEST ancestor that lists one. Physician / clinical status: the validated 990 doctor flag when that person is on
# a 990 of any of the system's hospitals (same year, name keys), else the HIMSS credential; clinical adds a nurse in either source.
# Needs PARTS 1-4 and PART 7 in the same session (uses ent, people, gen, hy, p990).  Outputs (CreatedData): himss_system_ceo.rds,
# himss_system_diagnostics.txt, himss_system_ceo_2010.csv (one row per system in 2010, to eyeball).
RUN_SYSTEM <- TRUE
if (RUN_SYSTEM) {
  out <- file(paste0(created_data_path, "/himss_system_diagnostics.txt"), open = "wt"); sink(out, split = TRUE)
  e8 <- ent %>% distinct(himss_year, HAEntityId, .keep_all = TRUE) %>%
    select(himss_year, HAEntityId, hosp_uid, ParentId, ename = hosp_name, etype = HAEntityType, ccn)
  cat("== S0. HIMSS entity types ==\n"); print(count(e8, etype, sort = TRUE), n = 30)
  pp <- e8 %>% filter(!is.na(ParentId))
  m_id  <- mean(paste(pp$himss_year, pp$ParentId) %in% paste(e8$himss_year, e8$HAEntityId))
  m_uid <- mean(paste(pp$himss_year, pp$ParentId) %in% paste(e8$himss_year, e8$hosp_uid))
  KEY8 <- if (m_id >= m_uid) "HAEntityId" else "hosp_uid"
  cat("Entities with a ParentId:", nrow(pp), "| ParentId found as HAEntityId:", round(m_id, 3), "| as UniqueId:", round(m_uid, 3), "-> using", KEY8, "\n")
  par8 <- e8 %>% transmute(himss_year, child = .data[[KEY8]], parent = ParentId) %>% filter(!is.na(child), !is.na(parent), parent != child) %>%
    distinct(himss_year, child, .keep_all = TRUE)
  
  # ancestors of every general hospital-year
  hos8 <- e8 %>% filter(etype == "Hospital", !is.na(ccn)) %>% semi_join(gen, by = c("ccn", "himss_year")) %>%
    transmute(himss_year, ccn, node = .data[[KEY8]], lvl = 0L) %>% distinct()
  chain <- hos8; cur <- hos8
  for (k in 1:8) {
    cur <- cur %>% inner_join(par8, by = c("himss_year", "node" = "child")) %>% transmute(himss_year, ccn, node = parent, lvl = k) %>%
      anti_join(chain, by = c("himss_year", "ccn", "node")) %>% distinct()
    if (!nrow(cur)) break
    chain <- bind_rows(chain, cur)
  }
  info8 <- e8 %>% transmute(himss_year, node = .data[[KEY8]], s_uid = hosp_uid, s_name = ename, s_type = etype) %>% distinct(himss_year, node, .keep_all = TRUE)
  cat("\n== S1. Depth of the parent chain (general hospital-years) ==\n")
  print(chain %>% group_by(himss_year, ccn) %>% summarise(depth = max(lvl), .groups = "drop") %>% count(depth))
  cat("\nTypes of ancestor entities:\n")
  print(chain %>% filter(lvl > 0) %>% left_join(info8, by = c("himss_year", "node")) %>% count(lvl, s_type) %>% arrange(lvl, desc(n)), n = 40)
  
  # CEOs at every entity
  rn_regex <- "\\bRN\\b|\\bBSN\\b|\\bMSN\\b|\\bDNP\\b|\\bNP\\b|\\bAPRN\\b|\\bCRNA\\b"
  ceo_e <- people %>% filter(TypeId == "1", Status == "Active", !is.na(ContactId), !is.na(LastName), exec_slot(role) == "CEO") %>%
    mutate(node = .data[[KEY8]], cr = str_to_upper(coalesce(Credentials, "")),
           c_md = str_detect(cr, md_regex) | str_detect(coalesce(Title, ""), md_regex), c_rn = str_detect(cr, rn_regex),
           spid = coalesce(person_uid, paste0("n_", node, "_", str_to_lower(paste(FirstName, LastName)))))
  pcred <- ceo_e %>% group_by(spid) %>% summarise(s_cred_md = any(c_md), s_cred_rn = any(c_rn), s_creds = paste(unique(na.omit(Credentials)), collapse = "; "), .groups = "drop")
  ceo_node <- ceo_e %>% group_by(himss_year, node) %>%
    summarise(s_pid = first_nn(spid), s_first = first_nn(FirstName), s_last = first_nn(LastName), s_title = first_nn(Title), .groups = "drop")
  
  # system CEO = CEO of the highest ancestor that lists one
  sysc <- chain %>% filter(lvl > 0) %>% inner_join(ceo_node, by = c("himss_year", "node")) %>% left_join(info8, by = c("himss_year", "node")) %>%
    group_by(himss_year, ccn) %>% slice_max(lvl, n = 1, with_ties = FALSE) %>% ungroup() %>% rename(s_lvl = lvl)
  member8 <- chain %>% group_by(himss_year, ccn) %>% summarise(s_member = any(lvl > 0), .groups = "drop")
  
  # 990 link: the system CEO on a 990 of any of the system's hospitals in the same year
  sys_ein <- sysc %>% inner_join(gen %>% filter(owner == "990 filer") %>% select(ccn, himss_year, Filer.EIN), by = c("ccn", "himss_year")) %>%
    distinct(s_pid, himss_year, Filer.EIN, s_first, s_last)
  slk <- sys_ein %>% mutate(key = himss_keys(s_first, s_last)) %>% select(s_pid, himss_year, Filer.EIN, key) %>% unnest(key) %>%
    inner_join(p990, by = c("Filer.EIN", "himss_year" = "TaxYr", "key"), relationship = "many-to-many") %>%
    group_by(s_pid) %>% summarise(s_linked = TRUE, s_doc990 = any(doc %in% TRUE), s_nurse990 = any(nurse %in% TRUE),
                                  s_pos990 = paste(sort(unique(position)), collapse = "; "), s_spec = first_nn(spec), .groups = "drop")
  
  hceo8 <- hy %>% distinct(ccn, himss_year, .keep_all = TRUE) %>% select(ccn, himss_year, ceo_pid, ceo_first, ceo_last)
  sy <- member8 %>% left_join(sysc, by = c("himss_year", "ccn")) %>% left_join(slk, by = "s_pid") %>% left_join(pcred, by = c("s_pid" = "spid")) %>%
    left_join(hceo8, by = c("ccn", "himss_year")) %>%
    mutate(s_linked = coalesce(s_linked, FALSE),
           s_src = case_when(!s_member ~ NA_character_, is.na(s_pid) ~ "no CEO at any parent", s_linked ~ "990 link", TRUE ~ "HIMSS credential"),
           s_ceo_md       = as.integer(if_else(s_linked, s_doc990, coalesce(s_cred_md, FALSE))),
           s_ceo_clinical = as.integer(s_ceo_md == 1 | s_nurse990 %in% TRUE | s_cred_rn %in% TRUE),
           across(c(s_ceo_md, s_ceo_clinical), ~ if_else(is.na(s_pid), NA_integer_, .x)),
           s_same_hosp = as.integer(!is.na(s_pid) & !is.na(ceo_pid) & (s_pid == ceo_pid | same_name(s_first, s_last, ceo_first, ceo_last))),
           s_ceo_name = if_else(is.na(s_pid), NA_character_, paste(s_first, s_last))) %>%
    select(ccn, himss_year, s_member, s_uid, s_name, s_type, s_lvl, s_ceo_name, s_title, s_creds, s_pos990, s_src, s_ceo_md, s_ceo_clinical, s_spec, s_same_hosp)
  saveRDS(sy, paste0(created_data_path, "/himss_system_ceo.rds"))
  
  gs <- gen %>% select(ccn, himss_year, owner, independent) %>% left_join(sy, by = c("ccn", "himss_year")) %>% mutate(s_member = coalesce(s_member, FALSE))
  cat("\n== S2. System membership: HIMSS parent chain vs AHA (independent = no AHA SYSID), general hospital-years ==\n")
  print(gs %>% count(owner, aha_independent = independent, himss_member = s_member) %>% group_by(owner) %>% mutate(share = round(n / sum(n), 3)), n = 20)
  cat("\n== S3. System members: where the system CEO's status comes from, and share physician / clinical, 2010 ==\n")
  print(gs %>% filter(himss_year == 2010, s_member) %>% group_by(owner, s_src) %>%
          summarise(n = n(), s_ceo_md = round(mean(s_ceo_md, na.rm = TRUE), 3), s_ceo_clinical = round(mean(s_ceo_clinical, na.rm = TRUE), 3),
                    same_as_hospital_ceo = round(mean(s_same_hosp, na.rm = TRUE), 3), .groups = "drop"))
  cat("\n== S4. 2010 system members, nonprofits: hospital CEO (combined) x system CEO physician ==\n")
  cmb8 <- read_rds(paste0(created_data_path, "/himss_ceo_combined.rds")) %>% select(ccn, himss_year, c_ceo_md, c_ceo_clinical)
  print(gs %>% filter(himss_year == 2010, s_member, owner == "990 filer") %>% left_join(cmb8, by = c("ccn", "himss_year")) %>%
          count(hospital_ceo_md = c_ceo_md, system_ceo_md = s_ceo_md, same_person = s_same_hosp))
  # does the 990's assigned CEO turn out to be the HIMSS system CEO where the two sources disagree? (needs PART 5 objects)
  if (exists("f990") && exists("cy")) {
    sk <- sysc %>% mutate(key = himss_keys(s_first, s_last)) %>% select(ccn, himss_year, key) %>% unnest(key) %>% distinct()
    hit8 <- f990 %>% inner_join(sk, by = c("ccn", "TaxYr" = "himss_year", "key")) %>% distinct(ccn, TaxYr) %>% mutate(is_sys = TRUE)
    cat("\n== S5. 990 filers: is the 990's assigned CEO the HIMSS system CEO? by whether 990 and HIMSS name the same hospital CEO ==\n")
    print(cy %>% left_join(hit8, by = c("ccn", "TaxYr")) %>% semi_join(sysc, by = c("ccn", "TaxYr" = "himss_year")) %>%
            group_by(same_hospital_ceo = agree) %>% summarise(ccn_years = n(), ceo990_is_system_ceo = round(mean(is_sys %in% TRUE), 3)))
    # per hospital-year: why the 990 and HIMSS name different CEOs (PART 5 classes) + whether the 990 CEO is the HIMSS system CEO -> bridge in analysis02
    if (exists("mm")) {
      dcls <- cy %>% select(ccn, TaxYr, agree) %>% left_join(mm %>% select(ccn, TaxYr, class) %>% distinct(ccn, TaxYr, .keep_all = TRUE), by = c("ccn", "TaxYr")) %>%
        left_join(hit8, by = c("ccn", "TaxYr")) %>%
        transmute(ccn, himss_year = TaxYr, d_class = if_else(agree, "0 same CEO", class), d_is_sys = as.integer(is_sys %in% TRUE))
      saveRDS(dcls, paste0(created_data_path, "/himss_ceo_disagreement.rds"))
      cat("\nSaved himss_ceo_disagreement.rds:\n"); print(count(dcls, d_class, d_is_sys))
    }
  }
  sink(); close(out)
  # one row per system in 2010, for eyeballing (largest first)
  gs %>% filter(himss_year == 2010, s_member, !is.na(s_ceo_name)) %>%
    group_by(s_uid, s_name, s_type, s_ceo_name, s_title, s_creds, s_pos990, s_src, s_ceo_md, s_ceo_clinical, s_spec) %>%
    summarise(hospitals = n(), nonprofit = sum(owner == "990 filer"), .groups = "drop") %>% arrange(desc(hospitals)) %>%
    write_csv(paste0(created_data_path, "/himss_system_ceo_2010.csv"))
  cat("Wrote himss_system_ceo.rds, himss_system_diagnostics.txt, himss_system_ceo_2010.csv\n")
}