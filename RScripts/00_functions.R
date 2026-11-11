### Shared functions for the physician-hospital project.
### Source this at the top of scripts 2-8:  source("./Scripts/00_functions.R")
### (the leadership build in script 1 keeps its own copies; if you change a function here
###  that script 1 also defines, change it there too so the two stay identical)

library(dplyr)
library(stringr)
library(tidyr)
library(tibble)

# ---- Key conventions used across the project -----------------------------------
# npi  : character, 10 digits
# ID   : character, AHA hospital ID (7 digits)
# ein  : character, 9 digits, leading zeros kept
# year : integer calendar year (the leadership file calls it TaxYr, MDPPAS calls it Year;
#        rename to `year` when you bring them into the assembly script)

# ---- CCN / names ----------------------------------------------------------------
pad_ccn <- function(x) {
  x <- str_trim(as.character(x))
  x <- str_remove(x, "\\.0+$")
  ifelse(!is.na(x) & str_detect(x, "^[0-9]+$"), str_pad(x, 6, pad = "0"), x)
}

clean_name <- function(x) {
  x %>%
    str_to_lower() %>%
    str_replace_all("[’']", "") %>%
    str_replace_all("&", " and ") %>%
    str_replace_all("[[:punct:]]", " ") %>%
    # standardise common abbreviations
    str_replace_all("\\bhosp\\b", "hospital") %>%
    str_replace_all("\\bctr\\b", "center") %>%
    str_replace_all("\\bcenter\\b|\\bcentre\\b", "center") %>%
    str_replace_all("\\bmed\\b", "medical") %>%
    str_replace_all("\\buniv\\b", "university") %>%
    str_replace_all("\\bst\\b", "saint") %>%
    str_replace_all("\\bmem\\b", "memorial") %>%
    str_replace_all("\\bsys\\b", "system") %>%
    # drop legal-form noise
    str_replace_all("\\b(inc|incorporated|corp|corporation|llc|llp|pc|pllc|pa|ltd|co|company|the|of|dba|d b a|fka)\\b", " ") %>%
    str_squish()
}

# split "X d/b/a Y" (and a/k/a, f/k/a) into two names; run on the RAW name
split_alias <- function(x) {
  m <- str_match(x, regex("^(.*?)\\s*\\b(?:d/b/a|dba|a/k/a|f/k/a)\\b\\s*(.*)$",
                          ignore_case = TRUE))
  tibble(main = coalesce(m[, 2], x), alt = na_if(m[, 3], ""))
}

# ---- Specialty groups (identical to the leadership script) ----------------------
specialty_group_fn <- function(x) {
  x <- str_to_lower(x)
  case_when(
    is.na(x) ~ NA_character_,
    # hospital-based / acute: no outpatient panel of their own
    str_detect(x, "emergency|anesth|anaesth|radiolog|patholog|nuclear|critical care|hospitalist") ~ "hospital_based",
    # surgical + procedural (OB/GYN, ophthalmology, ENT, urology grouped here -- judgement call)
    str_detect(x, "surg|orthop|\\burolog|otolaryn|ophthalm|obstet|gynec") ~ "surgical",
    # psychiatry & neurology
    str_detect(x, "psychiatr|neurolog|behavior") ~ "psych_neuro",
    # generalists
    str_detect(x, "family|general practice|pediatric|peditat|geriatric|preventive") ~ "primary_care",
    # internal medicine and its subspecialties
    str_detect(x, "internal|gastro|cardio|nephro|pulmon|endocrin|oncolog|hematolog|rheumat|infectious") ~ "internal_medicine",
    TRUE ~ "other_medical"   # dermatology, PM&R, allergy, genetics, pain, etc.
  )
}

spec_groups <- c("primary_care", "internal_medicine", "surgical",
                 "hospital_based", "psych_neuro", "other_medical")

# ---- Small helpers --------------------------------------------------------------
safe_max  <- function(x) if (length(x) == 0 || all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)
safe_mean <- function(x) if (length(x) == 0 || all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
safe_sum  <- function(x) if (length(x) == 0 || all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)
first_non_na <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) NA_character_ else as.character(x[1])
}

# stop the script if a table is not unique on the given keys, e.g. assert_unique(df, npi, year)
assert_unique <- function(df, ...) {
  n_dup <- df %>% count(..., name = ".n") %>% filter(.n > 1) %>% nrow()
  if (n_dup > 0) stop("Table is not unique on the given keys: ", n_dup, " duplicated key combinations")
  invisible(df)
}

# ---- Event variable: persistent gain ----------------------------------------------
# A "gain" in year t means the trait was 0 in t-1 and 1 in BOTH t and t+1
# (needs 3 consecutive years of data, otherwise NA). Expects Filer.EIN and TaxYr columns.
persistent_gain <- function(df, var, out) {
  df %>%
    arrange(Filer.EIN, TaxYr) %>%
    group_by(Filer.EIN) %>%
    mutate(.v = .data[[var]],
           .ok = (TaxYr - lag(TaxYr)) %in% 1 & (lead(TaxYr) - TaxYr) %in% 1 &
             !is.na(lag(.v)) & !is.na(.v) & !is.na(lead(.v)),
           !!out := if_else(.ok, as.integer(lag(.v) == 0 & .v == 1 & lead(.v) == 1), NA_integer_)) %>%
    ungroup() %>%
    select(-.v, -.ok)
}
# ---- Fine specialty groups (20), from the NPI taxonomy code; used by scripts 6b, 6c, 6g, 7 ------------------------------
fine_groups <- c("cardiology", "gastroenterology", "hem_onc", "pulm_critcare", "nephrology", "endo_rheum_id", "general_im",
                 "family_gp", "pediatrics", "general_surgery", "other_surgery", "orthopedics", "ob_gyn", "ophth_ent_uro",
                 "neurology", "psychiatry", "anesthesia", "radiology_pathology", "emergency_hospitalist", "other_medical")

# ---- 1. Taxonomy code -> fine group ---------------------------------------------------------------
fine_from_code <- function(code) {
  x <- str_to_upper(str_trim(code))
  case_when(
    is.na(x) ~ NA_character_,
    # exact codes first (several internal medicine subspecialties share the 207R prefix)
    x == "207RC0200X"                                        ~ "pulm_critcare",
    x %in% c("207RI0011X", "207RA0001X")                     ~ "cardiology",
    x %in% c("207RI0008X", "207RT0003X")                     ~ "gastroenterology",
    x %in% c("2085R0001X")                                   ~ "hem_onc",             # radiation oncology
    x %in% c("2084N0400X", "2084N0402X", "2084N0600X", "2084V0102X", "2084E0001X", "2084D0003X") ~ "neurology",
    # internal medicine and its subspecialties
    str_starts(x, "207RC")                                   ~ "cardiology",
    str_starts(x, "207RG0100")                               ~ "gastroenterology",
    x %in% c("207RH0000X", "207RH0003X", "207RX0202X")      ~ "hem_onc",
    str_starts(x, "207RP")                                   ~ "pulm_critcare",
    str_starts(x, "207RN")                                   ~ "nephrology",
    x %in% c("207RE0101X", "207RR0500X", "207RI0200X", "207RI0001X") ~ "endo_rheum_id",
    str_starts(x, "207R")                                    ~ "general_im",          # general, geriatrics, palliative, sleep, sports, ...
    # generalists and pediatrics
    str_starts(x, "207Q") | str_starts(x, "208D")            ~ "family_gp",
    str_starts(x, "2080")                                    ~ "pediatrics",
    # surgery
    str_starts(x, "2086")                                    ~ "general_surgery",
    str_starts(x, "207T") | str_starts(x, "208G") | str_starts(x, "2082") | str_starts(x, "208C") |
      str_starts(x, "204F") | str_starts(x, "204E")          ~ "other_surgery",       # neuro, thoracic, plastic, colorectal, transplant, oral
    str_starts(x, "207X")                                    ~ "orthopedics",
    str_starts(x, "207V")                                    ~ "ob_gyn",
    str_starts(x, "207W") | str_starts(x, "207Y") | str_starts(x, "2088") ~ "ophth_ent_uro",
    # psychiatry and neurology (neurology codes were matched above)
    str_starts(x, "2084")                                    ~ "psychiatry",
    # hospital-based
    str_starts(x, "207L") | str_starts(x, "208VP")           ~ "anesthesia",
    str_starts(x, "2085") | str_starts(x, "207Z") | str_starts(x, "207U") ~ "radiology_pathology",
    str_starts(x, "207P") | str_starts(x, "208M")            ~ "emergency_hospitalist",
    # remaining physician codes: dermatology, PM&R, preventive, allergy, genetics, ...
    str_starts(x, "20") | str_starts(x, "17")                ~ "other_medical",
    TRUE                                                     ~ NA_character_)
}

# fallback for executives with no NPI (specialty comes from the review notes or the class label)
fine_from_label <- function(s) {
  x <- str_to_lower(s)
  case_when(
    is.na(x) ~ NA_character_,
    str_detect(x, "critical care|pulmon")                    ~ "pulm_critcare",
    str_detect(x, "cardiothoracic|thoracic|neurological surg|plastic|colon|rectal|transplant") ~ "other_surgery",
    str_detect(x, "cardio")                                  ~ "cardiology",
    str_detect(x, "gastro")                                  ~ "gastroenterology",
    str_detect(x, "oncolog|hematolog|radiation")             ~ "hem_onc",
    str_detect(x, "nephro")                                  ~ "nephrology",
    str_detect(x, "endocrin|rheumat|infectious")             ~ "endo_rheum_id",
    str_detect(x, "internal")                                ~ "general_im",
    str_detect(x, "family|general practice")                 ~ "family_gp",
    str_detect(x, "pediatr|peditat")                         ~ "pediatrics",
    str_detect(x, "orthop")                                  ~ "orthopedics",
    str_detect(x, "surg")                                    ~ "general_surgery",
    str_detect(x, "obstet|gynec")                            ~ "ob_gyn",
    str_detect(x, "ophthal|otolaryn|urolog")                 ~ "ophth_ent_uro",
    str_detect(x, "neurolog")                                ~ "neurology",
    str_detect(x, "psychiatr|behavior")                      ~ "psychiatry",
    str_detect(x, "anesth|anaesth|pain")                     ~ "anesthesia",
    str_detect(x, "radiolog|patholog|nuclear")               ~ "radiology_pathology",
    str_detect(x, "emergency|hospitalist")                   ~ "emergency_hospitalist",
    TRUE                                                     ~ "other_medical")
}


# ---- Mid-level specialty groups (9): an in-between aggregation of the 20 fine groups (judgement call; used by script 6g counts) ----------------
mid_groups <- c("emergency_hospitalist", "critical_care_pulm", "anesth_rad_path", "generalist_adult", "pediatrics",
                "medical_subspecialty", "surgical_procedural", "psych_neuro", "other_medical")
mid_from_fine <- function(g) {
  case_when(
    is.na(g) ~ NA_character_,
    g == "emergency_hospitalist"                                                        ~ "emergency_hospitalist",
    g == "pulm_critcare"                                                                ~ "critical_care_pulm",
    g %in% c("anesthesia", "radiology_pathology")                                       ~ "anesth_rad_path",
    g %in% c("general_im", "family_gp")                                                 ~ "generalist_adult",
    g == "pediatrics"                                                                   ~ "pediatrics",
    g %in% c("cardiology", "gastroenterology", "nephrology", "endo_rheum_id", "hem_onc") ~ "medical_subspecialty",
    g %in% c("general_surgery", "other_surgery", "orthopedics", "ob_gyn", "ophth_ent_uro") ~ "surgical_procedural",
    g %in% c("psychiatry", "neurology")                                                 ~ "psych_neuro",
    TRUE                                                                                ~ "other_medical")
}

# ====================================================================================================
# PASTE THIS BLOCK AT THE END OF 00_functions.R
# Three specialty groupings, each defined from the 20 fine groups (fine_groups, fine_from_code(), fine_from_label() above).
# The logic of each grouping is a different answer to "who counts as the same group as the executive?":
#   resource     : physicians who draw on the same scarce hospital capacity (OR, cath lab, ICU/ED, imaging, beds, clinic slots).  Hypothesis H1 (capacity)
#   patient      : physicians who treat the same patients, as referral partners or by population (organ system, age, sex).         Hypothesis H2 (referrals)
#   professional : physicians with the same training and board identity (shared norms, peers, outside option).                    Hypotheses H5, H6
# These are JUDGEMENT CALLS made at the 20-fine-group level. Where a fine group mixes things (other_surgery = neuro / thoracic / plastic /
# colorectal / transplant; ophth_ent_uro; endo_rheum_id; emergency_hospitalist; radiology_pathology; other_medical) the whole fine group goes to one place.
# Splitting them needs finer taxonomy codes in fine_from_code().
# ====================================================================================================

# ---- RESOURCE (9 groups) -----------------------------------------------------------------------------
resource_groups <- c("operating_room", "cardiac", "acute_care", "imaging_lab", "oncology", "psych_neuro",
                     "women_children", "procedural_medicine", "office_based")
resource_from_fine <- function(g) {
  case_when(
    is.na(g) ~ NA_character_,
    g %in% c("general_surgery", "other_surgery", "orthopedics", "ophth_ent_uro", "anesthesia") ~ "operating_room",     # OR time, surgical beds (anesthesia shares the OR)
    g == "cardiology"                                                                          ~ "cardiac",            # cath lab, cardiac ICU
    g %in% c("pulm_critcare", "emergency_hospitalist")                                          ~ "acute_care",         # ICU, ED, inpatient wards
    g == "radiology_pathology"                                                                  ~ "imaging_lab",        # MRI / CT / PET, laboratory
    g == "hem_onc"                                                                              ~ "oncology",           # infusion, radiation therapy
    g %in% c("psychiatry", "neurology")                                                         ~ "psych_neuro",        # psychiatric beds, neuro units
    g %in% c("ob_gyn", "pediatrics")                                                            ~ "women_children",     # obstetric beds, NICU, pediatric beds
    g %in% c("gastroenterology", "nephrology")                                                  ~ "procedural_medicine", # endoscopy suites, dialysis
    g %in% c("general_im", "family_gp", "endo_rheum_id", "other_medical")                       ~ "office_based",       # clinic slots; little hospital capacity
    TRUE                                                                                        ~ NA_character_)
}

# ---- PATIENT (9 groups) ------------------------------------------------------------------------------
patient_groups <- c("adult_primary", "children", "women", "cardiovascular", "cancer", "neuro_mental",
                    "surgical_patients", "acute_inpatient", "medical_other")
patient_from_fine <- function(g) {
  case_when(
    is.na(g) ~ NA_character_,
    g %in% c("general_im", "family_gp")                                                         ~ "adult_primary",
    g == "pediatrics"                                                                           ~ "children",
    g == "ob_gyn"                                                                               ~ "women",
    g == "cardiology"                                                                           ~ "cardiovascular",     # cardiac surgery sits inside other_surgery and cannot be split here
    g == "hem_onc"                                                                              ~ "cancer",
    g %in% c("psychiatry", "neurology")                                                         ~ "neuro_mental",
    g %in% c("general_surgery", "other_surgery", "orthopedics", "ophth_ent_uro", "anesthesia",
             "radiology_pathology")                                                              ~ "surgical_patients", # anesthesia and radiology/pathology have no patients of their own: they serve the proceduralists' patients
    g %in% c("emergency_hospitalist", "pulm_critcare")                                          ~ "acute_inpatient",
    g %in% c("gastroenterology", "nephrology", "endo_rheum_id", "other_medical")                ~ "medical_other",
    TRUE                                                                                        ~ NA_character_)
}

# ---- PROFESSIONAL (10 groups) --------------------------------------------------------------------------
professional_groups <- c("internal_medicine", "family_medicine", "pediatrics", "surgery", "ob_gyn", "psych_neuro",
                         "anesthesiology", "radiology_pathology", "emergency_hospitalist", "other_medical")
professional_from_fine <- function(g) {
  case_when(
    is.na(g) ~ NA_character_,
    g %in% c("general_im", "cardiology", "gastroenterology", "hem_onc", "pulm_critcare", "nephrology", "endo_rheum_id") ~ "internal_medicine",  # internists and their fellowships
    g == "family_gp"                                                                            ~ "family_medicine",
    g == "pediatrics"                                                                           ~ "pediatrics",
    g %in% c("general_surgery", "other_surgery", "orthopedics", "ophth_ent_uro")                ~ "surgery",
    g == "ob_gyn"                                                                               ~ "ob_gyn",
    g %in% c("psychiatry", "neurology")                                                         ~ "psych_neuro",
    g == "anesthesia"                                                                           ~ "anesthesiology",
    g == "radiology_pathology"                                                                  ~ "radiology_pathology",
    g == "emergency_hospitalist"                                                                ~ "emergency_hospitalist",
    g == "other_medical"                                                                        ~ "other_medical",
    TRUE                                                                                        ~ NA_character_)
}

# one entry point: scheme_from_fine(g, "resource")
scheme_from_fine <- function(g, scheme) switch(scheme, resource = resource_from_fine(g), patient = patient_from_fine(g), professional = professional_from_fine(g))