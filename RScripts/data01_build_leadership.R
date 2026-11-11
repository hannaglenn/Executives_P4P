library(readr)
library(stringr)
library(dplyr)
library(gender)
library(httr2)
library(jsonlite)
library(tibble)
library(purrr)
library(tidyr)
library(data.table)
library(igraph)
library(fuzzyjoin)
library(ggplot2)
library(lubridate)
library(readxl)
library(writexl)
library(rlang)
library(stringdist)
library(igraph)

source("./Scripts/paths.R") 

xml_data <- readRDS(paste0(created_data_path, "/xml_people_data_new(with_ind).rds")) %>%
  rename(PersonNm = person_name, TitleTxt = title, TaxYr = end_year, Filer.EIN=ein) %>%
  filter(PersonNm != "NONE" & !is.na(PersonNm))
ein_data <- readRDS(paste0(created_data_path, "/xml_ein_data_new(with_ind).rds"))

fiscal_check <- ein_data %>%
  mutate(
    beg_dt = as.Date(beg_dt),
    end_dt = as.Date(end_dt),
    fiscal_days = as.numeric(end_dt - beg_dt) + 1,
    
    # Days of the fiscal period falling in each calendar year
    # (assumes fiscal periods don't span more than 2 calendar years,
    # true for any ~12-month period)
    days_in_end_year = as.numeric(
      end_dt - pmax(beg_dt, as.Date(paste0(year(end_dt), "-01-01"))) + 1
    ),
    days_in_prior_year = fiscal_days - days_in_end_year,
    
    majority_year = ifelse(days_in_end_year >= days_in_prior_year,
                           year(end_dt),
                           year(end_dt) - 1),
    
    label_matches_majority = (end_year == majority_year)
  )

# How often does the current label mismatch the "true" majority-days year?
fiscal_check %>%
  count(label_matches_majority) %>%
  mutate(pct = n / sum(n) * 100)

# Look at the mismatches specifically
mismatches <- fiscal_check %>%
  filter(!label_matches_majority) %>%
  select(ein, org_name, beg_dt, end_dt, end_year, majority_year,
         days_in_end_year, days_in_prior_year)

print(mismatches)

# How severe is the mismatch typically? (e.g. is it close to 50/50 or way off)
fiscal_check %>%
  filter(!label_matches_majority) %>%
  summarise(
    n = n(),
    avg_days_in_labeled_year = mean(days_in_end_year),
    avg_days_in_majority_year = mean(days_in_prior_year)
  )

fiscal_check %>%
  filter(!label_matches_majority) %>%
  mutate(day_diff = abs(days_in_end_year - days_in_prior_year)) %>%
  summarise(
    pct_close_call = mean(day_diff <= 15) * 100,
    pct_lopsided = mean(day_diff > 60) * 100
  )

ein_recode_flag <- ein_data %>%
  mutate(
    beg_dt = as.Date(beg_dt),
    end_dt = as.Date(end_dt),
    fiscal_days = as.numeric(end_dt - beg_dt) + 1,
    
    days_in_end_year = as.numeric(
      end_dt - pmax(beg_dt, as.Date(paste0(year(end_dt), "-01-01"))) + 1
    ),
    days_in_prior_year = fiscal_days - days_in_end_year,
    
    majority_year = ifelse(days_in_end_year >= days_in_prior_year,
                           year(end_dt),
                           year(end_dt) - 1),
    
    day_diff = abs(days_in_end_year - days_in_prior_year),
    
    lopsided_mismatch = (end_year != majority_year) 
  ) %>%
  filter(end_year >= 2010, end_year <= 2018) %>%
  group_by(ein) %>%
  summarise(
    n_lopsided = sum(lopsided_mismatch),
    needs_recode = n_lopsided > 1,
    .groups = "drop"
  )

ein_recode_flag %>% count(needs_recode)

### FILING LABELS: label every XML filing by its own fiscal-year dates -------------
# Put this right after ein_recode_flag is created, REPLACING the block that did
#   ein_data <- ein_data %>% left_join(ein_recode_flag ...) %>% mutate(TaxYr = ifelse(needs_recode, end_year - 1, end_year)) ...
# It needs fiscal_check (has ein, end_year, beg_dt, end_dt, fiscal_days, majority_year).

filing_labels <- fiscal_check %>%
  mutate(ein = str_pad(str_trim(as.character(ein)), 9, pad = "0")) %>%
  select(ein, end_year, beg_dt, end_dt, fiscal_days, majority_year) %>%
  distinct() %>%
  group_by(ein, end_year) %>%
  arrange(desc(fiscal_days), desc(end_dt), .by_group = TRUE) %>%
  mutate(filing_rank = row_number(), n_filings = n()) %>%
  ungroup()

# one row per EIN-year: label of the longest (full-year) return, and of the second (stub) return if it gets a different label
ein_year_labels <- filing_labels %>%
  group_by(ein, end_year) %>%
  summarise(
    n_filings  = first(n_filings),
    label_full = majority_year[filing_rank == 1],
    label_stub = ifelse(n_filings > 1 && majority_year[filing_rank == 2] != label_full,
                        majority_year[filing_rank == 2], NA_real_),
    .groups = "drop"
  )

ein_year_labels %>% count(n_filings, has_stub_label = !is.na(label_stub))   # expect ~60 EIN-years with 2 filings

# firm-level table: each filing gets its own label; if two filings land on the same label keep the longer one
ein_data <- ein_data %>%
  mutate(ein = str_pad(str_trim(as.character(ein)), 9, pad = "0"),
         beg_dt = as.Date(beg_dt), end_dt = as.Date(end_dt)) %>%
  left_join(filing_labels %>% select(ein, end_year, beg_dt, end_dt, majority_year, filing_rank),
            by = c("ein", "end_year", "beg_dt", "end_dt")) %>%
  mutate(TaxYr = coalesce(majority_year, end_year)) %>%
  group_by(ein, TaxYr) %>%
  arrange(filing_rank, .by_group = TRUE) %>%
  slice(1) %>%
  ungroup() %>%
  rename(Filer.EIN = ein) %>%
  select(-majority_year, -filing_rank)


### STEP 1: COMBINE OLD AND NEW 990 DATA ---------------------------------

# read in files from manual data extraction
files <- list.files(path = "./CreatedData/manual_990_data/", full.names = TRUE)

early_data <- data.frame()
for (file in files) {
  data <- read_csv(file) %>%
    mutate(ha=as.numeric(ha), return_type = as.character(return_type),
           avg_hours_per_week = as.numeric(avg_hours_per_week),
           avg_hours_per_week_rltd_org = as.numeric(avg_hours_per_week_rltd_org),
           reportable_comp_from_org = as.numeric(reportable_comp_from_org),
           individual_trustee_or_director = as.logical(individual_trustee_or_director),
           institutional_trustee = as.logical(institutional_trustee),
           officer_ind = as.logical(officer_ind),
           key_employee_ind = as.logical(key_employee_ind),
           highest_compensated_employee = as.logical(highest_compensated_employee),
           former_ofcr_director_trustee = as.logical(former_ofcr_director_trustee),
           reportable_comp_from_rltd_org = as.numeric(reportable_comp_from_rltd_org),
           other_compensation = as.numeric(other_compensation),
           doctor = as.numeric(doctor),
           other_doctor = as.numeric(other_doctor),
           nurse = as.numeric(nurse),
           ha = as.numeric(ha),
           Filer.EIN = as.character(Filer.EIN),
           TaxYr = as.numeric(TaxYr))
  
  early_data <- bind_rows(early_data, data)
}
rm(data)

early_data <- early_data %>%
  rename(PersonNm = name_cleaned) 

early_data %>%
  distinct(Filer.EIN) %>%
  count()

# combine data sets 
data <- bind_rows(early_data %>% mutate(from_xml = FALSE),
                  xml_data %>% mutate(from_xml = TRUE))

# make sure EINs are 9 digits in both sources (read_csv reads the manual files' EINs as numbers, which drops leading zeros,
# so "010130427" in the xml data would not match "10130427" in the manual data and the same hospital would split in two)
data <- data %>%
  mutate(Filer.EIN = str_pad(str_trim(as.character(Filer.EIN)), 9, pad = "0"))
data %>% count(nchar(Filer.EIN))   # expect only 9

# year labels
# - XML rows: each filing is labelled by its own dates (filing_labels / ein_year_labels are built in the
#   "FILING LABELS" block that goes right after ein_recode_flag, see data03_filing_labels.R)
# - manual 2009-2012 rows have no filing dates, so they keep the EIN-level recode
data <- data %>%
  mutate(orig_end_year = TaxYr) %>%
  left_join(
    ein_recode_flag %>% select(ein, needs_recode),
    by = c("Filer.EIN" = "ein")
  ) %>%
  left_join(
    ein_year_labels,
    by = c("Filer.EIN" = "ein", "orig_end_year" = "end_year")
  ) %>%
  mutate(
    needs_recode = ifelse(is.na(needs_recode), FALSE, needs_recode),
    use_filing_label = from_xml & !is.na(label_full),
    TaxYr = case_when(
      use_filing_label ~ label_full,
      needs_recode     ~ TaxYr - 1,
      TRUE             ~ TaxYr
    ),
    # two returns in the same end_year that map to the same label (e.g. Oct-Sep year + Oct-Dec stub):
    # the person is on both, so they must be max-ed, not added, when duplicates are combined below
    collapse_max = use_filing_label & n_filings == 2 & is.na(label_stub),
    # two returns that map to different labels: rows get split between the two years below
    split_returns = use_filing_label & n_filings == 2 & !is.na(label_stub)
  )

# two returns from DIFFERENT end_years can also land on the same label (e.g. a full Oct-Sep year plus a 93-day Oct-Jan 1 stub
# whose majority year is the same year): same label = same year, so the same person is combined with max, not added
data <- data %>%
  group_by(Filer.EIN, TaxYr) %>%
  mutate(collapse_max = collapse_max | (n_distinct(orig_end_year) > 1)) %>%
  ungroup()

data %>% filter(from_xml) %>% count(use_filing_label)   # nearly all xml rows should use a filing label
data %>% count(collapse_max, split_returns)

# define lists to look for the name column 
doctor_list <- c("md", "do", "mdmba", "doctor", "mdend", "mdpresident", "mdjd", "physician")
other_doctor_list <- c("mbbch", "mbbs", "dds", "dentist", "od", "pharmd", "pt")
nurse_list <- c("rn", "dnp", "aprn", "crna", "np", "scn", "fnp", "cne", "fnp-c", "apnp", "fnp-", "cenp", "mphrn", "acnp", "msnrnfaan", "crnp", "msnrn",
                "eddrn", "dpn", "drnp", "anp", "edrn", "arnp", "pnp", "cnp", "bcccrn", "crn", "nurse", "arpn", "cnaa", "lpn")
ha_list <- c("mha", "fache", "mhadrph", "mhsa", "mhcm", "mhs", "mhp", "msha")
remove_list <- c("mbbch", "mbbs", "until", "as of", "eff", "end", "beg", "thru", "through", "osb",
                 "ending", "term", "mph", "deceased", "to", "begin", "left", "see statement", "director", "chair", "hired", "termed",
                 "began", "retired", "from", "resigned", "off", "vp", "ended", "started", "trm", "dir of", "pd by", "provost",
                 "start", "oct", "treasurer", "jan", "st", "chief", "t0", "entered", "interim", "board", "fmr", "int", "trustee",
                 "vice president", "dir", "bh", "vppres", "msn", "mst", "assoc", "departed", "msw", "ceo", "cbe", "secretary", "rsmterm",
                 "effective", "effec", "former", "admininstrator", "consultant", "cnm", "fellow", "beginning", "president",
                 "dr", "bishop", "esq", "jr", "sr", "md", "phd", "dds", "ii", "iii", "iv", "ms", "mr", "do","most rev", "rev", "rn", "edd", "cpa", "od",
                 "the rt", "dnp", "bc", "faan", "mb", "chb", "mbbh", "mbbch", "chcio", "rsm", "part year", "family practice", "major general",
                 "sister", "mha", "magistrate", "- see sch o", "see sch o", "sch o", "dc","the very rev", "the most reverend", "the honorable", "the hon",
                 "general", "vice pres", "fr", "mba", "jd", "ma", "bsn", "cco", "chco", "mpas", "pac", "dsc", "cbs", "chs", "ed d",
                 "rd", "bsbn", "mshrm", "cpm", "ccim", "ecc", "home care", "st john", "ph d", "csj", "non voting", "ex officio",
                 "dmd", "ret maj gen", "sc", "pharm d", "int ceo", "msn", "osf", "mother", "rtscra", "dha", "dh", "evpcao", "evpcoo",
                 "dha", "family medicine", "mshr", "aprn", "cssf", "offcr", "cfo", "chcp", "ccep", "facp", "fhm", "cphq", "op", "cpe", "vd", "dvm",
                 "cma", "drph", "mhp", "pmhnp", "er", "fsgm", "eid", "dpm", "staff acct", "cfp", "cpfo", "pe", "father", "esquire", "phjc", "dnsc",
                 "csfn", "dpth", "br", "fsc", "emhs", "rpa", "facsm", "mn", "adm", "scd", "psyd", "jcdjv", "fp", "msc", "bs", "mfa", "csc", "snd",
                 "pa-c", "nhs", "facs", "vpgeneral couns", "preside", "gen counsel", "pa", "vpcfo", "lmt", "ccvi", "pe", "scc", "rrt", "wvote",
                 "nea-", "ph", "brother", "senior", "facc", "msf", "rph", "facep", "dml", "employed", "member", "dsw", "-april", "april-dec", "facs",
                 "inactive", "dmsc", "jul-", "dec-jun", "-jun", "lnha", "hon", "cc", "joined", "mdboard", "mddirector", "nlh", "presceo", "svp",
                 "lega", "presidentceo", "sp", "of", "fsa", "dmin", "dhsc", "sphr", "csa", "hm", "lcda", "lcdo", "mrs", "ms", "mr", "faap", "sessions",
                 "corporate compliance", "wm", "cso", "thm", "osp", "mpf", "commissioner", "honorable", "mpd", "ne-", "- care", "ambulatory",
                 "treasasst", "secr", "administrative", "pd", "mdiv", "msgr", "dba", "msed", "ba", "sectreas", "sectrint", "prsceo", "mdphd", "ihm",
                 "sen", "facog", "caqsh", "mmm", "fccp", "fc", "msm", "legal", "offi", "fann", "svpcfo", "executive", "dpt", "foo", "network dev", 
                 "cfoasst", "pc-a", "bds", "cme", "pp", "psy d", "vice", "key employe", "senior", "phcns", "fmm", "presidenceo", "mrcp", "lohr",
                 "lmhc", "mppm", "lcsw", "rnc-ob", "fabc", "bcps", "dabr", "csjp", "business", "cfe", "chfp", "counselman", "cpcu", "key", "pbvm",
                 "pres", "directorvp", "chairpers", "cnl", "non-voting", "jul", "vpcfocoo", "financial", "till", "mpa", "gen'l", "counse", "msf",
                 "exec", "dirintrm", "cgma", "fhfma", "frmr", "reverend", "ed", "acm", "fa", "mlt", "cfa", "umms rep", "chairperson", "voting", 
                 "annual life", "jrdo", "ssm", "mdpres", "med", "fabf", "ncpsya", "fac", "pr", "llm", "mshcm", "- mar", "crmc", "cno", "mhcds", 
                 "facr", "senator", "finance", "mso", "rhsj", "mot otrl", "ocn", "lsw", "mmhc", "cfre", "msa", "cws", "dph", "mscprp", "dec-june",
                 "mdms", "mdret", "dpa", "rdh", "faia", "jwc", "chairman", "msmphrncph", "fmp", "physd", "phdcne", "- dec", "then", "clinical",
                 "lpd", "-sept", "-may", "june", "incoming", "elect", "macp", "- reg", "- treas", "strategy", "dmv", "membervp", "jr", "feb-dec", "-aug",
                 "- dec", "cnsl", "empl", "hcomp", "and", "admin", "dec-mar", "rtt", "may-dec", "svpchief", "cic", "mbamsnrn", "msrncnscenp",
                 "dm", "medical", "evp", "patient", "philanthropy", "clinic", "ahmc", "division", "-care div med th", "-care div fin", "-care div ops",
                 "-cvn clinics", "-care div", "chse", "dns", "care division", "- nov", "south meadows", "cmo","pmh", "mpp", "ahdl", "-dec", "cpmsm",
                 "hacp", "coo", "dnpapn", "anp", "pastor", "dnpmsnrn", "mgmt", "secrtreas", "jrv", "vpceo", "scch", "frcp", "ssj", "ncmp", "mscr", "dsf",
                 "mbr", "mspt", "jrdo", "clin", "coo", "svpchief", "sectreasurer", "and", "msha", "mphd", "apa", "ahp", "ee", "contracted", "phr",
                 "iii", "facg", "mdterm", "macp", "lcpc", "ccp", "rnc", "msob", "ceosecretary", "wchd", "cfosr", "cfovp", "cphrm", "cpps", "cooasst",
                 "treasto", "-directorjul-apr", "apr", "fmp", "ops", "scrn", "msnrn", "rnc", "fccm", "mdoff", "informa", "family", "orthopedic", 
                 "ppcme", "august", "partial", "lmh", "evp ballad health", "past", "tmh", "- aug", "evp", "lmhtrh", "chairperson", "treas", "nov", "dec",
                 "rtrtc", "dmo", "truste", "presiden", "fach", "fhf", "year", "prn", "execut", "february", "frm", "- mar", "ncr", "fdtn", "ipd", 
                 "aug", "nov", "feb", "only", "part yr", "care div", "-cur", "- mar", " - care", "- reg", "-c", "- fin", "-directorjul", "-july",
                 "acns", "mpah", "achce", "judge", "rabbi", "rc", "jrphd", "since", "sept", "ex-officio", "jun", "sep", "aca", "nd", "sec", "co", "stm", "jr", "dd", "ch",
                 "vg", "faacvpr", "svcs", "ahhm", "care", "see sch", "sch", "inc", "vcos", "jrpa", "ahuv", "yr", "rahn", "syed", "cos", "ehfph", "ehmc", "msph",
                 "doctor", "-mar", "rp", "dp", "- part", "chfc", "cva", "edp", "abpp", "mcg", "bchphd", "ng", "presdir", "trea", "lssgb", "ret", "- ret", "-ret", "jcd",
                 "jcl", "promoted", "msrn", "by", "dw", "september", "october", "november", "december", "january", "february", "cff", "cpcs", "llc", "phdeff", "cjsp", "apn",
                 "cna", "nonvoting", "nha", "rnmsnmba", "dnprn", "healthcare", "mcch", "varied", "ccm", "ex - officio", "system", "vc", "fmccoo ws mkt", "fmccoo", "ccvi",
                 "company", "srs", "dcm", "staff", "aicp", "mp", "inter hlth inv", "cr", "oda", "- part", "liasion", "rotated", "iiphd", "interventional", "thoracic", 
                 "jrmdphd", "facoep", "mc", "in march", "cmoasst", "fsm", "frcpc", "td", "deemed", "officer", "phdc", "in", "rcrh", "dphil", "cjs", "iii", "pc", "exited",
                 "phh", "esp", "jdllm", "faa", "ballad health", "health", "rtrtc", "jrtermed", "operating", "-directorjul", "evpgen cncl", "mbataru", "cm", "rtr", "mar", "faafp",
                 "- reg", "faoao", "fabfp", "ahfr", "lfache", "july", "jrv", "bus develop", "ceopresident", "cns", "mbacpamha", "jr", "msci", "assistant", "rnmbafache", "epps",
                 "jcmc", "-acute", "evpcfo", "fsso", "-pediatric", "cio", "cfoevp", "human", "associate", "faccp", "ods", "rep")
all_remove_list <- c(doctor_list, other_doctor_list, nurse_list, ha_list, remove_list)

# words that are real name parts but were sitting in remove_list (never strip these on their own)
never_list <- c("ed", "jan", "june", "august", "syed", "ng", "lohr", "rahn", "sen", "key", "till", "epps", "wm", "mc", "st", "co",
                "foo", "counselman", "mbataru", "ahuv")
# credential-like words that are also plausible surnames/initials (only strip when 2+ other words remain)
ambiguous_list <- c("do", "ma", "pa", "pe", "od", "pt", "op", "sc", "pd", "sp", "cr", "fa", "mp", "rd", "dd", "dm", "hm", "cc",
                    "cm", "pc", "pp", "td", "vg", "mb", "nd", "ee")
# honorifics: only removed from the START of a name
hon_core_list  <- c("dr", "mr", "mrs", "ms", "miss", "rev", "reverend", "fr", "sister", "sr", "br", "hon", "honorable", "senator",
                    "rabbi", "msgr", "doctor", "prof", "professor", "the", "most", "very", "right", "rt", "ret")
hon_risky_list <- c("bishop", "judge", "father", "mother", "brother", "pastor", "general", "major", "maj", "gen", "commissioner",
                    "magistrate")   # also surnames (amy bishop), so only removed from the start when 3+ words
gen_list <- c("jr", "sr", "ii", "iii", "iv", "2nd", "3rd")
# credentials / roles: removed from the END of a name (or the middle in surname-first names)
trail_list <- union(setdiff(all_remove_list, c(never_list, hon_risky_list, gen_list, "senior", "junior", "sister", "senator", "staff")),
                    c("ne", "bc", "cert", "counsel", "gen"))
trail_safe <- setdiff(trail_list, ambiguous_list)
hyphen_ok  <- union(trail_list, c("c", "ne", "bc"))     # for things like "rn-phd", "ne-bc"
strong_mid_list <- c("md", "phd", "dds", "dmd", "rn", "np", "cpa", "mba", "jd", "mph", "msn", "bsn", "dnp", "crna", "aprn", "fnp",
                     "facs", "facp", "fache", "mha", "edd", "esq", "esquire", "dpm", "dvm", "pharmd")
# words that start an annotation like "thru 913", "eff 101" (only used when the name also contained digits)
month_list <- c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "sept", "oct", "nov", "dec", "january", "february",
                "march", "april", "june", "july", "august", "september", "october", "november", "december")
annotation_list <- c("thru", "to", "end", "eff", "fr", "start", "strt", "term", "beg", "from", "left", "until", "through", "started",
                     "ended", "began", "begin", "retired", "departed", "joined", "trm", "ret", "effective", "effec", "ending",
                     "beginning", "since", "exited", "hired", "till", "td", "of", "off", "st", "year", "yr", "only", "in", "by",
                     "and", "then", "as", "is", "termed", "resigned", "deceased", "mdterm", "mdend", "mdstrt", month_list)

# turn a word list into a regex alternation (longest words first, regex characters escaped, multi-word entries dropped)
make_alt <- function(x) {
  x <- unique(x[!str_detect(x, "\\s")])
  x <- x[order(-nchar(x))]
  paste(str_replace_all(x, "([.\\-'()+*?\\[\\]{}|^$\\\\])", "\\\\\\1"), collapse = "|")
}

alt_hon_core   <- make_alt(hon_core_list)
alt_hon_risky  <- make_alt(hon_risky_list)
alt_gen        <- make_alt(gen_list)
alt_trail      <- make_alt(trail_safe)
alt_trail_ann  <- make_alt(c(trail_safe, setdiff(annotation_list, ambiguous_list)))   # used when the name had digits
alt_ambiguous  <- make_alt(ambiguous_list)
alt_never      <- make_alt(never_list)
alt_mid        <- make_alt(c(strong_mid_list, hon_core_list, trail_safe))
alt_hyphen     <- make_alt(hyphen_ok)
alt_hyphen_combo <- paste0("(?:", alt_hyphen, ")(?:-(?:", alt_hyphen, "))+")
alt_vocab      <- make_alt(c(trail_list, annotation_list, gen_list, hon_core_list, hon_risky_list,
                             "former", "frmr", "fmr", "past", "pst", "emeritus", "interim", "vacant"))

# annotations that follow a name: "see sch o", "paid by", "termed", ...  (everything after them is dropped)
rx_strong_annotation <- paste0("(?:\\bsee (?:sch|schedule|statement|sched)\\w*(?: [a-z]\\b)?|(?:\\bpd|\\bpaid|pd) by\\b|\\btermed\\b|",
                               "\\bterminated\\b|\\bresigned\\b|\\bretired\\b|\\bdeceased\\b|\\bdeparted\\b|\\bpart[- ]?year\\b|",
                               "\\bpart yr\\b|\\bpartial(?: year)?\\b|\\bannual life\\b|\\bex[- ]?officio\\b|\\bnon[- ]?voting\\b|",
                               "\\bnonvoting\\b).*$")

# separators between a name and a role/date, e.g. "renae snyder -cfo jul-mar", "jenny robinson-started 3-2019"
glue_annotation <- "(?:started|start|left|began|begin|beg|term|termed|thru|through|end|ended|eff|effective|until|from|to|resigned|retired|joined|since|exited|hired|departed)\\b"
glue_role_long  <- "(?:director|trustee|treasurer|secretary|president|chairman|chairperson)"
glue_role_short <- "(?:(?:vp|svp|evp|avp|cfo|ceo|coo|cmo|cno|cio)[a-z]*\\b|(?:dir|treas|sec|chair|pres|board|member|officer|md|rn|phd|dds|cpa)\\b)"
dash_sep <- "(?:\\s+-+\\s*|\\s*-+\\s+)"
rx_separator <- paste0(dash_sep, "|\\s*[;:]\\s*|\\s+/\\s*|\\s*/\\s+|\\s*\\*+\\s*|-(?=", glue_annotation, ")|-(?=", glue_role_long, ")|-(?=", glue_role_short, ")")
# "koroghlian - scott" / "jacks- porter" are hyphenated surnames written with spaces: join them instead of cutting
rx_hyphen_surname <- paste0("^((?:(?!", dash_sep, ").)+)", dash_sep, "(?!(?:", alt_vocab, ")$)(?!", glue_annotation, "|", glue_role_long, "|", glue_role_short, ")([a-z]+)$")
rx_glued_annotation <- "(?<=[a-z]{3})(?:termed|term|end|strt|start|beg|eff|thru|from)$"

placeholder_pattern <- "^(vacant|none|n/?a|tbd|unknown|open|open position|vacant position|position vacant|to be determined|not applicable|various)$"

# convert to all lowercase
data <- data %>%
  dplyr::mutate(PersonNm = tolower(PersonNm),
                TitleTxt = tolower(TitleTxt)) %>%
  distinct()

# Create name_cleaned variable
data <- data %>%
  mutate(name_cleaned = str_remove(PersonNm, "^\\(\\d+\\)\\s*"))

former_title_pattern <- "\\bformer|\\b(?:frmr|fmr|emeritus|past|pst)\\b"     # "formervp" (glued) still counts as former
former_name_pattern  <- "\\b(?:former|frmr|fmr|past|pst)\\b"

data <- data %>%
  filter(!coalesce(former_ofcr_director_trustee, FALSE)) %>%
  select(-former_ofcr_director_trustee) %>%
  filter(!str_detect(coalesce(TitleTxt, ""), former_title_pattern)) %>%
  filter(!str_detect(name_cleaned, former_name_pattern)) %>%
  filter(!str_detect(PersonNm, "\\bee comp\\b"))    # "ee comp is for admin svcs" is a note, not a person

# update indicator columns (keep 1s, update 0s if string detected)
update_indicator <- function(existing_col, names_vector, title_list) {
  pattern <- paste0("\\b(", paste(title_list, collapse = "|"), ")\\b")
  detected <- as.integer(grepl(pattern, names_vector, ignore.case = TRUE))
  
  result <- pmax(existing_col, detected, na.rm = TRUE)
  
  # Fix cases where both inputs were NA
  result[is.na(result)] <- 0
  
  result
}

data$doctor      <- update_indicator(data$doctor,       data$PersonNm, doctor_list)
data$other_doctor <- update_indicator(data$other_doctor, data$PersonNm, other_doctor_list)
data$nurse       <- update_indicator(data$nurse,        data$PersonNm, nurse_list)
data$ha          <- update_indicator(data$ha,           data$PersonNm, ha_list)

data$doctor      <- update_indicator(data$doctor,       data$TitleTxt, doctor_list)
data$other_doctor <- update_indicator(data$other_doctor, data$TitleTxt, other_doctor_list)
data$nurse       <- update_indicator(data$nurse,        data$TitleTxt, nurse_list)
data$ha          <- update_indicator(data$ha,           data$TitleTxt, ha_list)

### CLEAN THE NAMES
# remove annotations and anything after them
data <- data %>%
  mutate(name_cleaned = str_squish(name_cleaned),
         name_cleaned = str_remove(name_cleaned, rx_strong_annotation),
         name_cleaned = str_remove_all(name_cleaned, "\\([^)]*\\)"),
         name_cleaned = str_remove(name_cleaned, "\\(.*$"),
         name_cleaned = str_remove_all(name_cleaned, "\\)"))

# anything after the first digit is a date or footnote, but remember there was one
data <- data %>%
  mutate(had_digit = str_detect(name_cleaned, "\\d"),
         name_cleaned = str_remove(name_cleaned, "\\d.*$"))

# cut at separators (dash, semicolon, slash) unless it's a hyphenated surname written with spaces
data <- data %>%
  mutate(name_cleaned = case_when(
    str_detect(name_cleaned, rx_hyphen_surname) ~ str_replace(name_cleaned, rx_hyphen_surname, "\\1-\\2"),
    str_detect(str_remove(name_cleaned, paste0("(?:", rx_separator, ").*$")), "\\S") ~ str_remove(name_cleaned, paste0("(?:", rx_separator, ").*$")),
    TRUE ~ name_cleaned))

# punctuation, then squish
data <- data %>%
  mutate(name_cleaned = str_remove_all(name_cleaned, "[.',]"),
         name_cleaned = str_replace_all(name_cleaned, "[^a-z\\- ]", " "),
         name_cleaned = str_squish(name_cleaned),
         name_cleaned = str_remove_all(name_cleaned, "^-+|-+$"),
         name_cleaned = str_squish(name_cleaned))

# keywords glued onto the end of a name with a date after it ("shariq ahmadterm 123117")
data <- data %>%
  mutate(name_cleaned = ifelse(had_digit, str_remove(name_cleaned, rx_glued_annotation), name_cleaned))

# credentials glued onto a name ("alan rothfeldmd", "ralph d brown jrmd", "james cornellphdmd")
data <- data %>%
  mutate(name_cleaned = str_replace(name_cleaned, "\\b(jr|sr|ii|iii|iv)(md|phd|do|esq|cpa|mba)\\b", " \\1 "),
         name_cleaned = str_replace_all(name_cleaned, "(?<=[a-z]{3})(?:phd|md)\\b", " "),
         name_cleaned = str_replace_all(name_cleaned, "(?<=[a-z]{3})(?:phd|md)\\b", " "),   # twice for "mdphd"
         name_cleaned = str_squish(name_cleaned))

# honorifics at the start (a few passes handles "the rev dr")
for (i in 1:4) {
  data <- data %>%
    mutate(name_cleaned = str_remove(name_cleaned, paste0("^(?:", alt_hon_core, ")\\s+(?=\\S)")),
           name_cleaned = str_remove(name_cleaned, paste0("^(?:", alt_hon_risky, ")\\s+(?=\\S+\\s+\\S)")))
}

# generational suffix: save it, then take it out of the name (never the first word)
data <- data %>%
  mutate(name_suffix = str_extract(name_cleaned, paste0("(?<=\\s)(?:", alt_gen, ")(?=\\s|$)")),
         name_suffix = dplyr::recode(name_suffix, "2nd" = "ii", "3rd" = "iii"),
         name_cleaned = str_remove_all(name_cleaned, paste0("(?<=\\S)\\s+(?:", alt_gen, ")(?=\\s|$)")))

# credentials / roles / date words at the END of the name (a few passes handles "lisa mollo bsn ma rn ne-bc")
# ambiguous ones like "do" or "ma" only come off if 2+ other words remain ("steven feder do" but not "tuan do")
for (i in 1:3) {
  data <- data %>%
    mutate(
      name_cleaned = ifelse(had_digit,
                            str_replace(name_cleaned, paste0("^(\\S.*?)(?:\\s+(?:", alt_trail_ann, "|", alt_hyphen_combo, "))+$"), "\\1"),
                            str_replace(name_cleaned, paste0("^(\\S.*?)(?:\\s+(?:", alt_trail, "|", alt_hyphen_combo, "))+$"), "\\1")),
      name_cleaned = str_replace(name_cleaned, paste0("^(\\S+\\s+\\S.*?)(?:\\s+(?:", alt_ambiguous, "))+$"), "\\1"),
      name_cleaned = ifelse(had_digit,
                            str_replace(name_cleaned, paste0("^(\\S+\\s+\\S.*?)\\s+(?:", alt_trail_ann, ")\\s+(?:", alt_never, ")$"), "\\1"),
                            str_replace(name_cleaned, paste0("^(\\S+\\s+\\S.*?)\\s+(?:", alt_trail, ")\\s+(?:", alt_never, ")$"), "\\1")))
}

# credentials in the MIDDLE for surname-first layouts ("weiss md mba anthony", "hebert np richard m")
data <- data %>%
  mutate(name_cleaned = str_remove_all(name_cleaned, paste0("(?<=\\S)\\s+(?:", alt_mid, ")(?=\\s+\\S)")),
         name_cleaned = str_squish(name_cleaned))

# placeholder rows ("vacant") and rows with nothing left are not people
data <- data %>%
  filter(!str_detect(name_cleaned, placeholder_pattern), name_cleaned != "")

# check: look at what changed most
data %>%
  distinct(PersonNm, name_cleaned) %>%
  mutate(n_words = str_count(name_cleaned, "\\S+")) %>%
  count(n_words)

data <- data %>%
  distinct()

rm(early_data, xml_data, all_remove_list, doctor_list, file, files, ha_list, nurse_list, other_doctor_list, remove_list)

# look at the number of distinct EINs in each year
data %>%
  group_by(TaxYr) %>%
  distinct(Filer.EIN) %>%
  summarise(n())

# drop end cap years
data <- data %>%
  filter(TaxYr %in% c(2008, 2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021,2022,2023))


# drop institutional trustees (an NA flag is kept instead of silently dropped)
data <- data %>%
  filter(!coalesce(institutional_trustee, FALSE)) %>%
  select(-institutional_trustee)

### COMBINE NAMES WITHIN AN EIN
names_compatible <- function(name_a, suffix_a, name_b, suffix_b) {
  if (!is.na(suffix_a) && !is.na(suffix_b) && suffix_a != suffix_b) return(FALSE)
  
  tokens_a <- strsplit(name_a, " ")[[1]]
  tokens_b <- strsplit(name_b, " ")[[1]]
  words_a <- tokens_a[nchar(gsub("-", "", tokens_a)) > 1]
  words_b <- tokens_b[nchar(gsub("-", "", tokens_b)) > 1]
  inits_a <- tokens_a[nchar(gsub("-", "", tokens_a)) == 1]
  inits_b <- tokens_b[nchar(gsub("-", "", tokens_b)) == 1]
  
  if (length(words_a) < 2 || length(words_b) < 2) return(FALSE)
  
  # middle initials can be missing from one name but can't contradict
  initials_ok <- all(inits_a %in% inits_b) || all(inits_b %in% inits_a)
  if (!initials_ok) return(FALSE)
  
  # same words, any order
  if (identical(sort(words_a), sort(words_b))) return(TRUE)
  
  # one word with a small typo (longer words allow a bigger typo)
  if (length(words_a) == length(words_b)) {
    sorted_a <- sort(words_a)
    sorted_b <- sort(words_b)
    diff_pos <- which(sorted_a != sorted_b)
    if (length(diff_pos) == 1) {
      dist <- stringdist(sorted_a[diff_pos], sorted_b[diff_pos], method = "osa")
      len <- min(nchar(sorted_a[diff_pos]), nchar(sorted_b[diff_pos]))
      same_start <- substr(sorted_a[diff_pos], 1, 1) == substr(sorted_b[diff_pos], 1, 1)
      if (same_start && ((dist == 1 && len >= 6) || (dist == 2 && len >= 11))) return(TRUE)
    }
  }
  FALSE
}

combine_name_matches <- function(data) {
  setDT(data)
  
  # Split by firm
  firm_list <- split(data, by = "Filer.EIN", keep.by = TRUE)
  
  process_firm <- function(firm_data) {
    # one row per distinct name, with how often it appears and which years
    unique_names <- firm_data[, .(name_count = .N, years = list(unique(TaxYr))), by = .(name_cleaned, name_suffix)]
    n <- nrow(unique_names)
    if (n <= 1) return(firm_data)
    
    # edges between compatible names, UNLESS both names appear in the same year
    # (two names listed in the same EIN-year are two different people)
    edges <- list()
    for (i in 2:n) {
      for (j in 1:(i - 1)) {
        if (length(intersect(unique_names$years[[i]], unique_names$years[[j]])) > 0) next
        if (names_compatible(unique_names$name_cleaned[i], unique_names$name_suffix[i],
                             unique_names$name_cleaned[j], unique_names$name_suffix[j])) {
          edges <- append(edges, list(c(j, i)))
        }
      }
    }
    if (length(edges) == 0) return(firm_data)
    
    g <- make_empty_graph(n, directed = FALSE)
    g <- add_edges(g, as.vector(t(do.call(rbind, edges))))
    unique_names[, group := components(g)$membership]
    
    # a chain a-b-c can join two names that appear in the same year; if so, leave that whole group alone
    unique_names[, clash := {
      yrs <- unlist(years)
      anyDuplicated(yrs) > 0
    }, by = group]
    unique_names[clash == TRUE, group := n + .I]   # make each name its own group
    
    # canonical name = the most common one in the group (ties: the longer name)
    unique_names[, canonical_name := name_cleaned[order(-name_count, -nchar(name_cleaned))][1], by = group]
    unique_names[, canonical_suffix := {
      s <- name_suffix[!is.na(name_suffix)]
      if (length(s) > 0) s[1] else NA_character_
    }, by = group]
    
    mapping <- unique_names[, .(name_cleaned, name_suffix, canonical_name, canonical_suffix)]
    firm_data <- merge(firm_data, mapping, by = c("name_cleaned", "name_suffix"), all.x = TRUE)
    firm_data[, name_cleaned := fcoalesce(canonical_name, name_cleaned)]
    firm_data[, name_suffix := canonical_suffix]
    firm_data[, c("canonical_name", "canonical_suffix") := NULL]
    
    return(firm_data)
  }
  
  corrected_list <- lapply(firm_list, process_firm)
  rbindlist(corrected_list, use.names = TRUE, fill = TRUE)
}

### CALL IT 
data <- combine_name_matches(data)

# only put the suffix back when the same EIN has two people with the same name and different suffixes (father/son)
data <- data %>%
  group_by(Filer.EIN, name_cleaned) %>%
  mutate(n_suffix = n_distinct(name_suffix, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(name_cleaned = ifelse(n_suffix > 1 & !is.na(name_suffix), paste(name_cleaned, name_suffix), name_cleaned)) %>%
  select(-name_suffix, -n_suffix, -had_digit)

# check: nobody should be listed twice in the same EIN-year after merging, and compare to before
data %>% count(Filer.EIN, TaxYr, name_cleaned) %>% filter(n > 1) %>% nrow()   # raw duplicates: this runs BEFORE the split / max handling below, so it is not the final number

data <- data %>%
  mutate(across(
    c(reportable_comp_from_org,
      reportable_comp_from_rltd_org,
      other_compensation,
      avg_hours_per_week,
      avg_hours_per_week_rltd_org),
    ~ ifelse(is.finite(.), ., NA_real_)
  ))

# fill missing compensation / hours with 0, EXCEPT on 990-EZ returns, where these fields are never reported
# (all compensation and hours are blank on the EZ), so there a missing value stays NA instead of looking like "paid $0"
data <- data %>%
  mutate(ez_form = str_detect(coalesce(as.character(return_type), ""), regex("ez", ignore_case = TRUE)))

fill_cols <- c("reportable_comp_from_org", "reportable_comp_from_rltd_org", "other_compensation",
               "avg_hours_per_week", "avg_hours_per_week_rltd_org")
for (col in fill_cols) {
  data[[col]] <- ifelse(is.na(data[[col]]) & !data$ez_form, 0, data[[col]])
}

data %>% filter(ez_form) %>% summarise(n_ez_rows = n(), n_ez_comp_still_na = sum(is.na(reportable_comp_from_org)))   # these should match
data <- data %>% select(-ez_form)

data <- data %>%
  mutate(reportable_comp_from_org = ifelse(reportable_comp_from_org<0, 0, reportable_comp_from_org),
         reportable_comp_from_rltd_org = ifelse(reportable_comp_from_rltd_org<0, 0, reportable_comp_from_rltd_org),
         other_compensation = ifelse(other_compensation<0, 0, other_compensation))



# two returns in the same end_year with different labels (full year + short transition return):
# the people rows have no filing id, so assign them by pay: a person's higher-paid row is the full-year return,
# the lower-paid row is the stub. A person with one row is assumed to be on both returns (same row copied to the stub year).
data <- data %>%
  mutate(pay_for_rank = coalesce(reportable_comp_from_org, 0) + coalesce(reportable_comp_from_rltd_org, 0)) %>%
  group_by(Filer.EIN, orig_end_year, name_cleaned) %>%
  mutate(row_rank = ifelse(split_returns, rank(-pay_for_rank, ties.method = "first"), 1L),
         n_rows_person = n()) %>%
  ungroup()

stub_copies <- data %>%
  filter(split_returns, n_rows_person == 1) %>%
  mutate(TaxYr = label_stub, stub_copy = TRUE)

data <- data %>%
  mutate(TaxYr = ifelse(split_returns & row_rank >= 2, label_stub, TaxYr),
         stub_copy = FALSE) %>%
  bind_rows(stub_copies) %>%
  select(-pay_for_rank, -row_rank, -n_rows_person, -orig_end_year, -n_filings, -label_full, -label_stub, -use_filing_label, -needs_recode)

data %>% filter(split_returns) %>% summarise(n_rows = n(), n_stub_copies = sum(stub_copy))

# the same person-year can appear twice with the identical title and compensation (e.g. the same line copied twice);
# summing below would double that compensation, so keep one copy of exact duplicates
data <- data %>%
  distinct(Filer.EIN, TaxYr, name_cleaned, TitleTxt, reportable_comp_from_org, reportable_comp_from_rltd_org, other_compensation, .keep_all = TRUE)

# sum / max that stay NA when every value is NA (plain sum(na.rm = TRUE) turns all-NA into 0, max(na.rm = TRUE) into -Inf)
sum_or_na <- function(x) if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)
max_or_na <- function(x) if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)
# compensation: add up rows, except when the rows are the same person on two returns in the same year (take the larger)
sum_or_max <- function(x, use_max) if (use_max) max_or_na(x) else sum_or_na(x)

# combine duplicates in name by adding up compensation and taking max of doctor type indicators
data <- data %>%
  distinct() %>%
  group_by(Filer.EIN, TaxYr, name_cleaned) %>%
  summarise(
    PersonNm = first(PersonNm),
    
    # Keep a representative title (or collapse them if you prefer)
    TitleTxt = paste(unique(TitleTxt), collapse = ", "),
    
    # Sum compensation (max when the duplicate rows are the same person on two returns)
    reportable_comp_from_org = sum_or_max(reportable_comp_from_org, any(collapse_max)),
    reportable_comp_from_rltd_org = sum_or_max(reportable_comp_from_rltd_org, any(collapse_max)),
    other_compensation = sum_or_max(other_compensation, any(collapse_max)),
    
    # Take max / OR for indicators
    officer_ind = max(officer_ind, na.rm = TRUE),
    key_employee_ind = max(key_employee_ind, na.rm = TRUE),
    individual_trustee_or_director = max(individual_trustee_or_director, na.rm = TRUE),
    highest_compensated_employee = max(highest_compensated_employee, na.rm = TRUE),
    
    # Hours: take max (usually best for your use case)
    avg_hours_per_week = max_or_na(avg_hours_per_week),
    avg_hours_per_week_rltd_org = max_or_na(avg_hours_per_week_rltd_org),
    
    doctor = max(doctor, na.rm=TRUE),
    other_doctor = max(other_doctor, na.rm=TRUE),
    nurse = max(nurse, na.rm=TRUE),
    ha = max(ha, na.rm=TRUE),
    
    .groups = "drop"
  )

# Fix average hours that got read in wrong
data <- data %>%
  mutate(avg_hours_per_week = ifelse(avg_hours_per_week>120, avg_hours_per_week/100, avg_hours_per_week),
         avg_hours_per_week_rltd_org = ifelse(avg_hours_per_week_rltd_org>79, avg_hours_per_week_rltd_org/100,avg_hours_per_week_rltd_org))

summary(data)

### STEP 2: ASSIGN GENDER BASED ON FIRST NAME ------------------------------------------------------------
# Read in Census data for those boarn in 2000
firstnames <- read_csv(paste0(raw_data_path, "/yob2000.txt"), col_names = FALSE) %>%
  filter(X3>50) %>%
  select(X1, X3) %>%
  rename(name=X1, count=X3) %>%
  mutate(name=tolower(name)) %>%
  group_by(name) %>%
  summarise(count = max(count)) %>%
  ungroup()
add_firstnames <- c("doug", "jef", "cyndy", "waine", "philippe", "jil", "pam", "justino", "beverley", "evalie", "wendall",
                    "krzystof", "berton", "terrie", "matt", "sheri", "dollie", "joette", "bob", "adrina", "seema", "aj",
                    "antonietta", "archie", "philippa", "daphnee", "dona", "chuck", "franca", "phil", "pat", "roddy",
                    "nicolene", "wilfred", "dirk", "lynne", "bud", "felice", "sharita", "gwenn", "delores", "maybelle",
                    "gail", "lauretta", "jj", "dwain", "cj", "katharin", "hal", "ladena", "stan", "rob", "penni", "polly", 
                    "rickie", "sheryl", "luella", "ritchie", "deb", "jeremie", "kraig", "carole", "tonja", "georgiana",
                    "sallie", "thor", "bart", "kerrie", "ward", "cliff", "derrek", "bernadine", "herb", "dotty", "christi",
                    "gregg", "sheri", "sue", "elayne", "chrissy", "phil", "vickie", "phyllis", "ned", "marsha", "clyde",
                    "barb", "bert", "stan", "cliff", "trudy", "patti", "rhoda", "rosy", "barrie", "cheryle", "luann",
                    "maurita", "shari", "val", "dick", "rich", "gayle", "nicki", "julee", "deedra", "denyse", "gretta",
                    "mistie", "carlyle", "zac", "russ", "catie", "jeanene", "patsy", "kathi", "elsbeth", "roseanne", "geoff",
                    "mitch", "cris", "shondra", "roxie", "kip", "mimi", "buddy", "reecia", "claudette", "tammie", 
                    "dorris", "roddy", "lyn", "fran", "anabell", "marlo", "thelma", "vern", "jen", "bud", "suzannah", 
                    "howie", "lonna", "steph", "danile", "halsey", "carole", "katharin", "butch", "tod", "susanne", 
                    "sharlene", "gerry", "wilbur", "ollie", "tadd", "sherri", "georgette", "vicki", "patty", "taisha")
firstnames_list <- append(as.list(firstnames)[["name"]], add_firstnames)

# remove initials and "-"
data <- data %>%
  mutate(name_cleaned = str_trim(name_cleaned)) %>%
  mutate(formatted_name = str_replace_all(name_cleaned, "(^|\\s)[a-z](?=\\s|$)", "\\1")) %>%
  mutate(formatted_name = str_squish(formatted_name)) %>%
  mutate(formatted_name = str_replace_all(formatted_name, "-"," ")) %>%
  mutate(formatted_name = str_replace(formatted_name, " van ", " van"))

# next, split phrase up into individual words
data <- data %>%
  mutate(formatted_name = str_squish(formatted_name)) %>%
  tidyr::separate_wider_delim(formatted_name, delim=" ", names_sep = ".", too_few = "align_start")

# count up number of times first names occur in the first word of name_cleaned in a firm-year
data <- data %>%
  mutate(first_count = ifelse(formatted_name.1 %in% firstnames_list,1,0)) %>%
  group_by(TaxYr, Filer.EIN) %>%
  mutate(total_names = n()) %>%
  mutate(first_first = sum(first_count)) %>%
  ungroup()

# If the majority of names have a first name located first, record that as the first name (even if there are > 2 names because it shouldn't matter)
data <- data %>%
  mutate(first_name = ifelse(first_first/total_names>=.8, formatted_name.1, NA))

# fill first_name
data <- data %>%
  group_by(name_cleaned, Filer.EIN) %>%
  fill(first_name, .direction="downup") %>%
  ungroup()

# If the majority of names have a first name located second, record that as the first name
# exclude firms where most first words are single-letter initials ("initial lastname" format)
data <- data %>%
  group_by(TaxYr, Filer.EIN) %>%
  mutate(initial_first = sum(nchar(formatted_name.1) == 1, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(first_name = ifelse(first_first/total_names<=.2 & is.na(formatted_name.3) & is.na(first_name) &
                               initial_first/total_names < 0.5, formatted_name.2, first_name))

# fill first_name when applicable
data <- data %>%
  group_by(name_cleaned, Filer.EIN) %>%
  fill(first_name, .direction="downup") %>%
  ungroup()

# Count up the number of times a first name is found
# If exactly one is found, assign that to first name
data <- data %>%
  rowwise() %>%
  mutate(num_first_across = sum(c_across(starts_with("formatted_name.")) %in% firstnames_list)) %>%
  mutate(first_name = if_else(num_first_across == 1 & is.na(first_name),
                              c_across(starts_with("formatted_name.")) %>% .[. %in% firstnames_list] %>% .[1],
                              first_name)) %>%
  ungroup()

# If multiple first names found but the last word is not a first name, pick the first matching word
# (e.g. "mary ann smith" -> "mary"; avoids ambiguity when last word could also be a first name)
data <- data %>%
  rowwise() %>%
  mutate(first_name = {
    if (!is.na(first_name) || num_first_across <= 1) {
      first_name
    } else {
      words <- na.omit(c_across(starts_with("formatted_name.")))
      if (length(words) > 0 && !(tail(words, 1) %in% firstnames_list)) {
        matches <- words[words %in% firstnames_list]
        if (length(matches) > 0) matches[1] else first_name
      } else {
        first_name
      }
    }
  }) %>%
  ungroup()

# If the name "mary" is found, assign that as first name
data <- data %>%
  mutate(first_name = ifelse(str_detect(name_cleaned, "mary\\b"), "mary", first_name),
         first_name = ifelse(str_detect(name_cleaned, "elizabeth\\b"), "elizabeth", first_name),
         first_name = ifelse(str_detect(name_cleaned, "\\banne\\b"), "anne", first_name),
         first_name = ifelse(str_detect(name_cleaned, "\\bjohn\\b"), "john", first_name))

# Look at whether the same name ever gets assigned different first names in different years
data %>%
  filter(!is.na(first_name)) %>%
  group_by(name_cleaned, Filer.EIN) %>%
  summarise(
    unique_first_names = n_distinct(first_name),
    first_names_found = paste(sort(unique(first_name)), collapse = " | ")
  ) %>%
  filter(unique_first_names > 1) %>%
  arrange(desc(unique_first_names)) %>%
  ungroup()
#none

# run gender algorithms on those with first names
people_has_first <- data %>%
  filter(!is.na(first_name))

# Use "gender" package to predict gender of each name
genders <- people_has_first %>%
  distinct(first_name) %>%
  rowwise() %>%
  do(results = gender(.$first_name, years = c(1960,2000), method="ssa")) %>%
  mutate(n = nrow(results)) %>%
  filter(n>0) %>%
  do(bind_rows(.$results)) %>%
  select(-year_min, -year_max)

# join back to original data
people_has_first <- people_has_first %>%
  left_join(genders, by=c("first_name"="name")) %>%
  rename(prob_male_r = proportion_male, prob_female_r=proportion_female, gender_r=gender)

# people with missing first name — run gender algorithms on every name part
people_no_first <- data %>%
  filter(is.na(first_name))

# pivot all name-part columns long so we can run algorithms on each word
name_parts_no_first <- people_no_first %>%
  select(name_cleaned, starts_with("formatted_name.")) %>%
  distinct() %>%
  pivot_longer(cols = starts_with("formatted_name."),
               names_to  = "position",
               values_to = "name_part") %>%
  filter(!is.na(name_part))

# run gender package on all unique name parts
genders_no_first <- name_parts_no_first %>%
  distinct(name_part) %>%
  rowwise() %>%
  do(results = gender(.$name_part, years = c(1960, 2000), method = "ssa")) %>%
  mutate(n = nrow(results)) %>%
  filter(n > 0) %>%
  do(bind_rows(.$results)) %>%
  select(-year_min, -year_max)

# join gender-package results back to name parts
name_parts_no_first <- name_parts_no_first %>%
  left_join(genders_no_first, by = c("name_part" = "name")) %>%
  rename(prob_male_r = proportion_male, prob_female_r = proportion_female, gender_r = gender)

name_parts_no_first <- name_parts_no_first %>%
  group_by(name_cleaned) %>%
  mutate(gender_r = ifelse(length(unique(gender_r))==1, gender_r, NA)) %>%
  mutate(prob_male_r = ifelse(!is.na(gender_r), min(prob_male_r), NA)) %>%
  ungroup() %>%
  distinct(name_cleaned, prob_male_r, gender_r)

# join back to people_no_first
people_no_first <- people_no_first %>%
  left_join(name_parts_no_first, by="name_cleaned")

# Merge dataset 1 and dataset 2
data_withgender <- bind_rows(people_no_first, people_has_first)
data <- data_withgender %>%
  select(Filer.EIN, TaxYr, PersonNm, name_cleaned, TitleTxt, reportable_comp_from_org, reportable_comp_from_rltd_org,
         other_compensation, avg_hours_per_week, avg_hours_per_week_rltd_org, officer_ind, individual_trustee_or_director,
         highest_compensated_employee, key_employee_ind, doctor, other_doctor, nurse, ha, prob_male_r, gender_r)

rm(xml_data, data_withgender, firstnames, genders, genders_no_first, multiple_firstnames, name_parts_no_first, people_has_first, people_no_first, add_firstnames, firstnames_list)

summary(data)

### STEP 3: CATEGORIZE POSITION -------------------------

# SNAPSHOT: rows where PersonNm is contributing to keyword matches that
# TitleTxt alone would not catch. Save this BEFORE switching to TitleTxt-only,
# so we have a fixed record of who needs manual/secondary review.
exec_keywords <- "ceo|chief exec|cfo|chief fin|cmo|chief med|cio|chief info|cno|chief nurs|coo|chief oper|chief.*off|evp|svp|vp\\b|vice pres|president|pres\\b|secretary|treasurer|chair"

data <- data %>%
  mutate(
    all_text = paste(PersonNm, TitleTxt),
    title_only_flag = str_detect(tolower(TitleTxt), exec_keywords),
    combined_flag   = str_detect(tolower(all_text), exec_keywords),
    name_leak_flag  = combined_flag & !title_only_flag
  )

# name_leak_candidates <- data %>%
#   filter(name_leak_flag) %>%
#   select(Filer.EIN, TaxYr, PersonNm, TitleTxt, name_leak_flag) %>%
#   distinct(Filer.EIN, PersonNm, name_leak_flag)
# 
# write_csv(name_leak_candidates, "name_leak_candidates_TODO.csv")

# Bring in manually resolved name-leak cases 
name_leak_resolved <- read_csv("./CreatedData/name_leak_candidates.csv", show_col_types = FALSE) %>%
  filter(!is.na(position)) %>%
  select(Filer.EIN, PersonNm, position) %>%
  distinct() %>%
  mutate(Filer.EIN = as.character(Filer.EIN))

# Merge onto main data
data <- data %>%
  left_join(name_leak_resolved, by = c("PersonNm", "Filer.EIN")) %>%
  select(-name_leak_flag, -all_text, -title_only_flag, -combined_flag)

# Assign CEOs first 
data <- data %>%
  mutate(position = ifelse(str_detect(TitleTxt, "ceo|chief executive|chief exec|cheif exec|c\\.e\\.o|president/ce"), "ceo", position))

# Assign administrators
# An "administrator" is only the CEO when (1) the title is not tied to a unit (nursing home, site, school, service line...) and
# (2) nobody else in the EIN-year holds a CEO title. Generic administrator titles are marked "potential admin ceo" here and
# resolved in the RESOLVE ADMINISTRATORS block right after all positions are assigned.
admin_unit_words <- paste0("nursing home|school|radiology|\\bsite\\b|service line|physician administrator|regional administrator|\\bclinic\\b|\\bph\\b|\\bfm\\b|",
                           "practice|department|dept|\\blab\\b|pharmacy|\\bhr\\b|\\bit\\b|surgery|patient|campus|\\bunit\\b|senior administrator|\\bsr administrator|",
                           "home health|hospice|assisted|long.term|rehab|imaging|cardio|oncology|behavioral|psych|dialysis|finance|operations|\\bnurs")
vp_words <- "\\bvp\\b|vice pres|\\bsvp\\b|\\bevp\\b|\\bv\\.p\\b|vice-pres|vice prs"

data <- data %>%
  mutate(
    position = case_when(
      !is.na(position) ~ position,  # don't override anything already assigned
      
      # Group 2: explicitly subordinate -- check FIRST so "assistant administrator" doesn't get caught by broader patterns below
      str_detect(tolower(TitleTxt), "assistant administrator|asst\\.? administrator|associate administrator") ~ "non-exec leader",
      
      # Group 3a: vp/board/ex-officio + administrator -> candidate for ceo
      str_detect(tolower(TitleTxt), "vp administrator|vp/administrator|vp associate administrator|administrator & board member|ex-officio/administrator|ex-officio administrator") ~ "potential admin ceo",
      
      # Group 3b: functional exec-level administrator titles -> other exec
      str_detect(tolower(TitleTxt), "operations administrator|finance administrator") ~ "other exec",
      
      # Group 3c: site/unit-level administrator titles -> non-exec leader
      str_detect(tolower(TitleTxt), "clinic administrator|nursing administrator|director/administrator") ~ "non-exec leader",
      
      # Group 3d: administrator of a unit (nursing home, site, school, service line, region...) -> non-exec leader,
      # unless the title also has a vp rank, in which case leave it for the vp rules below
      str_detect(tolower(TitleTxt), "administrato") & str_detect(tolower(TitleTxt), admin_unit_words) &
        !str_detect(tolower(TitleTxt), vp_words) ~ "non-exec leader",
      str_detect(tolower(TitleTxt), "administrato") & str_detect(tolower(TitleTxt), admin_unit_words) ~ NA_character_,
      
      # Group 1: general/hospital/facility administrator -> candidate for ceo
      str_detect(tolower(TitleTxt), "administrato") ~ "potential admin ceo",
      
      TRUE ~ position
    )
  )

# define cfo
data <- data %>%
  mutate(
    position = case_when(
      !is.na(position) ~ position,
      
      # subordinate CFO roles (same pattern as the other C-suite rules); a vp rank on top keeps them as vp
      str_detect(TitleTxt, "(assistant|asst\\.?|associate|assoc\\.?|deputy) (cfo|chief fin|cheif fin)") &
        str_detect(TitleTxt, vp_words) ~ "vp",
      str_detect(TitleTxt, "(assistant|asst\\.?|associate|assoc\\.?|deputy) (cfo|chief fin|cheif fin)") ~ "non-exec leader",
      
      # genuine CFO
      str_detect(TitleTxt, "cfo|chief fin|cheif fin|c\\.f\\.o") ~ "cfo",
      
      TRUE ~ position
    )
  )

# Define CMO
data <- data %>%
  mutate(
    position = case_when(
      !is.na(position) ~ position,  # don't override anything already assigned
      
      # Group 3: explicit subordinate — check first
      str_detect(tolower(TitleTxt), "associate chief medical officer|assoc chief medical officer|assistant chief medical officer|asst chief medical officer|\\bacmo\\b|assoc cmo|associate cmo|assistant cmo|asst cmo|deputy cmo|deputy chief medical") ~ "non-exec leader",
      
      # Group 2: CMIO — distinct informatics role, not a CMO — check before broad "chief med"
      str_detect(tolower(TitleTxt), "chief medical info|chief med info|chief medical information|chief med information") ~ "cio",
      
      # Group 4a: chief medical physicist — physics/radiation role, not a physician executive
      str_detect(tolower(TitleTxt), "chief medical physicist|medical physicist") ~ "non-exec leader",
      
      # Group 4b: bare "medical officer" with a regional/facility code suffix — treat as facility-level, not org-wide
      str_detect(tolower(TitleTxt), "medical officer,\\s*(cvn|ncr|scr)\\b") ~ "non-exec leader",
      
      # Group 5: medical staff chief / physician-in-chief are department or medical-staff roles, not the CMO
      str_detect(tolower(TitleTxt), "chief of clinical staff|physician in chief|physician-in-chief") ~ "non-exec leader",
      
      # Group 1: genuine CMO — rank-modifier prefixes (vp/svp/evp/system/care division/director) are fine here
      str_detect(tolower(TitleTxt), "\\bcmos?\\b|chief med|cheif med") ~ "cmo",   # \\bcmo\\b so that "ecmo specialist" is not a CMO
      
      # Group 4c: bare "medical officer" with no other qualifier — too vague, don't assume top exec
      str_detect(tolower(TitleTxt), "\\bmedical officer\\b") ~ "non-exec leader",
      
      TRUE ~ position
    )
  )

# Define CIO
data <- data %>%
  mutate(
    position = case_when(
      !is.na(position) ~ position,
      
      # subordinate CIO roles, following the same pattern as administrator/CFO/CMO
      str_detect(tolower(TitleTxt), "assistant cio|asst cio|associate cio|assoc cio|deputy cio") ~ "non-exec leader",
      
      # CISO — distinct security-focused role, bundling into cio per your "just group it into an existing bucket" preference
      str_detect(tolower(TitleTxt), "chief information security|ciso") ~ "cio",
      
      # genuine CIO
      str_detect(tolower(TitleTxt), "\\bcio\\b|chief info|cheif info") ~ "cio",
      
      TRUE ~ position
    )
  )

# Define CNO
data <- data %>%
  mutate(
    position = case_when(
      !is.na(position) ~ position,
      
      # subordinate CNO roles, same pattern as other C-suite rules
      str_detect(tolower(TitleTxt), "assistant cno|asst cno|associate cno|assoc cno|deputy cno") ~ "non-exec leader",
      
      # Director of Nursing (DON) -> non-exec leader, not org-wide CNO
      str_detect(tolower(TitleTxt), "\\bdon\\b|director of nursing|nursing director") ~ "non-exec leader",
      
      # genuine CNO
      str_detect(tolower(TitleTxt), "cno|chief nurs|cheif nurs") ~ "cno",
      
      TRUE ~ position
    )
  )

# Define COO
data <- data %>%
  mutate(
    position = case_when(
      !is.na(position) ~ position,
      
      # subordinate COO roles
      str_detect(tolower(TitleTxt), "assistant coo|asst coo|associate coo|assoc coo|deputy coo") ~ "non-exec leader",
      
      # genuine COO
      str_detect(tolower(TitleTxt), "coo\\b|chief oper|c\\.o\\.o|cheif oper") ~ "coo",
      
      TRUE ~ position
    )
  )

# Define other executive
data <- data %>%
  mutate(
    position = case_when(
      !is.na(position) ~ position,
      
      # dual role (board title + named executive title) -> capture the executive role
      str_detect(tolower(TitleTxt), "trustee & chief pediatric|trustee & chief pediatric off") ~ "cmo",
      
      # miscaught by broad "exec" match — no actual executive title present, just board language
      str_detect(tolower(TitleTxt), "co-chair|co chair|executive board member|executive committee|exec committee|exec comm") ~ "board",
      
      # "exec" inside a support title, not an executive title
      str_detect(tolower(TitleTxt), "executive assistant|exec\\.? assist|executive secretary|account executive|sales executive") ~ "non-exec leader",
      
      # genuine other exec catch-all (cao/cco need word boundaries: 'cco' sits inside 'accounting')
      str_detect(tolower(TitleTxt), "exec|\\bc{1,2}ao\\b|\\bcco\\b|general counsel|cmdo|cqo|chief.*off|evp|controller|e\\.v\\.p") ~ "other exec",
      
      TRUE ~ position
    )
  )

# Assign med staff president
data <- data %>%
  mutate(position = ifelse(is.na(position) & str_detect(TitleTxt, "chief of staff|president of med|pres med staff|president med|president of auxiliary|medical staff pres|president of the med|president-med|president, academic affairs|president, acute care|pres of med|president m/s|president, med|foundation|pres. med"), "non-exec leader", position)) 

# Assign other non-exec leaders
data <- data %>%
  mutate(position = ifelse(is.na(position) & str_detect(TitleTxt, "division chief|physician chief|medical staff president|medical dir|director of|pharmacy director|pharmacy manager|director pharm|dir pharm|dir of|dir nurs"), "non-exec leader", position)) 

# look at the distribution of VP salaries
data %>%
  filter(is.na(position) & str_detect(tolower(TitleTxt), "\\bvp\\b|vice pres|\\bv\\.p\\b|vice-pres|svp|vice prs")) %>%
  mutate(total_comp = reportable_comp_from_org + reportable_comp_from_rltd_org) %>%
  filter(total_comp <= 100000) %>%
  ggplot(aes(x = total_comp)) +
  geom_histogram(binwidth = 5000) +
  labs(title = "Compensation distribution, VP-pattern titles, capped at $100k")

# Assign vice presidents
data <- data %>%
  mutate(
    position = case_when(
      !is.na(position) ~ position,
      
      # explicit board-titled VP -> board, regardless of compensation
      str_detect(tolower(TitleTxt), "board vice president|board vp") ~ "board",
      
      # subordinate VP roles -> non-exec leader, regardless of compensation
      str_detect(tolower(TitleTxt), "assistant vice president|asst vice president|assistant vp|asst vp") ~ "non-exec leader",
      
      # remaining VP/SVP variants: split by compensation, $10k threshold based on observed bimodal distribution
      str_detect(tolower(TitleTxt), "\\bvp\\b|vice pres|\\bv\\.p\\b|vice-pres|svp|vice prs") &
        (reportable_comp_from_org + reportable_comp_from_rltd_org <= 10000) ~ "board",
      
      str_detect(tolower(TitleTxt), "\\bvp\\b|vice pres|\\bv\\.p\\b|vice-pres|svp|vice prs") ~ "vp",
      
      TRUE ~ position
    )
  )

# Categorize presidents
data <- data %>%
  mutate(
    position = case_when(
      !is.na(position) ~ position,
      
      # Group 3: route to existing med-staff-president logic
      str_detect(tolower(TitleTxt), "med staff president|medical staff pres|trustee - med staff president|trustee - pres medical staff") ~ "non-exec leader",
      
      # Group 1: explicit board/trustee titled "president" -> board
      str_detect(tolower(TitleTxt), "board president|president/trustee|board member/president|president, ex.officio|ex.officio/president|president/board member|co-president|trustee/president|president, trustee|board chairman/president|trustee, president|board presid|pres, chair") ~ "board",
      
      # Group 2: director + president combos -> ambiguous, use compensation split like bare president
      str_detect(tolower(TitleTxt), "director/president|president/director|director; president|director, president|director-president|president - director|president & director|director & president") &
        (reportable_comp_from_org + reportable_comp_from_rltd_org <= 10000) ~ "board",
      
      str_detect(tolower(TitleTxt), "director/president|president/director|director; president|director, president|director-president|president - director|president & director|director & president") ~ "potential exec pres",
      
      # Group 3 (facility-qualified) + Group 4 (bare "president" and everything else) 
      # -> potential exec pres, letting downstream CEO-resolution logic decide, per original design
      str_detect(tolower(TitleTxt), "\\bpres|prsident")  ~ "potential exec pres",
      
      TRUE ~ position
    )
  )

# assign people left as a board member if the only box checked is individual trustee no matter what the title text says 
data <- data %>%
  mutate(position = ifelse(is.na(position) & individual_trustee_or_director==TRUE & officer_ind==FALSE & key_employee_ind==FALSE & highest_compensated_employee==FALSE, "board", position))

# Assign people who are board members with officer titles within the board
data <- data %>%
  mutate(position = ifelse(is.na(position) & officer_ind==TRUE & str_detect(TitleTxt, "chair|\\bsec|treas|board|member|director|trustee"), "board", position)) 

# Assign physicians who are highest compensated employees
data <- data %>%
  mutate(
    position = ifelse(
      is.na(position) & str_detect(tolower(TitleTxt), 
                                   "physician|surgeon|pharmacist|\\brn\\b|crna|orthoped|cardiolog|hospitalist|registered nurse|
        physician assistant|nurse practi|\\bmd\\b|psychiatrist|\\bdoctor\\b|anesthesiolog|radiolog|
        neurosurg|physical therapist|clinical pharmacist|medical doctor|nurse anesthetist|aprn|\\bnurse\\b|
        \\bfnp\\b|physicist|\\bpa\\b|pediatrician|family practice physician|attending physician"),
      "employee", position
    )
  )

# Anyone left who has director and a positive compensation is probably a more senior leader
data <- data %>%
  mutate(position = ifelse(is.na(position) & str_detect(TitleTxt, "director|manager|chief") & coalesce(reportable_comp_from_org >= 10000, FALSE), "non-exec leader", position))   # PATCHED: NA comp (990-EZ) no longer makes the whole condition NA

# catch any board members that had weird indicator values
data <- data %>%
  mutate(position = ifelse(is.na(position) & str_detect(TitleTxt, "board member|trustee|director|president"), "board", position))

observe <- data %>%
  filter(is.na(position)) 

observe %>% count(TitleTxt, sort=TRUE)

# flag system level people
data <- data %>%
  mutate(system = ifelse(str_detect(TitleTxt, "system|region|network"), 1, 0))

# everyone left is just a very specific specialty physician. assign them all as employees
data <- data %>%
  mutate(position = ifelse(is.na(position), "employee", position))


# ---- RESOLVE ADMINISTRATORS
# A generic administrator is the CEO only if nobody else in the EIN-year has a CEO title. If several generic administrators are
# listed and there is no other CEO, the best paid one is the CEO (ties keep all tied) and the rest become other exec.
# This has to run BEFORE the potential-exec-president cascade below, which asks whether a CEO already exists.
data <- data %>%
  mutate(admin_pay = coalesce(reportable_comp_from_org, 0) + coalesce(reportable_comp_from_rltd_org, 0)) %>%
  group_by(Filer.EIN, TaxYr) %>%
  mutate(
    has_title_ceo = any(position == "ceo"),
    max_admin_pay = ifelse(any(position == "potential admin ceo"), max(admin_pay[position == "potential admin ceo"]), NA_real_)
  ) %>%
  ungroup() %>%
  mutate(position = case_when(
    position == "potential admin ceo" & !has_title_ceo & admin_pay == max_admin_pay ~ "ceo",
    position == "potential admin ceo" ~ "other exec",
    TRUE ~ position
  )) %>%
  select(-admin_pay, -has_title_ceo, -max_admin_pay)

data %>% count(position == "potential admin ceo")   # expect only FALSE

# NOW DEAL WITH POTENTIAL EXECUTIVE PRESIDENTS
# CEO resolution cascade for "potential exec pres" candidates
# Every step recomputes has_ceo/has_other_exec fresh to avoid stale
# group-level stats from before earlier steps ran.

# ---- Step 0: setup, done once 
data <- data %>%
  mutate(
    has_hospital = str_detect(tolower(TitleTxt), "hosp|mc\\b|hc\\b"),
    high_hours = coalesce(avg_hours_per_week >= 15, FALSE),   # PATCHED: hours are NA on 990-EZ rows, count that as "not high" instead of making exec_score NA
    exec_score = 1*(key_employee_ind==TRUE) + 1*has_hospital + 1*high_hours
  )

# ---- Step 1: demote potential-exec-pres where a confirmed CEO already exists 
data <- data %>%
  group_by(Filer.EIN, TaxYr) %>%
  mutate(has_ceo = any(position == "ceo")) %>%
  ungroup() %>%
  mutate(position = ifelse(position == "potential exec pres" & has_ceo, "board", position))

# ---- Step 2: same-person continuity (only where no CEO already exists) 
data <- data %>%
  group_by(Filer.EIN, TaxYr) %>%
  mutate(has_ceo = any(position == "ceo")) %>%  # recompute fresh
  ungroup() %>%
  arrange(Filer.EIN, name_cleaned, TaxYr) %>%
  group_by(Filer.EIN, name_cleaned) %>%
  mutate(
    ceo_lag  = lag(position == "ceo", order_by = TaxYr),
    ceo_lead = lead(position == "ceo", order_by = TaxYr)
  ) %>%
  ungroup() %>%
  mutate(position = case_when(
    position == "potential exec pres" & !has_ceo & (ceo_lag == TRUE | ceo_lead == TRUE) ~ "ceo",
    TRUE ~ position
  )) %>%
  select(-ceo_lag, -ceo_lead)

# ---- Step 3: no CEO, no continuity, other exec exists, exactly one candidate 
data <- data %>%
  group_by(Filer.EIN, TaxYr) %>%
  mutate(
    has_ceo = any(position == "ceo"),  # recompute fresh again
    has_other_exec = any(position %in% c("cfo", "cmo", "coo", "cio", "cno", "other exec")),
    n_potential_exec_pres = sum(position == "potential exec pres", na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(position = case_when(
    position == "potential exec pres" & !has_ceo & has_other_exec & n_potential_exec_pres == 1 ~ "ceo",
    TRUE ~ position
  ))

# ---- Step 4: no CEO, no continuity, multiple (or zero-signal) candidates 
data <- data %>%
  group_by(Filer.EIN, TaxYr) %>%
  mutate(
    has_ceo = any(position == "ceo"),  # recompute fresh again
    has_other_exec = any(position %in% c("cfo", "cmo", "coo", "cio", "cno", "other exec")),
    n_potential_exec_pres = sum(position == "potential exec pres", na.rm = TRUE),
    max_score = ifelse(
      n_potential_exec_pres > 0,
      max(exec_score[position == "potential exec pres"], na.rm = TRUE),
      NA_real_
    )
  ) %>%
  ungroup() %>%
  mutate(position = case_when(
    # Step 4: other exec exists, multiple candidates, scoring tiebreak
    position == "potential exec pres" & !has_ceo & has_other_exec & n_potential_exec_pres > 1 & exec_score == max_score ~ "ceo",
    position == "potential exec pres" & !has_ceo & has_other_exec & n_potential_exec_pres > 1 & exec_score < max_score ~ "board",
    
    # Step 5: no other exec at all -> board, regardless of candidate count
    position == "potential exec pres" & !has_ceo & !has_other_exec ~ "board",
    
    TRUE ~ position
  )) %>%
  select(-has_ceo, -has_other_exec, -n_potential_exec_pres, -max_score, -has_hospital, -high_hours, -exec_score)




# ---- FUNCTION: what the title says the person does, kept separate from seniority (position)
# priority order below; n_functions counts how many groups the title matches so ambiguous titles can be checked.
# (the column is called role_function because `function` is a reserved word in R)
fn_it       <- "\\bcio\\b|\\bcmio\\b|\\bciso\\b|chief info|cheif info|chief medical info|chief med info|information (tech|serv|sys|sec|off)|\\bit\\b|technolog|digital|informatics|analytics"
fn_physician <- "\\bcmos?\\b|chief med|cheif med|chief pediatric|medical affairs|medical staff|physician|\\bmd\\b|medical director|chief of clinical staff|medical group"
fn_nursing  <- "nurs|\\bcno\\b|cheif nurs|patient care|patient serv|\\bdon\\b"
fn_clinical <- "clinical|quality|qlty|\\bcqo\\b|patient safety|pharm|ambulatory|population health|care deliver|care manage|case manage|surg|hospitalist|\\bclinic\\b|\\bpatient\\b|cardio|oncology|behavioral|psych|laborator|radiolog|therap|rehab"
fn_finance  <- "finan|\\bcfo\\b|c\\.f\\.o|chief fin|cheif fin|treasur|accounting|accountant|revenue|budget|controller|audit|reimburs|payer|payor|managed care|business office|chief investment"
fn_hr       <- "human res|\\bhr\\b|people|talent|workforce|learning"
fn_legal    <- "legal|counsel|compliance|\\bcco\\b|risk|privacy|ethics"
fn_academic <- "academic|education|research|scientific|teaching|medical school"
fn_strategy <- "strateg|develop|marketing|planning|philanthrop|foundation|mission|communicat|business dev|innovation|community|government|public affairs|brand|transformation|integration"
fn_ops      <- "operat|\\bcoo\\b|c\\.o\\.o|administrat|chief admin|\\bc{1,2}ao\\b|facilit|support serv|supply chain|plant|real estate|construction|environmental|hospital serv"

data <- data %>%
  mutate(
    title_lc = tolower(TitleTxt),
    n_functions = str_detect(title_lc, fn_it) + str_detect(title_lc, fn_physician) + str_detect(title_lc, fn_nursing) +
      str_detect(title_lc, fn_clinical) + str_detect(title_lc, fn_finance) + str_detect(title_lc, fn_hr) +
      str_detect(title_lc, fn_legal) + str_detect(title_lc, fn_academic) + str_detect(title_lc, fn_strategy) + str_detect(title_lc, fn_ops),
    role_function = case_when(
      position == "ceo"                          ~ "general",
      str_detect(title_lc, fn_it)                ~ "it",
      str_detect(title_lc, fn_physician)         ~ "physician",
      str_detect(title_lc, fn_nursing)           ~ "nursing",
      str_detect(title_lc, fn_clinical)          ~ "other clinical",
      str_detect(title_lc, fn_finance)           ~ "finance",
      str_detect(title_lc, fn_hr)                ~ "hr",
      str_detect(title_lc, fn_legal)             ~ "legal/compliance",
      str_detect(title_lc, fn_academic)          ~ "academic/research",
      str_detect(title_lc, fn_strategy)          ~ "strategy/development",
      str_detect(title_lc, fn_ops)               ~ "operations",
      TRUE                                       ~ "general"
    ),
    clinical_function = role_function %in% c("physician", "nursing", "other clinical")
  ) %>%
  select(-title_lc)

# check: where do senior titles with each function land?
data %>%
  filter(position %in% c("cfo", "cmo", "cno", "coo", "cio", "other exec", "vp")) %>%
  count(role_function, position) %>%
  tidyr::pivot_wider(names_from = position, values_from = n, values_fill = 0)

# change doctor/nurse to 1 if CMO/CNO
data <- data %>%
  mutate(doctor = ifelse(position=="cmo", 1, doctor),
         nurse = ifelse(position=="cno", 1, nurse))

# make sure someone is always or never a doctor/nurse
data <- data %>%
  group_by(name_cleaned, Filer.EIN) %>%
  mutate(doctor=max(doctor), nurse=max(nurse), other_doctor=max(other_doctor), ha=max(ha)) %>%
  ungroup() %>%
  distinct()

summary(data)

### STEP 4: SAVE EXECUTIVE DATA ------------------------------------------
exec_data <- data %>%
  filter(position%in%c("other exec", "ceo", "cfo", "cno", "cio", "cmo", "coo", "vp")) %>%
  select(-individual_trustee_or_director, -highest_compensated_employee, -key_employee_ind, -TitleTxt, -officer_ind)

midlevel <- data %>% filter(position == "non-exec leader") %>%
  select(TaxYr, Filer.EIN, name_cleaned, TitleTxt, doctor)
saveRDS(midlevel, paste0(created_data_path, "/midlevel_leaders.rds"))

# fix typos if before and after are consistent and there is already someone else in the typo position
exec_data <- exec_data %>%
  arrange(Filer.EIN, name_cleaned, TaxYr) %>%
  group_by(Filer.EIN, name_cleaned) %>%
  mutate(
    pos_lag  = lag(position),
    pos_lead = lead(position),
    pos_lag2  = lag(position, 2),
    pos_lead2 = lead(position, 2)
  ) %>%
  ungroup() %>%
  
  group_by(Filer.EIN, TaxYr, position) %>%
  mutate(n_in_position = n()) %>%
  ungroup() %>%
  
  mutate(
    typo_flag = !is.na(pos_lag) &
      !is.na(pos_lead) &
      pos_lag == pos_lead &
      position != pos_lag &
      n_in_position > 1 &
      (pos_lag2 != position | is.na(pos_lag2)) &
      (pos_lead2 != position | is.na(pos_lead2)),
    
    position = if_else(typo_flag, pos_lag, position)
  ) %>%
  select(-pos_lag, -pos_lead, -pos_lag2, -pos_lead2, -n_in_position, -typo_flag)

exec_data <- exec_data %>%
  group_by(Filer.EIN, TaxYr, position) %>%
  mutate(
    has_individual = any(system == 0),
    keep_candidate = (system == 0) | !has_individual
  ) %>%
  ungroup() %>%
  filter(keep_candidate) %>%
  select(-has_individual, -keep_candidate)

# Only keep people with sufficient number of hours at the organization (<5 means they are probably board member of this hosp and exec of a different hosp)
exec_data <- exec_data %>%
  mutate(
    avg_hours_per_week = ifelse(avg_hours_per_week >= 100, NA_real_, avg_hours_per_week),
    avg_hours_per_week_rltd_org = ifelse(avg_hours_per_week_rltd_org >= 100, NA_real_, avg_hours_per_week_rltd_org)
  )

exec_data <- exec_data %>%
  arrange(Filer.EIN, name_cleaned, position, TaxYr) %>%
  group_by(Filer.EIN, name_cleaned, position) %>%
  mutate(
    # rolling average: current year + up to 1 year before/after, to smooth single-year noise
    hours_prev = lag(avg_hours_per_week),
    hours_next = lead(avg_hours_per_week),
    rolling_avg_hours = rowMeans(cbind(avg_hours_per_week, hours_prev, hours_next), na.rm = TRUE)
  ) %>%
  ungroup() %>%
  select(-hours_prev, -hours_next)

# Now apply the keep/drop logic using rolling_avg_hours
exec_data <- exec_data %>%
  group_by(Filer.EIN, TaxYr, position) %>%
  mutate(
    n_in_role = n(),
    keep_candidate = (n_in_role == 1 | coalesce(rolling_avg_hours > 4, FALSE)),   # PATCHED: all-NA hours (990-EZ) no longer give NA, which filter() would silently drop
    n_keep = sum(keep_candidate, na.rm = TRUE)
  ) %>%
  filter(
    (n_keep > 0 & keep_candidate) |
      (n_keep == 0)
  ) %>%
  ungroup() %>%
  select(-n_in_role, -keep_candidate, -n_keep, -rolling_avg_hours)

# categorize people into full-time (all hours at this org), part-time (split hours), or no-time (majority hours at related org)

exec_data <- exec_data %>%
  mutate(
    total_hours = avg_hours_per_week + avg_hours_per_week_rltd_org,
    
    time_allocation = case_when(
      # PATCHED Case 0: no hours AND no compensation reported (990-EZ returns) -> genuinely no signal
      (is.na(total_hours) | total_hours == 0) & (is.na(reportable_comp_from_org) | is.na(reportable_comp_from_rltd_org)) ~ "part-time",
      
      # Case 1: no hours data, and one org clearly pays more -> use compensation as the signal
      (is.na(total_hours) | total_hours == 0) & reportable_comp_from_org > reportable_comp_from_rltd_org ~ "full-time",
      (is.na(total_hours) | total_hours == 0) & reportable_comp_from_org < reportable_comp_from_rltd_org ~ "no-time",
      
      # Case 1b: no hours data AND compensation is tied (including both zero) -> genuinely no signal
      (is.na(total_hours) | total_hours == 0) & reportable_comp_from_org == reportable_comp_from_rltd_org ~ "part-time",
      
      # Case 2: real hours data exists and total_hours > 0 -> use the proportional rules
      avg_hours_per_week >= 0.75 * total_hours ~ "full-time",
      avg_hours_per_week_rltd_org >= 0.85 * total_hours ~ "no-time",
      
      # Case 3: everything else with real data -> part-time
      TRUE ~ "part-time"
    )
  ) %>%
  select(-total_hours)

exec_data %>% count(time_allocation)

#define threshold for "few"
threshold <- 1

exec_data <- exec_data %>%
  group_by(Filer.EIN, position, TaxYr) %>%
  mutate(
    n_working = sum(time_allocation %in% c("full-time", "part-time"), na.rm = TRUE)
  ) %>%
  filter(
    n_working <= threshold |
      (n_working > threshold & time_allocation != "no-time")
  ) %>%
  ungroup() %>%
  select(-n_working)

# diagnostics 
exec_data %>%
  count(position, sort = TRUE)
exec_data %>%
  group_by(Filer.EIN, TaxYr) %>%
  summarise(n_ceo = sum(position %in% c("ceo")), .groups = "drop") %>%
  count(n_ceo)
exec_data %>%
  group_by(Filer.EIN, TaxYr) %>%
  summarise(n_ceo = sum(position %in% c("ceo")), .groups = "drop") %>%
  
  # collapse to firm level
  group_by(Filer.EIN) %>%
  summarise(max_ceo = max(n_ceo), .groups = "drop") %>%
  
  # count firms by their maximum CEO count
  count(max_ceo)

# I'm not going to drop them, just flag that they have a lot of CEOs
exec_data <- exec_data %>%
  group_by(Filer.EIN, TaxYr) %>%
  mutate(n_ceo = sum(position == "ceo", na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(Filer.EIN) %>%
  mutate(max_ceo = max(n_ceo)) %>%
  mutate(many_ceos = (max_ceo > 3)) %>%
  ungroup() %>%
  select(-max_ceo)


# How many EINs in each year have execs? 
exec_data %>%
  group_by(TaxYr) %>%
  distinct(Filer.EIN) %>%
  summarise(n())
exec_data %>%
  group_by(Filer.EIN, TaxYr) %>%
  summarise(n_ceo = sum(position == "ceo"), .groups = "drop") %>%
  count(n_ceo)

# How many people in each position?
exec_data %>%
  distinct(Filer.EIN, name_cleaned, position) %>%
  group_by(position) %>%
  summarise(n())

# what's the average team size?
exec_data %>% group_by(TaxYr, Filer.EIN) %>%
  summarise(obs_count = n()) %>%
  group_by(TaxYr) %>%
  summarise(avg = mean(obs_count), .groups="drop")

# create column for total_compensation
exec_data <- exec_data %>%
  mutate(total_comp = if_else(is.na(reportable_comp_from_org) & is.na(reportable_comp_from_rltd_org),
                              NA_real_,   # PATCHED: 990-EZ returns report no compensation, so keep it unknown instead of $0
                              coalesce(reportable_comp_from_org, 0) + coalesce(reportable_comp_from_rltd_org, 0)))


# only keep relevant variables
exec_data <- exec_data %>%
  select(TaxYr, Filer.EIN, name_cleaned, position, role_function, clinical_function, total_comp, doctor, other_doctor, nurse, ha, time_allocation, prob_male_r, gender_r, system, many_ceos)

summary(exec_data)

# Get an idea of whether the panel is relatively complete
# Hospital-year presence
gap_summary <- exec_data %>%
  distinct(Filer.EIN, TaxYr) %>%
  group_by(Filer.EIN) %>%
  summarise(
    years = list(sort(unique(TaxYr))),
    .groups = "drop"
  ) %>%
  rowwise() %>%
  mutate(
    missing = list(setdiff(seq(min(years), max(years)), years)),
    n_missing = length(missing)
  )

gap_summary <- gap_summary %>%
  filter(n_missing > 0)

# fill gaps of 1-2 years in person-level missing data
exec_data_filled <- exec_data %>%
  mutate(person_id = name_cleaned) %>%
  arrange(Filer.EIN, person_id, TaxYr) %>%
  group_by(Filer.EIN, person_id) %>%
  
  # Compute gaps
  mutate(
    next_year = lead(TaxYr),
    next_position = lead(position),
    gap = next_year - TaxYr,
    should_fill = (gap >= 2) & (gap <= 3) & (position == next_position)
  ) %>%
  
  # Safe list construction
  mutate(
    should_fill = if_else(is.na(should_fill), FALSE, should_fill),
    year_list = pmap(list(TaxYr, gap, should_fill), function(y, g, fill) {
      if (fill) seq(y, y + g - 1) else y
    })
  ) %>%
  
  ungroup() %>%
  
  # Expand
  unnest(year_list) %>%
  
  # Construct final vars
  mutate(
    is_filled = year_list != TaxYr,
    TaxYr = year_list
  ) %>%
  
  # Clean up
  select(-year_list, -next_year, -next_position, -gap, -should_fill) %>%
  
  ungroup()

# now add rows for positions that likely exist even if the person changed 
position_fills <- exec_data_filled %>%
  distinct(Filer.EIN, position, TaxYr) %>%
  arrange(Filer.EIN, position, TaxYr) %>%
  group_by(Filer.EIN, position) %>%
  mutate(
    next_year = lead(TaxYr),
    gap = next_year - TaxYr
  ) %>%
  filter(gap %in% c(2, 3)) %>%
  mutate(year_list = pmap(list(TaxYr, gap), function(y, g) seq(y + 1, y + g - 1))) %>%
  ungroup() %>%
  unnest(year_list) %>%
  mutate(
    TaxYr = year_list,
    name_cleaned = "unknown",
    is_filled = TRUE
  ) %>%
  select(-year_list, -next_year, -gap)

position_fills <- position_fills %>%
  anti_join(
    exec_data_filled %>% select(Filer.EIN, TaxYr, position),
    by = c("Filer.EIN", "TaxYr", "position")
  )

exec_data <- bind_rows(exec_data_filled, position_fills) %>%
  arrange(Filer.EIN, TaxYr, position, name_cleaned)



rm(threshold, firms_with_exec_pres, inspect_exec_pres_firms, exec_data_filled, gap_summary, hospital_years, missing_years, position_fills)

summary(exec_data)

exec_data <- exec_data %>%
  select(-time_allocation)

# Verify doctor status and match to NPI
parse_name <- function(name_str) {
  cleaned <- name_str %>% str_to_upper() %>% str_remove_all("[.,]") %>% str_squish()
  tokens  <- str_split(cleaned, "\\s+")[[1]]
  tokens  <- tokens[tokens != ""]
  is_initial <- nchar(tokens) == 1
  list(
    word_key = paste(sort(tokens[!is_initial]), collapse = " "),
    initials = sort(tokens[is_initial])
  )
}

add_name_keys <- function(df, name_col, prefix) {
  parsed <- map(df[[name_col]], parse_name)
  df[[paste0(prefix, "_word_key")]] <- map_chr(parsed, "word_key")
  df[[paste0(prefix, "_initials")]] <- map(parsed, "initials")
  df
}

initials_compatible <- function(a, b) {
  if (length(a) == 0 || length(b) == 0) return(TRUE)
  setequal(a, b)
}

npi_keys <- readRDS(paste0(created_data_path, "/npidata_names.rds")) %>%
  filter(t_group == "Allopathic & Osteopathic Physicians") %>%
  filter(!(is.na(firstname) & is.na(lastname))) %>%
  mutate(
    enum_year = as.numeric(format(as.Date(enum_date, "%m/%d/%Y"), "%Y")),
    npi_name  = tolower(paste(firstname, lastname))
  ) %>%
  filter(enum_year < 2010) %>%
  distinct(npi, npi_name, enum_year) %>%
  add_name_keys("npi_name", "npi")

## STEP 2. Set aside people you already resolved manually in the past
prior_manual <- read_excel("./CreatedData/prior_manual_npi_matches.xlsx", col_types = "text") %>%
  mutate(ein = str_pad(str_trim(as.character(ein)), 9, pad = "0"),
         chosen_npi = npi_prior_manual)   # the file's NPI column is npi_prior_manual; the code below expects chosen_npi

exec_all <- exec_data %>%
  mutate(Filer.EIN = as.character(Filer.EIN)) %>%
  left_join(prior_manual, by = c("Filer.EIN" = "ein", "name_cleaned" = "name_cleaned"))

exec_prior  <- exec_all %>% filter(!is.na(manual_status))                   
exec_todo   <- exec_all %>% filter(is.na(manual_status)) %>%
  select(-manual_status)                                  

## Apply resolved doctor corrections, then keep doctors only
doctor_corrections <- read_excel("./CreatedData/nondoctor_review_resolved.xlsx") %>%
  filter(doctor == 1) %>%
  mutate(Filer.EIN = as.character(Filer.EIN)) %>%
  distinct(Filer.EIN, name_cleaned) %>%
  mutate(corrected_doctor = 1)

exec_doctors <- exec_todo %>%
  left_join(doctor_corrections, by = c("Filer.EIN", "name_cleaned")) %>%
  mutate(doctor = if_else(!is.na(corrected_doctor), 1, doctor)) %>%
  select(-corrected_doctor) %>%
  filter(coalesce(doctor, 0) == 1) %>%
  mutate(exec_row_id = row_number()) %>%
  add_name_keys("name_cleaned", "exec")

## Find candidate NPIs for each doctor
candidates <- exec_doctors %>%
  select(exec_row_id, exec_word_key, exec_initials) %>%
  inner_join(npi_keys, by = c("exec_word_key" = "npi_word_key"),
             relationship = "many-to-many") %>%
  filter(map2_lgl(exec_initials, npi_initials, initials_compatible)) %>%
  distinct(exec_row_id, npi, npi_name, enum_year)

n_matches <- candidates %>%
  group_by(exec_row_id) %>%
  summarise(n_npi_matches = n_distinct(npi), .groups = "drop")

exec_doctors <- exec_doctors %>%
  left_join(n_matches, by = "exec_row_id") %>%
  mutate(n_npi_matches = replace_na(n_npi_matches, 0L))

## Exactly one match -> attach NPI
exec_unique <- exec_doctors %>%
  filter(n_npi_matches == 1) %>%
  left_join(distinct(candidates, exec_row_id, npi), by = "exec_row_id") %>%
  mutate(npi_source = "auto_unique")


## Multiple or zero matches -> manual review spreadsheet

org_info <- ein_data %>%
  distinct(Filer.EIN, org_name, city, state, zip) %>%
  group_by(Filer.EIN) %>%
  summarise(org_name = last(org_name), city = last(city), state=last(state), zip = last(zip))

context_cols <- c("org_name", "city", "state", "zip")

exec_doctors <- exec_doctors %>%
  left_join(org_info, by="Filer.EIN")

review_base <- exec_doctors %>%
  filter(n_npi_matches != 1) %>%
  group_by(Filer.EIN, name_cleaned) %>%
  summarise(
    exec_row_id   = first(exec_row_id),          # any row works: same name gives the same candidates
    position      = paste(sort(unique(position)), collapse = "; "),
    n_npi_matches = first(n_npi_matches),
    across(any_of(context_cols), first),
    .groups = "drop"
  )

review_multi <- review_base %>%
  filter(n_npi_matches > 1) %>%
  left_join(candidates, by = "exec_row_id") %>%      # one row per candidate NPI
  arrange(Filer.EIN, name_cleaned, npi) %>%
  mutate(chosen_npi = NA_character_, manual_status = NA_character_, notes = NA_character_)

review_zero <- review_base %>%
  filter(n_npi_matches == 0) %>%
  mutate(chosen_npi = NA_character_, manual_status = NA_character_, notes = NA_character_)

# Run once, fill in by hand, then comment out so you don't overwrite your work:
# write_xlsx(list(multiple_matches = review_multi, no_matches = review_zero),
#           "./npi_manual_review.xlsx")

# Join manually matched information to executive data set
keep_unresolved_as_is <- TRUE  # TRUE: unresolved people keep their original doctor flag (marked unresolved)
# FALSE: unresolved people are set to doctor = 0

clean_review <- function(df) {
  df %>%
    mutate(
      Filer.EIN  = str_pad(str_trim(as.character(Filer.EIN)), 9, pad = "0"),
      chosen_raw = str_to_lower(str_trim(as.character(chosen_npi))),
      notes      = na_if(str_trim(as.character(notes)), ""),
      chosen_npi = if_else(str_detect(chosen_raw, "^\\d{10}$"), chosen_raw, NA_character_),
      not_doctor = chosen_raw %in% "not physician",
      specialty_note = !is.na(notes)
    ) %>%
    select(Filer.EIN, name_cleaned, chosen_npi, not_doctor, specialty_note, notes)
}

# col_types = "text" stops Excel reading NPIs as numbers and EINs losing leading zeros
read_review <- function(sheet) {
  read_excel(paste0(created_data_path,"/npi_manual_review_completed.xlsx"), sheet = sheet, col_types = "text") %>% clean_review()
}
review_multi_done <- read_review("multiple_matches")
review_zero_done  <- read_review("no_matches")

first_non_na <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) NA_character_ else as.character(x[1])
}

# One verdict per person-EIN from the manual review
manual_verdict <- bind_rows(review_multi_done, review_zero_done) %>%
  group_by(Filer.EIN, name_cleaned) %>%
  summarise(
    npi            = first_non_na(chosen_npi),
    any_not_doctor = any(not_doctor),
    any_note       = any(specialty_note),
    note_specialty = first_non_na(notes),             # free-text specialty from your review
    .groups = "drop"
  ) %>%
  mutate(
    verified   = case_when(!is.na(npi)     ~ TRUE,     # physician with NPI
                           any_not_doctor  ~ FALSE,    # not a physician
                           any_note        ~ TRUE,     # physician per specialty note, no NPI found
                           TRUE            ~ NA),      # unresolved
    npi_source = case_when(!is.na(npi)     ~ "manual",
                           any_not_doctor  ~ "manual_not_doctor",
                           any_note        ~ "manual_doctor_no_npi",
                           TRUE            ~ NA_character_)
  ) %>%
  select(Filer.EIN, name_cleaned, npi, verified, npi_source, note_specialty)

# sanity checks (expect 0 rows each)
bind_rows(review_multi_done, review_zero_done) %>%
  group_by(Filer.EIN, name_cleaned) %>%
  filter(n_distinct(chosen_npi, na.rm = TRUE) > 1 | (any(not_doctor) & any(!is.na(chosen_npi)))) %>%
  print()

# Verdicts from the other two sources
# prior manual matches (assumes prior_manual has chosen_npi + manual_status like the new sheet)
prior_verdict <- exec_prior %>%
  mutate(chosen_npi = na_if(str_trim(as.character(chosen_npi)), ""),
         manual_status = str_to_lower(str_trim(as.character(manual_status)))) %>%
  group_by(Filer.EIN, name_cleaned) %>%
  summarise(
    npi = first_non_na(chosen_npi),
    any_not_doctor = any(str_detect(coalesce(manual_status, ""), "no_match|no match|not|reject")),
    .groups = "drop"
  ) %>%
  mutate(
    verified   = case_when(!is.na(npi) ~ TRUE, any_not_doctor ~ FALSE, TRUE ~ NA),
    npi_source = case_when(!is.na(npi) ~ "prior_manual", any_not_doctor ~ "prior_manual_not_doctor", TRUE ~ NA_character_)
  ) %>%
  select(Filer.EIN, name_cleaned, npi, verified, npi_source)

# unique automatic matches
auto_verdict <- exec_unique %>%
  mutate(npi = format(npi, scientific = FALSE, trim = TRUE)) %>%   # numeric -> text, to match manual NPIs
  distinct(Filer.EIN, name_cleaned, npi) %>%
  group_by(Filer.EIN, name_cleaned) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(verified = TRUE, npi_source = "auto_unique")

# Combine; priority: manual > prior_manual > auto
person_verdict <- bind_rows(
  manual_verdict  %>% mutate(priority = 1),
  prior_verdict   %>% mutate(priority = 2),
  auto_verdict    %>% mutate(priority = 3)
) %>%
  filter(!is.na(verified)) %>%
  arrange(Filer.EIN, name_cleaned, priority) %>%
  group_by(Filer.EIN, name_cleaned) %>%
  slice(1) %>%
  ungroup() %>%
  select(-priority)

person_verdict %>% count(npi_source, verified)

### FINALIZE DOCTOR COLUMN 
# Start from the full exec_data (not just the doctor subset), re-applying your earlier doctor corrections
exec_final <- exec_data %>%
  mutate(Filer.EIN = as.character(Filer.EIN),
         doctor_raw = coalesce(doctor, 0)) %>%          # flag BEFORE any corrections
  left_join(doctor_corrections, by = c("Filer.EIN", "name_cleaned")) %>%
  mutate(doctor = if_else(!is.na(corrected_doctor), 1, doctor)) %>%
  select(-corrected_doctor) %>%
  left_join(person_verdict, by = c("Filer.EIN", "name_cleaned")) %>%
  mutate(
    doctor_orig = coalesce(doctor, 0),
    doctor = case_when(
      verified %in% TRUE  ~ 1,
      verified %in% FALSE ~ 0,
      # no verdict: either never flagged as a doctor, or flagged but unresolved
      doctor_orig == 1 & keep_unresolved_as_is ~ 1,
      TRUE ~ 0
    ),
    doctor_npi_verified = verified %in% TRUE,
    doctor_unresolved   = doctor_orig == 1 & is.na(verified)
  )

# make doctor / npi constant within person (EIN x name), incl. gap-filled years
exec_final <- exec_final %>%
  group_by(Filer.EIN, name_cleaned) %>%
  mutate(
    doctor              = max(doctor),
    doctor_npi_verified = any(doctor_npi_verified),
    npi                 = first_non_na(npi)
  ) %>%
  ungroup() %>%
  # placeholder "unknown" rows for filled positions are not people -> never doctors
  mutate(doctor = if_else(name_cleaned == "unknown", 0, doctor),
         npi    = if_else(name_cleaned == "unknown", NA_character_, npi))

# join to specialties in NPI data
npis_needed <- exec_final %>% filter(!is.na(npi)) %>% distinct(npi) %>% pull(npi)

npi_spec_lookup <- readRDS(paste0(created_data_path, "/npidata_names.rds")) %>%
  mutate(npi = format(npi, scientific = FALSE, trim = TRUE)) %>%
  filter(npi %in% npis_needed) %>%
  mutate(is_physician_group = t_group == "Allopathic & Osteopathic Physicians") %>%
  arrange(npi, desc(is_physician_group)) %>%
  group_by(npi) %>%
  summarise(npi_taxonomy_code  = first(t_code),
            npi_specialty      = first(t_class),
            npi_all_specialties = paste(unique(t_class), collapse = "; "),
            .groups = "drop")

# how many of your NPIs were found in the raw file? (expect nearly all)
tibble(needed = length(npis_needed), found = nrow(npi_spec_lookup))
npi_spec_lookup %>% count(npi_specialty, sort = TRUE) %>% print(n = 40)

clean_note_specialty <- function(x) {
  x <- str_to_lower(str_squish(x))
  case_when(
    is.na(x)                                              ~ NA_character_,
    str_detect(x, "internal|internist")                   ~ "Internal Medicine",
    str_detect(x, "family|fp$")                           ~ "Family Medicine",
    str_detect(x, "ped|peditat")                          ~ "Pediatrics",
    str_detect(x, "emergency|\\ber\\b")                  ~ "Emergency Medicine",
    str_detect(x, "anesth|anaesth")                       ~ "Anesthesiology",
    str_detect(x, "cardiothoracic|cardiac surg")          ~ "Thoracic Surgery",
    str_detect(x, "plastic")                              ~ "Plastic Surgery",
    str_detect(x, "ortho")                                ~ "Orthopaedic Surgery",
    str_detect(x, "surg")                                 ~ "Surgery",
    str_detect(x, "gastro")                               ~ "Gastroenterology",
    str_detect(x, "otolaryn|\\bent\\b")                   ~ "Otolaryngology",
    str_detect(x, "psych")                                ~ "Psychiatry",
    str_detect(x, "urolog")                               ~ "Urology",
    str_detect(x, "patholog")                             ~ "Pathology",
    str_detect(x, "radiol")                               ~ "Radiology",
    str_detect(x, "ob.?gyn|obstet|gynec")                 ~ "Obstetrics & Gynecology",
    str_detect(x, "nephro")                               ~ "Nephrology",
    str_detect(x, "cardio")                               ~ "Cardiology",
    str_detect(x, "pulmon")                               ~ "Pulmonary Disease",
    str_detect(x, "ophthal")                              ~ "Ophthalmology",
    TRUE                                                  ~ str_to_title(x)   # keep anything unrecognised as-is
  )
}

note_lookup <- person_verdict %>%
  filter(npi_source %in% c("manual", "manual_doctor_no_npi", "manual_not_doctor")) %>%
  transmute(Filer.EIN, name_cleaned, note_specialty = clean_note_specialty(note_specialty))

exec_final <- exec_final %>%
  select(-any_of(c("note_specialty", "npi_specialty", "npi_taxonomy_code", "npi_all_specialties",
                   "specialty", "specialty_source"))) %>%
  left_join(npi_spec_lookup, by = "npi") %>%
  left_join(note_lookup, by = c("Filer.EIN", "name_cleaned")) %>%
  mutate(
    specialty        = coalesce(npi_specialty, note_specialty),
    specialty_source = case_when(!is.na(npi_specialty)  ~ "npi_data",
                                 !is.na(note_specialty) ~ "review_note",
                                 TRUE                   ~ NA_character_)
  )

# diagnostics
exec_final %>% filter(doctor == 1) %>% distinct(Filer.EIN, name_cleaned, specialty_source) %>% count(specialty_source)
exec_final %>% filter(doctor == 1) %>% distinct(Filer.EIN, name_cleaned, specialty) %>% count(specialty, sort = TRUE) %>% print(n = 40)

exec_final <- exec_final %>%
  select(TaxYr, Filer.EIN, name_cleaned, position, role_function, clinical_function, total_comp,
         doctor, doctor_npi_verified, doctor_unresolved, npi,
         specialty, specialty_source, 
         other_doctor, nurse, ha, prob_male_r, gender_r, system, many_ceos, is_filled)


# save executive individual data
saveRDS(exec_final, paste0(created_data_path, "/exec_individual_data.rds"))

### STEP 5: SAVE BOARD DATA ----------------------------------------------
board_data <- data %>%
  filter(position=="board") 

board_data <- board_data %>%
  rowwise() %>%
  mutate(total_comp = sum(reportable_comp_from_org, reportable_comp_from_rltd_org, na.rm=T))

board_data <- board_data %>%
  select(TaxYr, Filer.EIN, name_cleaned, position, reportable_comp_from_org, total_comp, doctor, other_doctor, nurse, ha, prob_male_r, gender_r)

summary(board_data)

saveRDS(board_data, paste0(created_data_path, "/board_individual_data.rds"))

### STEP 6: AGGREGATE TO HOSPITAL LEVEL -----------------------------------

safe_max <- function(x) if (length(x) == 0 || all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)
safe_mean <- function(x) if (length(x) == 0 || all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
safe_sum <- function(x) if (length(x) == 0 || all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)   # PATCHED: new, sum that stays NA when everything is NA
first_non_na <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) NA_character_ else as.character(x[1])
}

# PATCH 2a: persistent_gain(), used for the event variables after the entry-consistency step below.
# A "gain" in year t means the trait was 0 in t-1 and 1 in BOTH t and t+1 (needs 3 consecutive years of data, else NA).
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

exec_all <- exec_final %>%
  mutate(Filer.EIN = as.character(Filer.EIN))

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

exec_all <- exec_all %>%
  mutate(specialty_group = specialty_group_fn(specialty))

# check the mapping -- look at "other_medical" for anything that should be elsewhere
exec_all %>% filter(doctor == 1) %>% distinct(name_cleaned, Filer.EIN, specialty, specialty_group) %>%
  count(specialty_group, specialty, sort = TRUE) %>% print(n = 80)

exec_people <- exec_all %>% filter(!is.na(name_cleaned), name_cleaned != "unknown")

exec_py <- exec_people %>%
  mutate(clinical_row = pmax(coalesce(doctor, 0), coalesce(other_doctor, 0), coalesce(nurse, 0)),
         # seniority order, used to pick the function of someone who holds two roles in a year
         position_rank = case_when(position == "ceo" ~ 1,
                                   position %in% c("cmo", "cno", "cfo", "coo", "cio") ~ 2,
                                   position == "other exec" ~ 3,
                                   position == "vp" ~ 4,
                                   TRUE ~ 5)) %>%
  group_by(Filer.EIN, TaxYr, name_cleaned) %>%
  summarise(
    doctor           = max(coalesce(doctor, 0)),
    clinical         = max(clinical_row),
    doctor_unresolved = max(as.numeric(coalesce(doctor_unresolved, FALSE))),
    female           = safe_max(as.numeric(gender_r == "female")),
    # comp assumed repeated on each position row of the same person-year, so take max rather than sum
    total_comp       = safe_max(total_comp),
    only_vp          = all(position == "vp"),
    # role groupings from Step 3: does ANY of the person's titles name a clinical function, and what is the main function
    clinical_role    = as.numeric(any(coalesce(clinical_function, FALSE))),
    primary_function = first(role_function[order(position_rank)]),
    is_ceo           = any(position == "ceo"),
    is_cmo           = any(position == "cmo"),
    specialty        = first_non_na(specialty),
    specialty_group  = first_non_na(specialty_group),
    .groups = "drop"
  )

exec_py <- exec_py %>%
  mutate(primary_function = coalesce(primary_function, "general"))

hospital_positions <- exec_all %>%
  group_by(Filer.EIN, TaxYr) %>%
  summarise(
    has_ceo = as.integer(any(position == "ceo")),
    has_cmo = as.integer(any(position == "cmo")),            # CHANGED: was cmo OR cno
    has_cno = as.integer(any(position == "cno")),            # new, so the old combined version is recoverable
    has_other_position = as.integer(any(position %in% c("cmo", "cno", "cio", "other exec", "vp", "coo"))),
    many_ceos = first(many_ceos),
    n_placeholder_positions = sum(name_cleaned == "unknown"),
    .groups = "drop"
  )

hospital_composition <- exec_py %>%
  group_by(Filer.EIN, TaxYr) %>%
  summarise(
    exec_team_size = n(),                                    # CHANGED: unique people, not rows
    
    # CHANGED: NA clinical flags no longer silently drop people from the denominator
    exec_perc_clinical_with_vp = safe_mean(clinical),
    exec_perc_md_with_vp       = safe_mean(doctor),
    exec_perc_clinical_no_vp   = safe_mean(clinical[!only_vp]),
    exec_perc_md_no_vp         = safe_mean(doctor[!only_vp]),
    
    n_md_unresolved = sum(doctor == 1 & doctor_unresolved == 1),
    
    total_execs_comp = if (all(is.na(total_comp))) NA_real_ else sum(total_comp, na.rm = TRUE),
    
    # CHANGED: NA (not 0) when the hospital-year has no CEO, so "no CEO" isn't read as "CEO paid $0"
    ceo_comp     = if (any(is_ceo)) safe_sum(total_comp[is_ceo]) else NA_real_,   # PATCHED: NA stays NA (990-EZ)
    ceo_clinical = if (any(is_ceo)) safe_max(clinical[is_ceo]) else NA_real_,
    ceo_md       = if (any(is_ceo)) safe_max(doctor[is_ceo]) else NA_real_,
    # PATCH 1: CEO gender, number of CEOs in the year (>1 flags a transition year), and how many execs have a known gender
    ceo_female   = if (any(is_ceo)) safe_max(female[is_ceo]) else NA_real_,
    n_ceos       = sum(is_ceo),
    n_exec_gender_known = sum(!is.na(female)),
    
    exec_perc_female = safe_mean(female),
    
    # ROLE GROUPINGS (title says the job is clinical) vs TRAINING (clinical flag from credentials / NPI):
    exec_perc_clinical_role        = safe_mean(clinical_role),                                # share of the team in clinical-function roles
    n_clin_trained_clinical_role   = sum(clinical == 1 & clinical_role == 1),                 # clinician in a clinical role
    n_clin_trained_other_role      = sum(clinical == 1 & clinical_role == 0 & primary_function != "general"),   # clinician in finance/ops/IT/HR/...
    n_clin_trained_general_role    = sum(clinical == 1 & clinical_role == 0 & primary_function == "general"),   # clinician in a bare "vp"/ceo title
    n_nonclin_clinical_role        = sum(clinical == 0 & clinical_role == 1),                 # clinical job title held by someone without clinical training
    .groups = "drop"
  )

# number of executives whose main function is each group (one count per person-year)
fn_levels <- c(physician = "physician", nursing = "nursing", other_clinical = "other clinical", finance = "finance",
               operations = "operations", it = "it", hr = "hr", legal = "legal/compliance",
               academic = "academic/research", strategy = "strategy/development", general = "general")
hospital_functions <- exec_py %>%
  group_by(Filer.EIN, TaxYr) %>%
  summarise(
    !!!set_names(map(fn_levels, ~ expr(sum(primary_function == !!.x))), paste0("n_fn_", names(fn_levels))),
    .groups = "drop"
  )

md_py <- exec_py %>% filter(doctor == 1)

spec_groups <- c("primary_care", "internal_medicine", "surgical",
                 "hospital_based", "psych_neuro", "other_medical")

hospital_specialty <- md_py %>%
  group_by(Filer.EIN, TaxYr) %>%
  summarise(
    n_md_exec            = n(),
    n_md_known_specialty = sum(!is.na(specialty_group)),
    n_md_no_specialty    = sum(is.na(specialty_group)),
    n_specialties        = n_distinct(specialty[!is.na(specialty)]),
    n_specialty_groups   = n_distinct(specialty_group[!is.na(specialty_group)]),
    !!!set_names(
      map(spec_groups, ~ expr(sum(specialty_group == !!.x, na.rm = TRUE))),
      paste0("n_md_", spec_groups)
    ),
    .groups = "drop"
  )

count_cols <- paste0("n_md_", spec_groups)
cnt   <- as.matrix(hospital_specialty[count_cols])
known <- hospital_specialty$n_md_known_specialty

for (g in spec_groups) {
  hospital_specialty[[paste0("share_md_", g)]] <-
    ifelse(known > 0, hospital_specialty[[paste0("n_md_", g)]] / known, NA_real_)
  hospital_specialty[[paste0("any_md_", g)]] <-
    as.integer(hospital_specialty[[paste0("n_md_", g)]] > 0)
}
# diversity of the physician mix: 1 - Herfindahl across groups (0 = all one group; higher = more mixed)
hospital_specialty$md_specialty_diversity <- ifelse(known > 0, 1 - rowSums((cnt / known)^2), NA_real_)
hospital_specialty$md_all_one_group <- as.integer(hospital_specialty$n_specialty_groups == 1)

# specialty of the CEO / CMO when they are physicians
leader_specialty <- exec_py %>%
  filter(doctor == 1, is_ceo | is_cmo) %>%
  group_by(Filer.EIN, TaxYr) %>%
  summarise(
    ceo_specialty_group = first_non_na(specialty_group[is_ceo]),
    cmo_specialty_group = first_non_na(specialty_group[is_cmo]),
    .groups = "drop"
  )


## 3. TURNOVER FUNCTIONS 
as_set <- function(x) if (is.null(x) || length(x) == 0) character(0) else x

compute_persistent_entry_count <- function(curr, prev, next_people, year, lag_year, lead_year) {
  if (is.na(lag_year) || is.na(lead_year) || (year - lag_year != 1) || (lead_year - year != 1)) return(NA_integer_)
  entrants <- setdiff(as_set(curr), as_set(prev))
  length(intersect(entrants, as_set(next_people)))
}
compute_persistent_entry <- function(curr, prev, next_people, year, lag_year, lead_year) {
  n <- compute_persistent_entry_count(curr, prev, next_people, year, lag_year, lead_year)
  if (is.na(n)) NA_integer_ else as.integer(n > 0)
}
compute_exit_count <- function(curr, prev, year, lag_year) {
  if (is.na(lag_year) || (year - lag_year != 1)) return(NA_integer_)
  length(setdiff(as_set(prev), as_set(curr)))
}
compute_exit_indicator <- function(curr, prev, year, lag_year) {
  n <- compute_exit_count(curr, prev, year, lag_year)
  if (is.na(n)) NA_integer_ else as.integer(n > 0)
}
# NEW (optional, symmetric with persistent entry): people present last year, gone this year AND next year
compute_persistent_exit_count <- function(curr, prev, next_people, year, lag_year, lead_year) {
  if (is.na(lag_year) || is.na(lead_year) || (year - lag_year != 1) || (lead_year - year != 1)) return(NA_integer_)
  leavers <- setdiff(as_set(prev), as_set(curr))
  length(setdiff(leavers, as_set(next_people)))
}
compute_overlap <- function(curr, prev, year, lag_year) {
  if (is.na(lag_year) || (year - lag_year != 1)) return(NA_real_)
  length(intersect(as_set(curr), as_set(prev)))
}
compute_union <- function(curr, prev, year, lag_year) {
  if (is.na(lag_year) || (year - lag_year != 1)) return(NA_real_)
  length(union(as_set(curr), as_set(prev)))
}

# CHANGED: takes a vector of positions instead of a string that gets parsed into code
build_team_sets <- function(data, positions = NULL) {
  if (!is.null(positions)) data <- data %>% filter(position %in% positions)
  data %>%
    group_by(Filer.EIN, TaxYr) %>%
    summarise(people = list(unique(name_cleaned)), .groups = "drop") %>%
    arrange(Filer.EIN, TaxYr) %>%
    group_by(Filer.EIN) %>%
    mutate(people_lag = lag(people), people_lead = lead(people),
           year_lag = lag(TaxYr), year_lead = lead(TaxYr)) %>%
    ungroup()
}

## 4. EXEC TURNOVER
# CHANGED: built from exec_people so "unknown" placeholders never count as entrants or leavers
exec_team_sets <- build_team_sets(exec_people) %>%
  mutate(
    n_exec_entry_persistent = pmap_int(list(people, people_lag, people_lead, TaxYr, year_lag, year_lead), compute_persistent_entry_count),
    exec_entry_indicator_persistent = pmap_int(list(people, people_lag, people_lead, TaxYr, year_lag, year_lead), compute_persistent_entry),
    n_exec_exit = pmap_int(list(people, people_lag, TaxYr, year_lag), compute_exit_count),
    exec_exit_indicator = pmap_int(list(people, people_lag, TaxYr, year_lag), compute_exit_indicator),
    n_exec_exit_persistent = pmap_int(list(people, people_lag, people_lead, TaxYr, year_lag, year_lead), compute_persistent_exit_count),
    exec_overlap = pmap_dbl(list(people, people_lag, TaxYr, year_lag), compute_overlap),
    exec_union = pmap_dbl(list(people, people_lag, TaxYr, year_lag), compute_union),
    exec_stability = ifelse(exec_union > 0, exec_overlap / exec_union, NA_real_)
  )

exec_team_metrics <- exec_team_sets %>%
  select(Filer.EIN, TaxYr, n_exec_entry_persistent, exec_entry_indicator_persistent,
         n_exec_exit, exec_exit_indicator, n_exec_exit_persistent, exec_stability)

ceo_metrics <- build_team_sets(exec_people, "ceo") %>%
  mutate(ceo_entry = pmap_int(list(people, people_lag, people_lead, TaxYr, year_lag, year_lead), compute_persistent_entry)) %>%
  select(Filer.EIN, TaxYr, ceo_entry)

cmo_metrics <- build_team_sets(exec_people, "cmo") %>%
  mutate(cmo_entry = pmap_int(list(people, people_lag, people_lead, TaxYr, year_lag, year_lead), compute_persistent_entry)) %>%
  select(Filer.EIN, TaxYr, cmo_entry)

## SHARED LEADERSHIP ACROSS HOSPITALS
# For each hospital-year: how much of the executive team also appears on another hospital's executive team in the same year.
# - team = real executives only (no "unknown" placeholders, no gap-filled rows), one entry per person
# - a person is matched across hospitals by the non-initial words of the name in any order ("smith john" = "john a smith"),
#   and only names with two or more such words are used (one-word names would match too many different people)
# - percentages divide by the focal hospital's own team size; the counts are kept because small teams make percentages coarse
# - the "strict" versions only count a partner hospital when it shares TWO OR MORE people: a single shared common name
#   is often a coincidence (in the data about half of one-person overlaps are in the same state, against 72-82% for 2+)
person_key_fn <- function(x) {
  ux   <- unique(x)
  keys <- vapply(str_split(ux, "[\\s\\-]+"), function(tok) paste(sort(tok[nchar(tok) > 1]), collapse = " "), character(1))
  unname(keys[match(x, ux)])
}

overlap_people <- exec_people %>%
  filter(!coalesce(is_filled, FALSE)) %>%
  mutate(person_key = person_key_fn(name_cleaned)) %>%
  filter(str_detect(person_key, " ")) %>%
  distinct(Filer.EIN, TaxYr, person_key)

shared_team_sizes <- overlap_people %>% count(Filer.EIN, TaxYr, name = "shared_team_size")

# every pair (hospital, other hospital) in the same year that has at least one person in common
pairs_keys <- overlap_people %>%
  inner_join(overlap_people, by = c("TaxYr", "person_key"), suffix = c("", "_other"), relationship = "many-to-many") %>%
  filter(Filer.EIN != Filer.EIN_other)

pair_overlap <- pairs_keys %>%
  count(Filer.EIN, TaxYr, Filer.EIN_other, name = "n_shared") %>%
  left_join(shared_team_sizes, by = c("Filer.EIN", "TaxYr")) %>%
  mutate(pct_shared = n_shared / shared_team_size)

# state of every hospital, to check that the partner is plausible (same-state partners are far more likely to be real)
ein_col <- intersect(c("Filer.EIN", "ein"), names(ein_data))[1]
ein_state <- ein_data %>%
  transmute(Filer.EIN = as.character(.data[[ein_col]]), state = as.character(state)) %>%
  filter(!is.na(state)) %>%
  group_by(Filer.EIN) %>%
  summarise(state = last(state), .groups = "drop")

pick_best_partner <- function(pairs, prefix) {
  pairs %>%
    arrange(Filer.EIN, TaxYr, desc(pct_shared), desc(n_shared), Filer.EIN_other) %>%   # ties: more people, then lowest EIN (reproducible)
    group_by(Filer.EIN, TaxYr) %>%
    slice(1) %>%
    ungroup() %>%
    left_join(ein_state, by = "Filer.EIN") %>%
    left_join(ein_state %>% rename(Filer.EIN_other = Filer.EIN, state_other = state), by = "Filer.EIN_other") %>%
    transmute(Filer.EIN, TaxYr,
              !!paste0(prefix, "_pct")        := pct_shared,
              !!paste0(prefix, "_n")          := n_shared,
              !!paste0(prefix, "_partner")    := Filer.EIN_other,
              !!paste0(prefix, "_same_state") := as.integer(state == state_other))
}

best_any    <- pick_best_partner(pair_overlap, "max_overlap")
best_strict <- pick_best_partner(pair_overlap %>% filter(n_shared >= 2), "max_overlap2")

# people who also lead ANY other hospital (union over partners, so a leader shared with B and another shared with C both count)
people_any <- pairs_keys %>%
  left_join(pair_overlap %>% select(Filer.EIN, TaxYr, Filer.EIN_other, n_shared),
            by = c("Filer.EIN", "TaxYr", "Filer.EIN_other")) %>%
  group_by(Filer.EIN, TaxYr) %>%
  summarise(n_shared_any        = n_distinct(person_key),
            n_shared_any_strict = n_distinct(person_key[n_shared >= 2]),
            n_overlap_hospitals = n_distinct(Filer.EIN_other),
            .groups = "drop")

shared_leadership <- shared_team_sizes %>%
  left_join(best_any,    by = c("Filer.EIN", "TaxYr")) %>%
  left_join(best_strict, by = c("Filer.EIN", "TaxYr")) %>%
  left_join(people_any,  by = c("Filer.EIN", "TaxYr")) %>%
  mutate(
    # a hospital-year with a comparable team but no overlap is a genuine 0 (hospital-years with no comparable team stay NA after the join)
    across(c(max_overlap_pct, max_overlap_n, max_overlap2_pct, max_overlap2_n,
             n_shared_any, n_shared_any_strict, n_overlap_hospitals), ~ coalesce(.x, 0)),
    shared_any_pct        = n_shared_any / shared_team_size,
    shared_any_strict_pct = n_shared_any_strict / shared_team_size,
    any_shared_leader     = as.integer(n_shared_any > 0),
    shared_leader_strict  = as.integer(max_overlap2_n >= 2)
  )

# checks: how common, and does it look real (partners in the same state)?
shared_leadership %>% group_by(TaxYr) %>%
  summarise(n = n(), any = mean(any_shared_leader), strict = mean(shared_leader_strict), mean_max_pct = mean(max_overlap_pct)) %>% print(n = 30)
shared_leadership %>% filter(max_overlap_n > 0) %>% group_by(max_overlap_n = pmin(max_overlap_n, 4)) %>%
  summarise(n = n(), same_state = mean(max_overlap_same_state, na.rm = TRUE))   # expect ~0.5 for 1 shared, ~0.9 for 2+

saveRDS(pair_overlap, paste0(created_data_path, "/exec_pair_overlap.rds"))   # hospital pairs, to inspect who overlaps with whom

## 5. ASSEMBLE HOSPITAL EXEC DATA 
hospital_exec_data <- hospital_positions %>%
  left_join(hospital_composition, by = c("Filer.EIN", "TaxYr")) %>%
  left_join(exec_team_metrics, by = c("Filer.EIN", "TaxYr")) %>%
  left_join(ceo_metrics, by = c("Filer.EIN", "TaxYr")) %>%
  left_join(cmo_metrics, by = c("Filer.EIN", "TaxYr")) %>%
  left_join(shared_leadership, by = c("Filer.EIN", "TaxYr")) %>%
  left_join(hospital_specialty, by = c("Filer.EIN", "TaxYr")) %>%
  left_join(leader_specialty, by = c("Filer.EIN", "TaxYr")) %>%
  left_join(hospital_functions, by = c("Filer.EIN", "TaxYr")) %>%
  mutate(
    across(starts_with("n_fn_"), ~ coalesce(.x, 0L)),
    across(c(n_clin_trained_clinical_role, n_clin_trained_other_role, n_clin_trained_general_role, n_nonclin_clinical_role), ~ coalesce(.x, 0L)),
    # hospital-years with NO physician executives: counts are genuinely 0 (shares/diversity stay NA)
    across(c(n_md_exec, n_md_known_specialty, n_md_no_specialty, n_specialties, n_specialty_groups,
             all_of(count_cols), starts_with("any_md_")), ~ coalesce(.x, 0L)),
    exec_team_size = coalesce(exec_team_size, 0L)
  )

# clinical / CMO changes at the hospital level
hospital_exec_data <- hospital_exec_data %>%
  arrange(Filer.EIN, TaxYr) %>%
  group_by(Filer.EIN) %>%
  mutate(
    is_adjacent = !is.na(lag(TaxYr)) & (TaxYr - lag(TaxYr) == 1),
    lag_clin_with_vp = lag(exec_perc_clinical_with_vp),
    lag_clin_no_vp   = lag(exec_perc_clinical_no_vp),
    lag_ceo_clinical = lag(ceo_clinical),
    
    clinical_exec_entry_with_vp = as.integer(is_adjacent & lag_clin_with_vp == 0 & exec_perc_clinical_with_vp > 0),
    clinical_exec_entry_no_vp   = as.integer(is_adjacent & lag_clin_no_vp == 0 & exec_perc_clinical_no_vp > 0),
    ceo_clinical_entry          = as.integer(is_adjacent & lag_ceo_clinical == 0 & ceo_clinical == 1),
    exec_delta_pct_clinical_with_vp = ifelse(is_adjacent, exec_perc_clinical_with_vp - lag_clin_with_vp, NA_real_),
    exec_delta_pct_clinical_no_vp   = ifelse(is_adjacent, exec_perc_clinical_no_vp - lag_clin_no_vp, NA_real_),
    
    # NEW: first year a hospital gains a physician from a specialty group it didn't have last year
    new_specialty_group_entry = as.integer(
      is_adjacent & (
        (any_md_primary_care == 1 & lag(any_md_primary_care) == 0) |
          (any_md_internal_medicine == 1 & lag(any_md_internal_medicine) == 0) |
          (any_md_surgical == 1 & lag(any_md_surgical) == 0) |
          (any_md_hospital_based == 1 & lag(any_md_hospital_based) == 0) |
          (any_md_psych_neuro == 1 & lag(any_md_psych_neuro) == 0) |
          (any_md_other_medical == 1 & lag(any_md_other_medical) == 0))
    ),
    delta_md_specialty_diversity = ifelse(is_adjacent, md_specialty_diversity - lag(md_specialty_diversity), NA_real_)
  ) %>%
  ungroup() %>%
  select(-is_adjacent, -lag_clin_with_vp, -lag_clin_no_vp, -lag_ceo_clinical)

fix_entry_consistency <- function(data, has_col, entry_col) {
  data %>%
    arrange(Filer.EIN, TaxYr) %>%
    group_by(Filer.EIN) %>%
    mutate(
      is_adjacent = !is.na(lag(TaxYr)) & (TaxYr - lag(TaxYr) == 1),
      !!entry_col := ifelse(.data[[has_col]] == 0 & is_adjacent, 0L, .data[[entry_col]])
    ) %>%
    ungroup() %>%
    select(-is_adjacent)
}
hospital_exec_data <- hospital_exec_data %>%
  fix_entry_consistency("has_cmo", "cmo_entry") %>%
  fix_entry_consistency("has_ceo", "ceo_entry")

# PATCH 2b: persistent-gain event variables for CEO gender and CEO physician background
hospital_exec_data <- hospital_exec_data %>%
  persistent_gain("ceo_female", "ceo_female_gain") %>%
  persistent_gain("ceo_md",     "ceo_md_gain")

# PATCH 3: persistent-gain event variables for each CEO specialty group
# ceo_is_<group> is 1 when the CEO is a physician of that group, 0 when there is a CEO but not of that group,
# NA when the hospital-year has no CEO. A physician CEO with an unknown specialty counts as 0 for every group.
for (g in spec_groups) {
  hospital_exec_data[[paste0("ceo_is_", g)]] <-
    if_else(hospital_exec_data$has_ceo == 1,
            as.integer(hospital_exec_data$ceo_md %in% 1 & hospital_exec_data$ceo_specialty_group %in% g),
            NA_integer_)
  hospital_exec_data <- persistent_gain(hospital_exec_data,
                                        paste0("ceo_is_", g), paste0("ceo_gain_", g))
}

## 6. CEO-MISSING FLAGS AND 5-YEAR PRESENCE
# CHANGED: computed for EVERY hospital (before, hospitals that never had a CEO got 0 missing years,
# i.e. looked better than hospitals that were only sometimes missing a CEO)
ceo_missing <- hospital_exec_data %>%
  group_by(Filer.EIN) %>%
  summarise(
    ever_ceo   = any(has_ceo == 1),
    always_ceo = all(has_ceo == 1),
    partial_ceo = ever_ceo & !always_ceo,
    never_ceo   = !ever_ceo,
    .groups = "drop"
  )
ceo_missing %>% summarise(n_partial = sum(partial_ceo), n_never = sum(never_ceo))

hospital_exec_data <- hospital_exec_data %>%
  left_join(ceo_missing %>% select(Filer.EIN, partial_ceo, never_ceo), by = "Filer.EIN")

# WINDOW-FREE coverage of each hospital (no filtering). prefix = "exec" or "board".
coverage_fn <- function(data, prefix) {
  data %>%
    distinct(Filer.EIN, TaxYr) %>%
    arrange(Filer.EIN, TaxYr) %>%
    group_by(Filer.EIN) %>%
    mutate(run_id = TaxYr - row_number()) %>%
    group_by(Filer.EIN, run_id) %>%
    summarise(run_start = min(TaxYr), run_end = max(TaxYr), run_length = n(), .groups = "drop") %>%
    group_by(Filer.EIN) %>%
    summarise(first_year = min(run_start), last_year = max(run_end),
              n_years = sum(run_length), n_gaps = n() - 1L,
              longest_run = max(run_length), .groups = "drop") %>%
    rename_with(~ paste0(prefix, "_", .x), -Filer.EIN)
}

hospital_exec_data <- hospital_exec_data %>%
  left_join(coverage_fn(hospital_exec_data, "exec"), by = "Filer.EIN")

summary(hospital_exec_data)
saveRDS(hospital_exec_data, paste0(created_data_path, "/hospital_exec_data.rds"))

## BOARD
# NOTE: board_data's doctor flag has NOT been through the NPI verification above (that was exec-only),
# so board_perc_md / board_perc_clinical are on the older, unverified definition.
board_people <- board_data %>% filter(!is.na(name_cleaned), name_cleaned != "unknown")

# CHANGED: one row per person-year (same double-counting issue as execs if someone has two board rows)
board_py <- board_people %>%
  mutate(clinical_row = pmax(coalesce(doctor, 0), coalesce(other_doctor, 0), coalesce(nurse, 0))) %>%
  group_by(Filer.EIN, TaxYr, name_cleaned) %>%
  summarise(
    doctor = max(coalesce(doctor, 0)),
    clinical = max(clinical_row),
    female = safe_max(as.numeric(gender_r == "female")),
    total_comp = safe_max(total_comp),
    .groups = "drop"
  )

hospital_board_data <- board_py %>%
  group_by(Filer.EIN, TaxYr) %>%
  summarise(
    board_size = n(),
    board_perc_clinical = safe_mean(clinical),
    board_perc_md = safe_mean(doctor),
    board_perc_female = safe_mean(female),
    board_perc_compensated = safe_mean(as.numeric(total_comp > 0)),
    total_board_comp = if (all(is.na(total_comp))) NA_real_ else sum(total_comp, na.rm = TRUE),
    .groups = "drop"
  )

compute_entry_count <- function(curr, prev, year, lag_year) {
  if (is.na(lag_year) || (year - lag_year != 1)) return(NA_integer_)
  length(setdiff(as_set(curr), as_set(prev)))
}
compute_entry_indicator <- function(curr, prev, year, lag_year) {
  n <- compute_entry_count(curr, prev, year, lag_year)
  if (is.na(n)) NA_integer_ else as.integer(n > 0)
}

# board entry here is RAW (any new name vs last year), whereas exec entry is "persistent" (still there next year).
# Keep that in mind when comparing the two.
board_sets <- build_team_sets(board_people) %>%
  mutate(
    n_board_entry = pmap_int(list(people, people_lag, TaxYr, year_lag), compute_entry_count),
    board_entry_indicator = pmap_int(list(people, people_lag, TaxYr, year_lag), compute_entry_indicator),
    n_board_exit = pmap_int(list(people, people_lag, TaxYr, year_lag), compute_exit_count),
    board_exit_indicator = pmap_int(list(people, people_lag, TaxYr, year_lag), compute_exit_indicator),
    board_overlap = pmap_dbl(list(people, people_lag, TaxYr, year_lag), compute_overlap),
    board_union = pmap_dbl(list(people, people_lag, TaxYr, year_lag), compute_union),
    board_stability = ifelse(board_union > 0, board_overlap / board_union, NA_real_)
  )

board_metrics <- board_sets %>%
  select(Filer.EIN, TaxYr, n_board_entry, board_entry_indicator,
         n_board_exit, board_exit_indicator, board_stability)

# CHANGED: explicit join keys (the old join also matched on board_size, which was in both tables)
hospital_board_data <- hospital_board_data %>%
  left_join(board_metrics, by = c("Filer.EIN", "TaxYr"))

# CHANGED: window-free board coverage (same definition as exec)
hospital_board_data <- hospital_board_data %>%
  left_join(coverage_fn(hospital_board_data, "board"), by = "Filer.EIN")

summary(hospital_board_data)
saveRDS(hospital_board_data, paste0(created_data_path, "/hospital_board_data.rds"))

## 8. JOIN EXEC AND BOARD ----------------------------------------------------------------------
hospital_data <- full_join(hospital_exec_data, hospital_board_data, by = c("Filer.EIN", "TaxYr"))
# Note: board data was not gap-filled, so board-side NAs in some years are expected.

# CHANGED: NO hospital filter here. After the full_join the hospital-level coverage columns are NA on rows that exist
# in only one dataset, so fill them within hospital, and add row-level presence flags.
hospital_data <- hospital_data %>%
  mutate(in_exec_data = !is.na(exec_team_size),
         in_board_data = !is.na(board_size)) %>%
  group_by(Filer.EIN) %>%
  mutate(across(starts_with("exec_") & (ends_with("_first_year") | ends_with("_last_year") |
                                          ends_with("_n_years") | ends_with("_n_gaps") | ends_with("_longest_run")),
                ~ .x[!is.na(.x)][1]),
         across(starts_with("board_") & (ends_with("_first_year") | ends_with("_last_year") |
                                           ends_with("_n_years") | ends_with("_n_gaps") | ends_with("_longest_run")),
                ~ .x[!is.na(.x)][1])) %>%
  ungroup()

# how many hospitals each window would keep (for choosing the window): see stable_sample() in stable_sample.R

# sanity checks on the specialty measures
hospital_data %>% filter(n_md_exec > 0) %>%
  summarise(mean_known = mean(n_md_known_specialty / n_md_exec),
            mean_groups = mean(n_specialty_groups),
            mean_diversity = mean(md_specialty_diversity, na.rm = TRUE))
hospital_data %>% filter(n_md_exec > 0) %>% count(n_specialty_groups)

saveRDS(hospital_data, paste0(created_data_path, "/hospital_leader_data.rds"))
