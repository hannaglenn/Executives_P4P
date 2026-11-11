library(dplyr)
library(readr)

if (!exists("raw_data_path")) source("./RScripts/paths.R")   # 11 Nov: so the script runs on its own
# 11 Nov: the raw yearly folders sit in RawData/HCRIS/HCRIS_v1996 on your machine; fall back to RawData/HCRIS_v1996.
# Outputs (final_HCRIS_v1996.csv) still go to raw_data_path, where HCRIS_combine.R and data02 look for them.
hcris_raw_dir <- file.path(raw_data_path, "HCRIS", "HCRIS_v1996")
if (!dir.exists(hcris_raw_dir)) hcris_raw_dir <- file.path(raw_data_path, "HCRIS_v1996")
cat("Reading raw HCRIS v1996 folders from:", hcris_raw_dir, "\n")
# 11 Nov: find a raw file inside a year folder, whatever its case or sub-folder (e.g. HOSP10_2012_NMRC.CSV, hosp10_2012_nmrc.csv,
# HOSP10FY2012/HOSP10FY2012/...). Matches files whose name contains the year and ends in _ALPHA / _NMRC / _RPT .csv.
find_raw <- function(dir, stem) {
  part <- base::sub(".*_", "", stem); yr <- base::regmatches(stem, base::regexpr("[0-9]{4}", stem))   # base:: because a session function called sub() can mask it
  f <- list.files(dir, pattern = "\\.csv$", recursive = TRUE, ignore.case = TRUE, full.names = TRUE)
  hit <- f[grepl(yr, basename(f)) & grepl(paste0("_", part, "\\.csv$"), basename(f), ignore.case = TRUE)]
  if (length(hit)) return(hit[1])
  if (!dir.exists(dir)) { message("  Folder does not exist: ", dir); return(NA_character_) }
  all_f <- list.files(dir, recursive = TRUE)
  message("  No *", yr, "*_", part, ".csv in ", dir, ". It contains: ",
          if (length(all_f)) paste(head(all_f, 8), collapse = ", ") else "(nothing)",
          if (any(grepl("\\.zip$", all_f, ignore.case = TRUE))) "  <- unzip these first" else "")
  NA_character_
}

#### Thank you to my advisor, Ian McCarthy, for writing this code and making it publicly available. If interested, you can find this resource
#### at https://ianmccarthyecon.netlify.app/resources/


# Variable locations ------------------------------------------------------

hcris.vars = NULL
hcris.vars = rbind(hcris.vars,c('beds','S300001','01200','0100','numeric'))
hcris.vars = rbind(hcris.vars,c('tot_charges','G300000','00100','0100','numeric'))
hcris.vars = rbind(hcris.vars,c('tot_discounts','G300000','00200','0100','numeric'))
hcris.vars = rbind(hcris.vars,c('tot_operating_exp','G300000','00400','0100','numeric'))
hcris.vars = rbind(hcris.vars,c('ip_charges','G200000','00100','0100','numeric'))
hcris.vars = rbind(hcris.vars,c('icu_charges','G200000','01500','0100','numeric'))
hcris.vars = rbind(hcris.vars,c('ancillary_charges','G200000','01700','0100','numeric'))
hcris.vars = rbind(hcris.vars,c('tot_discharges','S300001','00100','1500','numeric'))
hcris.vars = rbind(hcris.vars,c('mcare_discharges','S300001','00100','1300','numeric'))
hcris.vars = rbind(hcris.vars,c('mcaid_discharges','S300001','00100','1400','numeric'))
hcris.vars = rbind(hcris.vars,c('tot_mcare_payment','E00A18A','01600','0100','numeric'))
hcris.vars = rbind(hcris.vars,c('secondary_mcare_payment','E00A18A','01700','0100','numeric'))
hcris.vars = rbind(hcris.vars,c('street','S200000','00100','0100','alpha'))
hcris.vars = rbind(hcris.vars,c('city','S200000','00101','0100','alpha'))
hcris.vars = rbind(hcris.vars,c('state','S200000','00101','0200','alpha'))
hcris.vars = rbind(hcris.vars,c('zip','S200000','00101','0300','alpha'))
hcris.vars = rbind(hcris.vars,c('county','S200000','00101','0400','alpha'))
hcris.vars = rbind(hcris.vars,c('uncomp_care','S100000','03000','0100','numeric'))
hcris.vars = rbind(hcris.vars,c('cost_to_charge','S100000','02400','0100','numeric'))
hcris.vars = rbind(hcris.vars,c('new_cap_ass','A700002','00900','0200','numeric'))
hcris.vars = rbind(hcris.vars,c('cash','G000000','00100','0100','numeric'))
hcris.vars = rbind(hcris.vars,c('tot_pat_rev','G300000','00100','0100','numeric'))
hcris.vars = rbind(hcris.vars,c('allowance','G300000','00200','0100','numeric'))
hcris.vars = rbind(hcris.vars,c('net_pat_rev','G300000','00300','0100','numeric'))

# variables added by me
hcris.vars = rbind(hcris.vars,c('land_purch','A700001','00100','0200','numeric'))
hcris.vars = rbind(hcris.vars,c('land_assets','A700001','00100','0700','numeric'))
hcris.vars = rbind(hcris.vars,c('landimpr_purch','A700001','00200','0200','numeric'))
hcris.vars = rbind(hcris.vars,c('landimpr_assets','A700001','00200','0700','numeric'))
hcris.vars = rbind(hcris.vars,c('build_purch','A700001','00300','0200','numeric'))
hcris.vars = rbind(hcris.vars,c('build_assets','A700001','00300','0700','numeric'))
hcris.vars = rbind(hcris.vars,c('buildimpr_purch','A700001','00400','0200','numeric'))
hcris.vars = rbind(hcris.vars,c('buildimpr_assets','A700001','00400','0700','numeric'))
hcris.vars = rbind(hcris.vars,c('fixedequipment_purch','A700001','00500','0200','numeric'))
hcris.vars = rbind(hcris.vars,c('fixedequipment_assets','A700001','00500','0700','numeric'))
hcris.vars = rbind(hcris.vars,c('movableequipment_purch','A700001','00600','0200','numeric'))
hcris.vars = rbind(hcris.vars,c('moveableequipment_assets','A700001','00600','0700','numeric'))
hcris.vars = rbind(hcris.vars,c('total_purch','A700001','00900','0200','numeric'))
hcris.vars = rbind(hcris.vars,c('total_assets','A700001','00900','0700','numeric'))
hcris.vars = rbind(hcris.vars,c('labor_costs','S300003','00600','0100','numeric'))
hcris.vars = rbind(hcris.vars,c('capital_cost_buildings_old','A000000','00100','0200','numeric'))
hcris.vars = rbind(hcris.vars,c('capital_cost_buildings_new','A000000','00300','0200','numeric'))
hcris.vars = rbind(hcris.vars,c('capital_cost_movableequip_old','A000000','00200','0200','numeric'))
hcris.vars = rbind(hcris.vars,c('capital_cost_movableequip_new','A000000','00400','0200','numeric'))
hcris.vars = rbind(hcris.vars,c('operating_expenses','A000000','10100','0100','numeric'))
hcris.vars = rbind(hcris.vars,c('medrecords_expenses','A000000','01700','0200','numeric'))
hcris.vars = rbind(hcris.vars,c('mcaid_beddays','S300001','00100','0500','numeric'))
hcris.vars = rbind(hcris.vars,c('mcare_beddays','S300001','00100','0400','numeric'))
hcris.vars = rbind(hcris.vars,c('total_beddays','S300001','00100','0600','numeric'))
# added 6 Nov: observation and inpatient days (2552-96 S-3 Part I: line 26 = observation bed days, line 12 = total hospital;
# columns 4 = Title XVIII, 6 = total all patients - same columns as mcare_beddays / total_beddays above)
hcris.vars = rbind(hcris.vars,c('obs_days_tot','S300001','02600','0600','numeric'))
hcris.vars = rbind(hcris.vars,c('obs_days_mcare','S300001','02600','0400','numeric'))
hcris.vars = rbind(hcris.vars,c('inpt_days_tot','S300001','01200','0600','numeric'))
hcris.vars = rbind(hcris.vars,c('inpt_days_mcare','S300001','01200','0400','numeric'))




colnames(hcris.vars)=c("variable","WKSHT_CD","LINE_NUM","CLMN_NUM","source")


# Import data -------------------------------------------------------------

hcris_list <- list()
for (i in 2006:2011) {
  dir_i <- file.path(hcris_raw_dir, paste0("HOSPFY", i))
  f_alpha <- find_raw(dir_i, paste0("HOSP_", i, "_ALPHA")); f_nmrc <- find_raw(dir_i, paste0("HOSP_", i, "_NMRC"))
  f_rpt   <- find_raw(dir_i, paste0("HOSP_", i, "_RPT"))
  if (anyNA(c(f_alpha, f_nmrc, f_rpt))) { message("Skipping ", i, ": raw files not found in ", dir_i); next }
  HCRIS.alpha=read_csv(f_alpha,
                       col_names=c('RPT_REC_NUM','WKSHT_CD','LINE_NUM','CLMN_NUM','ITM_VAL_NUM'))
  HCRIS.numeric=read_csv(f_nmrc,
                         col_names=c('RPT_REC_NUM','WKSHT_CD','LINE_NUM','CLMN_NUM','ITM_VAL_NUM'))
  HCRIS.report=read_csv(f_rpt,
                        col_names=c('RPT_REC_NUM','PRVDR_CTRL_TYPE_CD','PRVDR_NUM','NPI',
                                    'RPT_STUS_CD','FY_BGN_DT','FY_END_DT','PROC_DT',
                                    'INITL_RPT_SW','LAST_RPT_SW','TRNSMTL_NUM','FI_NUM',
                                    'ADR_VNDR_CD','FI_CREAT_DT','UTIL_CD','NPR_DT',
                                    'SPEC_IND','FI_RCPT_DT'))
  final.reports = HCRIS.report %>%
    select(report=RPT_REC_NUM, provider_number=PRVDR_NUM, npi=NPI, 
           fy_start=FY_BGN_DT, fy_end=FY_END_DT, date_processed=PROC_DT, 
           date_created=FI_CREAT_DT, status=RPT_STUS_CD) %>%
    mutate(year=i)
  
  for (v in 1:nrow(hcris.vars)) {
    hcris.data=get(paste("HCRIS.",hcris.vars[v,5],sep=""))
    var.name=quo_name(hcris.vars[v,1])    
    val = hcris.data %>%
      filter(WKSHT_CD==hcris.vars[v,2], LINE_NUM==hcris.vars[v,3], CLMN_NUM==hcris.vars[v,4]) %>%
      select(report=RPT_REC_NUM, !!var.name:=ITM_VAL_NUM) 
    assign(paste("val.",v,sep=""),val)
    final.reports=left_join(final.reports, 
                            get(paste("val.",v,sep="")),
                            by="report")
  }
  assign(paste("final.reports.",i,sep=""),final.reports)
  hcris_list[[as.character(i)]] <- final.reports
  
}

if (!length(hcris_list)) stop("No raw HCRIS v1996 years were read - nothing written. Check the folder printed at the top.")
final.hcris.v1996 <- bind_rows(hcris_list)
cat("\nv1996: years read", paste(names(hcris_list), collapse = ", "), "\n")
final.hcris.v1996 <- final.hcris.v1996 %>%
  mutate(HIT_purch = NA,
         HIT_assets = NA) %>%
  group_by(provider_number, year) %>%
  mutate(capital_cost_buildings = sum(capital_cost_buildings_old,capital_cost_buildings_new, na.rm=T)) %>%
  mutate(capital_cost_movableequip = sum(capital_cost_movableequip_old,capital_cost_movableequip_new, na.rm=T)) %>%
  select(-capital_cost_buildings_old, -capital_cost_buildings_new, -capital_cost_movableequip_old, -capital_cost_movableequip_new)


write_csv(final.hcris.v1996,paste0(raw_data_path, '/final_HCRIS_v1996.csv'),append=FALSE,col_names=TRUE)