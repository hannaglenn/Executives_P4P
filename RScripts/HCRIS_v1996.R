library(dplyr)
library(readr)

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




colnames(hcris.vars)=c("variable","WKSHT_CD","LINE_NUM","CLMN_NUM","source")


# Import data -------------------------------------------------------------

for (i in 2006:2011) {
  HCRIS.alpha=read_csv(paste0(raw_data_path, "/HCRIS_v1996/HOSPFY",i,"/HOSP_",i,"_ALPHA.csv"),
                       col_names=c('RPT_REC_NUM','WKSHT_CD','LINE_NUM','CLMN_NUM','ITM_VAL_NUM'))
  HCRIS.numeric=read_csv(paste0(raw_data_path, "/HCRIS_v1996/HOSPFY",i,"/HOSP_",i,"_NMRC.csv"),
                         col_names=c('RPT_REC_NUM','WKSHT_CD','LINE_NUM','CLMN_NUM','ITM_VAL_NUM'))
  HCRIS.report=read_csv(paste0(raw_data_path, "/HCRIS_v1996/HOSPFY",i,"/HOSP_",i,"_RPT.csv"),
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
  if (i==2006) {
    final.hcris.v1996=final.reports.2006
  } else {
    final.hcris.v1996=rbind(final.hcris.v1996,get(paste0("final.reports.",i)))
  }
  
}

final.hcris.v1996 <- final.hcris.v1996 %>%
  mutate(HIT_purch = NA,
         HIT_assets = NA) %>%
  group_by(provider_number, year) %>%
  mutate(capital_cost_buildings = sum(capital_cost_buildings_old,capital_cost_buildings_new, na.rm=T)) %>%
  mutate(capital_cost_movableequip = sum(capital_cost_movableequip_old,capital_cost_movableequip_new, na.rm=T)) %>%
  select(-capital_cost_buildings_old, -capital_cost_buildings_new, -capital_cost_movableequip_old, -capital_cost_movableequip_new)


write_csv(final.hcris.v1996,paste0(raw_data_path, '/final_HCRIS_v1996.csv'),append=FALSE,col_names=TRUE)
