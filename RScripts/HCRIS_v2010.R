library(dplyr)
library(readr)

#### Thank you to my advisor, Ian McCarthy, for writing this code and making it publicly available. If interested, you can find this resource
#### at https://ianmccarthyecon.netlify.app/resources/


# Variable locations ------------------------------------------------------

hcris.vars = NULL
hcris.vars = rbind(hcris.vars,c('beds','S300001','01400','00200','numeric'))
hcris.vars = rbind(hcris.vars,c('tot_charges','G300000','00100','00100','numeric'))
hcris.vars = rbind(hcris.vars,c('tot_discounts','G300000','00200','00100','numeric'))
hcris.vars = rbind(hcris.vars,c('tot_operating_exp','G300000','00400','00100','numeric'))
hcris.vars = rbind(hcris.vars,c('ip_charges','G200000','00100','00100','numeric'))
hcris.vars = rbind(hcris.vars,c('icu_charges','G200000','01600','00100','numeric'))
hcris.vars = rbind(hcris.vars,c('ancillary_charges','G200000','01800','00100','numeric'))
hcris.vars = rbind(hcris.vars,c('tot_discharges','S300001','00100','01500','numeric'))
hcris.vars = rbind(hcris.vars,c('mcare_discharges','S300001','00100','01300','numeric'))
hcris.vars = rbind(hcris.vars,c('mcaid_discharges','S300001','00100','01400','numeric'))
hcris.vars = rbind(hcris.vars,c('tot_mcare_payment','E00A18A','05900','00100','numeric'))
hcris.vars = rbind(hcris.vars,c('secondary_mcare_payment','E00A18A','06000','00100','numeric'))
hcris.vars = rbind(hcris.vars,c('street','S200001','00100','00100','alpha'))
hcris.vars = rbind(hcris.vars,c('city','S200001','00200','00100','alpha'))
hcris.vars = rbind(hcris.vars,c('state','S200001','00200','00200','alpha'))
hcris.vars = rbind(hcris.vars,c('zip','S200001','00200','00300','alpha'))
hcris.vars = rbind(hcris.vars,c('county','S200001','00200','00400','alpha'))
hcris.vars = rbind(hcris.vars,c('hvbp_payment','E00A18A','07093','00100','numeric'))
hcris.vars = rbind(hcris.vars,c('hrrp_payment','E00A18A','07094','00100','numeric'))
hcris.vars = rbind(hcris.vars,c('tot_uncomp_care_charges','S100000','02000','00300','numeric'))
hcris.vars = rbind(hcris.vars,c('tot_uncomp_care_partial_pmts','S100000','02200','00300','numeric'))
hcris.vars = rbind(hcris.vars,c('bad_debt','S100000','02800','00100','numeric'))
hcris.vars = rbind(hcris.vars,c('cost_to_charge','S100000','00100','00100','numeric'))
hcris.vars = rbind(hcris.vars,c('new_cap_ass','A700001','01000','00200','numeric'))
hcris.vars = rbind(hcris.vars,c('cash','G000000','00100','00100','numeric'))
hcris.vars = rbind(hcris.vars,c('tot_pat_rev','G300000','00100','00100','numeric'))
hcris.vars = rbind(hcris.vars,c('allowance','G300000','00200','00100','numeric'))
hcris.vars = rbind(hcris.vars,c('net_pat_rev','G300000','00300','00100','numeric'))

# variables added by me
hcris.vars = rbind(hcris.vars,c('land_purch','A700001','00100','00200','numeric'))
hcris.vars = rbind(hcris.vars,c('land_assets','A700001','00100','00700','numeric'))
hcris.vars = rbind(hcris.vars,c('landimpr_purch','A700001','00200','00200','numeric'))
hcris.vars = rbind(hcris.vars,c('landimpr_assets','A700001','00200','00700','numeric'))
hcris.vars = rbind(hcris.vars,c('build_purch','A700001','00300','00200','numeric'))
hcris.vars = rbind(hcris.vars,c('build_assets','A700001','00300','00700','numeric'))
hcris.vars = rbind(hcris.vars,c('buildimpr_purch','A700001','00400','00200','numeric'))
hcris.vars = rbind(hcris.vars,c('buildimpr_assets','A700001','00400','00700','numeric'))
hcris.vars = rbind(hcris.vars,c('fixedequipment_purch','A700001','00500','00200','numeric'))
hcris.vars = rbind(hcris.vars,c('fixedequipment_assets','A700001','00500','00700','numeric'))
hcris.vars = rbind(hcris.vars,c('movableequipment_purch','A700001','00600','00200','numeric'))
hcris.vars = rbind(hcris.vars,c('moveableequipment_assets','A700001','00600','00700','numeric'))
hcris.vars = rbind(hcris.vars,c('HIT_purch','A700001','00700','00200','numeric'))
hcris.vars = rbind(hcris.vars,c('HIT_assets','A700001','00700','00700','numeric'))
hcris.vars = rbind(hcris.vars,c('total_purch','A700001','01000','00200','numeric'))
hcris.vars = rbind(hcris.vars,c('total_assets','A700001','01000','00700','numeric'))
hcris.vars = rbind(hcris.vars,c('labor_costs','S300003','00600','00200','numeric'))
hcris.vars = rbind(hcris.vars,c('capital_cost_buildings','A000000','00100','00300','numeric'))
hcris.vars = rbind(hcris.vars,c('capital_cost_movableequip','A000000','00200','00300','numeric'))
hcris.vars = rbind(hcris.vars,c('operating_expenses','A000000','20000','00300','numeric'))
hcris.vars = rbind(hcris.vars,c('medrecords_expenses','A000000','01600','00300','numeric'))




colnames(hcris.vars)=c("variable","WKSHT_CD","LINE_NUM","CLMN_NUM","source")



# Import data -------------------------------------------------------------
for (i in 2010:2016) {
  HCRIS.alpha=read_csv(paste0(raw_data_path, "/HCRIS_v2010/HOSP10FY",i,"/HOSP10_",i,"_ALPHA.csv"),
                       col_names=c('RPT_REC_NUM','WKSHT_CD','LINE_NUM','CLMN_NUM','ITM_VAL_NUM'))
  HCRIS.numeric=read_csv(paste0(raw_data_path, "/HCRIS_v2010/HOSP10FY",i,"/HOSP10_",i,"_NMRC.csv"),
                         col_names=c('RPT_REC_NUM','WKSHT_CD','LINE_NUM','CLMN_NUM','ITM_VAL_NUM'))
  HCRIS.report=read_csv(paste0(raw_data_path, "/HCRIS_v2010/HOSP10FY",i,"/HOSP10_",i,"_RPT.csv"),
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
  if (i==2010) {
    final.hcris.v2010=final.reports.2010
  } else {
    final.hcris.v2010=rbind(final.hcris.v2010,get(paste0("final.reports.",i)))
  }
}
write_csv(final.hcris.v2010,paste0(raw_data_path, '/final_HCRIS_v2010.csv'),append=FALSE,col_names=TRUE)
