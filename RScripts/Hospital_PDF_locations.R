library(readr)
library(httr)
library(jsonlite)
library(dplyr)
library(stringr)
library(tidyr)

# This script uses the list of hospital EINs created in "Hospital_EINs.R" 
# to dowload data on each hospital from the API, including name, state, zip code,
# and pdf urls for each tax year.

##### ORDER : 2 #####

# Run the entire "paths.R" script
source("paths.R")

# Read in list of hospital EIN numbers
ein_list <- readRDS(paste0(created_data_path, "/ein_list.rds"))

# use the API to create a data set with information on each EIN
count=0 #keep track of how many hospitals have pdfs available
hospital_data_list <- vector(mode='list', length=length(ein_list))
for (x in seq_along(ein_list)) {
  tryCatch({
    url <- paste0('https://projects.propublica.org/nonprofits/api/v2/organizations/',ein_list[i],'.json')
    data <- GET(url)
    data <- fromJSON(rawToChar(data$content), flatten=TRUE)
    org <- as.data.frame(list(data$organization$ein, 
                                data$organization$name, 
                                data$organization$state, 
                                data$organization$zipcode))
    colnames(org) <- c("ein", "name", "state", "zipcode")
    without_data <- data$filings_without_data %>%
        select(tax_prd_yr, pdf_url, formtype)
    with_data <- data$filings_with_data %>%
        select(tax_prd_yr, pdf_url, formtype)
    pdf_locations <- rbind(without_data, with_data)
    cross <- crossing(org, pdf_locations)
    hospital_data_list[[x]] <- cross
    count=count+1
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}
# get rid of null elements of hospital_data_list
hospital_data_list <- hospital_data_list[!sapply(hospital_data_list, is.null)]

# combine the list into one data frame 
hospital_pdf_locations <- do.call(rbind, hospital_data_list)

hospital_pdf_locations <- hospital_pdf_locations %>%
  filter(!is.na(pdf_url)) %>%
  filter(tax_prd_yr>=2006 & tax_prd_yr<=2020)

# create variable that captures multiple forms in a given year
hospital_pdf_locations <- hospital_pdf_locations %>%
  group_by(ein, tax_prd_yr) %>%
  mutate(num=row_number())

# create a data set of eins that have multiple pdfs in the same year
multiple_pdfs <- hospital_pdf_locations %>%
  mutate(count=1) %>%
  group_by(ein, tax_prd_yr) %>%
  mutate(sum=sum(count)) %>%
  filter(sum>1) %>%
  ungroup()
# almost a third of observations are repeats in the same year
# it is difficult to detect which forms contain the info needed, will have to keep all of them


# save the data frame
saveRDS(hospital_pdf_locations, paste0(created_data_path,"/hospital_pdf_locations.rds"))



