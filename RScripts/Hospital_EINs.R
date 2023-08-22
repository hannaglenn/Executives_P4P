library(httr)
library(jsonlite)
library(dplyr)
library(stringr)

# This script uses the Nonprofit Explorer API from ProPublica
# (documentation here: https://projects.propublica.org/nonprofits/api)
# to create a list of all hospital EIN numbers, which I will use to locate 
# the pdfs for each tax year filing for each hospital. 

###### ORDER : 1 ######

# get EINs for nonprofits categorized as hospitals (NTEE codes E20, E21, E22, E24)
# have to do this by looping on every state b/c of pagniation in API
# CA is the only state that still needs further parsing, leave this one out and add at the end

source("paths.R")

# create empty list to store data
state_hospital_ein_data <- vector(mode='list', length=49)
state_abbreviations <- c("AL", "AK", "AZ", "AR", "CO", "CT", "DE", "FL", "GA", 
                         "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", 
                         "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", 
                         "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", 
                         "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")

# loop through each state 
for (x in seq_along(state_abbreviations)){
  url_base <- paste0('https://projects.propublica.org/nonprofits/api/v2/search.json?ntee%5Bid%5D=4&state%5Bid%5D=',state_abbreviations[[x]])
  total_orgs <- GET(url_base)
  total_orgs <- fromJSON(rawToChar(total_orgs$content), flatten = TRUE)
  num_pages <- ceiling(as.numeric(total_orgs$total_results)/100)
  
  hospital_ein <- vector(mode='list', length=num_pages)
  
  # loop through each page of results for each state
  for (i in 0:(num_pages-1)){
    url <- paste0(url_base,'&page=',i)
    data <- GET(url)
    data <- fromJSON(rawToChar(data$content), flatten=TRUE)
    org_data <- data$organizations %>%
      select(ein, ntee_code) %>%
      filter(str_detect(.$ntee_code, "E20|E21|E22"))
    hospital_ein[[i+1]] <- org_data
  }
  
  # store data
  hospital_ein_data <- do.call(rbind, hospital_ein)
  
  state_hospital_ein_data[[x]] <- hospital_ein_data
}

# combine all data
all_hospital_ein_data <- do.call(rbind, state_hospital_ein_data)

# Subset CA by keyword search (including hospital as keyword and excluding hospital as keyword)
CA_ein_data <- vector(mode='list', length=2)
CA_subset <- c("q=-hospital", "q=hospital")

# loop over each subset of CA data
for (x in seq_along(CA_subset)){
  url_base <- paste0('https://projects.propublica.org/nonprofits/api/v2/search.json?ntee%5Bid%5D=4&state%5Bid%5D=CA&',CA_subset[[x]])
  total_orgs <- GET(url_base)
  total_orgs <- fromJSON(rawToChar(total_orgs$content), flatten = TRUE)
  num_pages <- ceiling(as.numeric(total_orgs$total_results)/100)
  
  CA_ein <- vector(mode='list', length=num_pages)
  
  for (i in 0:(num_pages-1)){
    url <- paste0(url_base,'&page=',i)
    data <- GET(url)
    data <- fromJSON(rawToChar(data$content), flatten=TRUE)
    org_data <- data$organizations %>%
      select(ein, ntee_code) %>%
      filter(str_detect(.$ntee_code, "E20|E21|E22"))
    CA_ein[[i+1]] <- org_data
  }
  
  CA_hospital_ein <- do.call(rbind, CA_ein)
  
  CA_ein_data[[x]] <- CA_hospital_ein
}

CA_hospital_ein_data <- do.call(rbind, CA_ein_data)

# combine CA data with other states
all_hospital_ein_data <- rbind(all_hospital_ein_data, CA_hospital_ein_data)

# create list out of data
ein_list <- all_hospital_ein_data$ein

#save ein_list
saveRDS(ein_list, file=paste0(created_data_path,'/ein_list.rds'))
