library(readr)
library(dplyr)

source("paths.R")

# read in cleaned data set of names
all_ein_list <- readRDS(paste0(created_data_path,"all_ein_list_2016_2020.rds"))


# convert errors in list into data frame
errors_data <- data.frame(ein=as.character(), year=as.character(), text = as.character(), dataframe=as.character())


for (i in seq_along(all_ein_list)){
  ein <- all_ein_list[[i]][["ein"]]
  
  for (x in seq_along(all_ein_list[[i]][["ein_data"]])) {
    data <- as.data.frame(all_ein_list[[i]][["ein_data"]][[x]]) %>%
      mutate(ein=ein)
    
    n <- nrow(data)
    
    if (n>1) {data <- data %>% mutate(text=0)}
    
    dataframe = ifelse(n==1,0,1)
    
    data <- data %>%
      mutate(dataframe=dataframe) %>%
      select(ein, year, text, dataframe)

    errors_data <- rbind(errors_data, data)
  }
}

errors_data <- errors_data %>%
  group_by(ein, year) %>%
  mutate(keep=sum(dataframe, na.rm=TRUE)) %>%
  ungroup() %>%
  filter(keep==0)

errors_data <- errors_data %>%
  distinct(ein, year)

write.csv(errors_data, file=paste0(created_data_path, "errors_data_2016_2020.csv"))
