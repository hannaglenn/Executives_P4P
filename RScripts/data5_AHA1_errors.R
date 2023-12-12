library(readr)
library(dplyr)

source("paths.R")

##### ORDER : 5 ##########

# This script will record any EINs that only pulled a 990-T form for a given year (which does not have names in it)
# I manually download any pdfs that are missing and put them in the folder "manual pdfs"

# read in cleaned data set of names
AHA_ein_list <- readRDS(paste0(created_data_path,"AHA_ein_list.rds"))


# convert errors in list into data frame
errors_data <- data.frame(ein=as.character(), year=as.character(), text_pulled=as.character())

# Any text that returned null means tax form 990-T was pulled.
# If an EIN only has a 990 for that year, I need to get the 990 form manually
# Create a list of Ein, years that need a url manually inputted

for (i in 1:length(AHA_ein_list)){
  ein <- AHA_ein_list[[i]][["ein"]]
  
  # loop through all elements of text data
  for (x in seq_along(AHA_ein_list[[i]][["text_data"]])) {
    year <- AHA_ein_list[[i]][["text_data"]][[x]][["year"]]
    data <- data.frame(ein=as.character(), year=as.character(), text_pulled=as.character())
    
    if (length(AHA_ein_list[[i]][["text_data"]][[x]][["text"]])>0) {
      data <- data.frame(ein=ein, year=year, text_pulled=1)
    }
    if (length(AHA_ein_list[[i]][["text_data"]][[x]][["text"]])==0 & length(AHA_ein_list[[i]][["text_data"]][[x]][["year"]]!=0)) {
      data[nrow(data)+1,] <- c(ein, year, 0)
    }
    
    errors_data <- errors_data %>%
      rbind(data)
  }
}

# Find all ein-years that do not have any text pulled
errors_data <- errors_data %>%
  mutate(text_pulled=as.numeric(text_pulled)) %>%
  group_by(ein,year) %>%
  mutate(sum=sum(text_pulled)) %>%
  ungroup() %>%
  distinct(ein, year, sum) %>%
  filter(sum==0)

write.csv(errors_data, file=paste0(created_data_path, "AHA_errors_data6.csv"))


# list elements completed 
# 1-180 done
# 181-359 done
# 360-570 done
# 571-673 done
# 674-842 done
# 842 until the end done! 


