library(pdftools)
library(tm)
library(tidytext)
library(dplyr)
library(stringr)

# For each subfolder in the 2016 folder, read in the pdfs and extract the text
# Create a list of all folder names in the "2016" folder
folders_2016 <- list.files(paste0(data_path, "/2016"))

# Create list with same length as files_2016, but with each element being a list
# that has the same length as the number of files in each subfolder
hospital_names <- vector(mode = "list", length = length(folders_2016))
for (x in seq_along(folders_2016)) {
  files <- list.files(paste0(data_path, "/2016/", folders_2016[[x]]),
                      pattern = "pdf$", full.names = FALSE)
  hospital_files <- vector(mode = "list", length = length(files))
  for (y in seq_along(files)) {
    pg1 <- pdf_ocr_text(paste0(data_path, "/2016/",
                               folders_2016[[x]], "/", files[y]),
                        pages = 1,
                        dpi = 600,
                        opw = "",
                        upw = "",
                        language = "eng")
    pg1 <- str_split(pg1, "\n")
    pg1 <- pg1[[1]]
    pg1 <- tibble(line = 1:length(pg1), text = pg1) # nolint
    pg1 <- pg1 %>%
      tidytext::unnest_tokens(word, text) %>%
      mutate(hospital = ifelse(word == "hospital", 1, 0))
    hospital <- sum(pg1$hospital)
    hospital_files[[y]] <- ifelse(hospital == 1, files[y], NA)
  }
  hospital_files <- hospital_files[!is.na(hospital_files)]
  hospital_names[[x]] <- hospital_files
}


library(pdftools)
library(dplyr)
library(stringr)

# Create a function to process each PDF file
process_pdf_pg1 <- function(file_path) {
  pg1 <- pdf_ocr_text(file_path, pages = 1,
                      dpi = 600, opw = "", upw = "", language = "eng")
  pg1 <- str_split(pg1, "\n")[[1]]
  word_counts <- sum(str_detect(pg1, fixed("hospital|medical|health",
                                           ignore_case = TRUE)))
  if (word_counts > 0) {
    return(file_path)
  } else {
    return(NULL)
  }
}

# Get a list of all PDF files in subfolders
pdf_files <- list.files(paste0(data_path, "/2016"), pattern = "pdf$",
                        full.names = TRUE, recursive = TRUE)

# Process each PDF file and filter out those containing "hospital"
hospital_pdf_files <- lapply(pdf_files, process_pdf_pg1)
hospital_pdf_files <- unlist(hospital_pdf_files, use.names = FALSE)
hospital_pdf_files <- hospital_pdf_files[!is.na(hospital_pdf_files)]

# Print the resulting vector of file names
print(hospital_pdf_files)