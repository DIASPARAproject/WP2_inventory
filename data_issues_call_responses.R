# define libraries needed
libs <- c("tidyverse") 

#define libraries already installed
installed_libs <- libs %in% rownames(installed.packages())

# install libraries that are not installed already
if (any(installed_libs == F)) {
  install.packages(libs[!installed_libs])
}

### load libraries needed
invisible(lapply(libs, library, character.only = T))




# 1. Load all relevant data ####

# List all CSV files in folder "x"
file_list <- list.files(path = "C:/Users/pohlmann/Desktop/Validation", pattern = "\\.csv$", full.names = TRUE)

# Step 1: Read all files
dfs <- lapply(file_list, function(f) read.csv2(f, stringsAsFactors = FALSE))

# Step 2: Identify all columns that are character in any file
char_cols <- unique(unlist(lapply(dfs, function(df) {
  names(df)[sapply(df, is.character)]
})))

# Step 3: Coerce those columns to character in all data frames
dfs_aligned <- lapply(dfs, function(df) {
  for (col in char_cols) {
    if (col %in% names(df)) {
      df[[col]] <- as.character(df[[col]])
    }
  }
  df
})

# Step 4: Combine
combined_df <- bind_rows(dfs_aligned)

count_c <- sum(grepl("c", combined_df$action, ignore.case = TRUE))
count_v <- sum(grepl("v", combined_df$action, ignore.case = TRUE))
