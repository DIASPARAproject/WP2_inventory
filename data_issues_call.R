# define libraries needed
libs <- c("tidyverse", "icesTAF") 

#define libraries already installed
installed_libs <- libs %in% rownames(installed.packages())

# install libraries that are not installed already
if (any(installed_libs == F)) {
  install.packages(libs[!installed_libs])
}

### load libraries needed
invisible(lapply(libs, library, character.only = T))

### create directories
mkdir("data/issues/data_issues_call/comma")
mkdir("data/issues/data_issues_call/semicolon")


# 1. Load all relevant data ####

#collect all files with odd data
files <- list.files("./data/issues/odd_data", pattern = "\\.RData$", full.names = TRUE)

# Function to load a single .Rdata and extract the object inside
load_rdata_as_df <- function(file) {
  env <- new.env()
  load(file, envir = env)
  get(ls(env)[1], envir = env)  # Assumes one object per file
}

#Load and combine all dataframes (collapsing duplicates in one row is a rowwise operation and takes long. data.table faster but to avoid issues when converting I stuck with this)
all_odd <- files %>%
  lapply(load_rdata_as_df) %>%
  bind_rows() %>% 
  select(-issue) %>%
  group_by(mei_fi_id) %>%
  summarize(across(everything(), ~ na.omit(.x)[1]), .groups = "drop")

#create a vector of all logical columns
logi_cols <- names(Filter(is.logical, all_odd))

#add issues column  
all_odd <- all_odd %>% 
  filter(if_any(all_of(logi_cols), ~ .)) %>%
  mutate(issue = apply(select(., all_of(logi_cols)), 1, function(x) {
    paste(names(x)[which(x)], collapse = ", ")
  }))

# reduce dataframe to relevant columns
all_odd <- all_odd %>% 
  select(mei_fi_id, fi_id_cou, fi_year, fi_date, ser_x, ser_y, ser_cou_code, ser_nameshort, ser_emu_nameshort, ser_hty_code, lengthmm, weightg, ageyear, female_proportion, method_sex, sex, sex_SI, sex_source, fi_lfs_code, lfs_SI, ser_lfs_code, lfs_code, lfs_code_source, sai_samplingstrategy, sai_samplingobjective, fi_comment, sai_comment, ser_qal_id, source, issue) %>% 
  mutate(action = "")

#save all_odd
save(all_odd, file = "data/issues/data_issues_call/all_odd.RData")

# Split into list of dataframes by ser_cou_code
split_dfs <- split(all_odd, all_odd$ser_cou_code)

# Save each dataframe to a CSV (comma separated)
invisible(lapply(names(split_dfs), function(code) {
  write.table(
    split_dfs[[code]],
    file = file.path("data/issues/data_issues_call/comma", paste0("validate_", code, ".csv")),
    sep = ",",
    row.names = FALSE,
    col.names = TRUE,
    quote = TRUE
  )
}))

# Save each dataframe to a CSV (semicolon separated)
invisible(lapply(names(split_dfs), function(code) {
  write.table(
    split_dfs[[code]],
    file = file.path("data/issues/data_issues_call/semicolon", paste0("validate_", code, ".csv")),
    sep = ";",
    row.names = FALSE,
    col.names = TRUE,
    quote = TRUE
  )
}))
