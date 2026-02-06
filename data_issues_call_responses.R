# IF ANOTHER CALL IS ISSUED; THIS SCRIPT NEEDS TO BE EDITED TO ACCOUNT FOR FISH THAT WERE ALREADY VALIDATED IN A PREVIOUS CALL ###



# define libraries needed
libs <- c("tidyverse", "readxl", "stringr", "purrr") 

#define libraries already installed
installed_libs <- libs %in% rownames(installed.packages())

# install libraries that are not installed already
if (any(installed_libs == F)) {
  install.packages(libs[!installed_libs])
}

### load libraries needed
invisible(lapply(libs, library, character.only = T))




# 1. Load all relevant data ####

#load RData with all IDs tagged for review
load("data/issues/data_issues_call/all_odd.RData")

# Define the path from project root
folder_path <- "data/issues/responses"

# List files
file_list <- list.files(path = folder_path, 
                        pattern = "\\.(csv|xlsx)$", 
                        full.names = TRUE)

df_list <- basename(file_list) %>% 
  str_replace("\\.", "_")


#load xlsx files and reduce to relevant cols
xlsx_files <- file_list[grepl("\\.xlsx$", file_list)]

for (f in xlsx_files) {
  obj_name <- str_replace(basename(f), "\\.", "_")
  assign(obj_name, read_excel(f) %>% 
           select(mei_fi_id, ser_cou_code, issue, action) %>% 
           mutate(across(everything(), as.character)))
}

# load csvs (semicolon separated) and reduce to relevant cols
csv_files <- file_list[grepl("\\.csv$", file_list)]

for (f in csv_files) {
  first_line <- readLines(f, n = 1)
  if (grepl(";", first_line)) {
    obj_name <- str_replace(basename(f), "\\.", "_")
    assign(obj_name, read.csv2(f, colClasses = "character") %>% 
             select(mei_fi_id, ser_cou_code, issue, action))
  }
}

# load csvs (comma separated) and reduce to relevant cols
for (f in csv_files) {
  first_line <- readLines(f, n = 1)
  if (!grepl(";", first_line)) {
    obj_name <- str_replace(basename(f), "\\.", "_")
    assign(obj_name, read.csv(f, colClasses = "character") %>% 
             select(mei_fi_id, ser_cou_code, issue, action))
  }
}



# 2. Collate and remove duplicates ###

# combine in one dataframe
combined_df <- bind_rows(mget(df_list)) %>% 
  mutate(action = na_if(action, "NA"),
         action = na_if(action, ""))

# Find duplicated IDs with conflicting actions
conflicts <- combined_df %>%
  group_by(mei_fi_id) %>%
  filter(n() > 1) %>%                      # Keep only duplicates
  filter(n_distinct(action) > 1) %>%       # Keep only if actions differ
  ungroup()

print(conflicts)

# remove duplicates (since some were provided as xlsx and csv, before was checked if they differ in the action (they do not)
combined_df_clean <- combined_df %>% 
  distinct(mei_fi_id, .keep_all = TRUE)

#check unique entries for action
unique(combined_df_clean$action)

#filter only corrected and check
df_corrected <- combined_df_clean %>% 
  filter(!is.na(action)) 

unique(df_corrected$action)




# 3. compare to fish initially tagged as odd and create lists with ids of fish that still have issues

#create df with all uncorrected data
df_uncorrected <- all_odd %>% 
  select(mei_fi_id, ser_cou_code, issue, action) %>%
  mutate(mei_fi_id = as.character(mei_fi_id)) %>% 
  anti_join(df_corrected, by = "mei_fi_id") %>%
  distinct(mei_fi_id, .keep_all = TRUE)

#create a summary of df_uncorrected (long/cross)
summary_df_uncorrected_long <- df_uncorrected %>% 
  count(issue, ser_cou_code)
  
summary_df_uncorrected_cross <- summary_df_uncorrected_long %>% 
  pivot_wider(
    names_from = ser_cou_code, 
    values_from = n,
    values_fill = 0  # Replaces NAs with 0 where a combination doesn't exist
  )

#create a summary of df_corrected (long/cross)
summary_df_corrected_long <- df_corrected %>% 
  count(issue, ser_cou_code)

summary_df_corrected_cross <- summary_df_corrected_long %>% 
  pivot_wider(
    names_from = ser_cou_code, 
    values_from = n,
    values_fill = 0  # Replaces NAs with 0 where a combination doesn't exist
  )

#extract all potential issues & print
all_issues <- all_odd %>%
  pull(issue) %>%
  str_split(",\\s*") %>%
  flatten_chr() %>%
  unique() %>%
  discard(~ .x == "")

all_issues

# create a vector with issues considered highly relevant to the analyses
relevant <- c("weight_length_off|ser_y_out_of_range|too_short|unrealistic_length|too_long")


#create df with only those fish that initially had an issue relevant to the analyses & check (missing coordinates excluded since filtered out for modeling anyway)
all_odd_relevant <- all_odd %>%
  filter(str_detect(issue, relevant))

unique(all_odd_relevant$issue)


#create df with only those fish that, after calling for correction/validation, still had an issue relevant to the analyses & check (missing coordinates excluded since filtered out for modeling anyway)
df_uncorrected_relevant <- df_uncorrected %>%
  filter(str_detect(issue, relevant))

unique(df_uncorrected_relevant$issue)

# 4. save relevant output
save(all_odd, all_odd_relevant, all_odd_coordinates, df_uncorrected, df_uncorrected_relevant, df_corrected, summary_df_uncorrected_long, summary_df_uncorrected_cross, 
     summary_df_corrected_long, summary_df_corrected_cross,
     file = "data/issues/summary_corrections.RData")


