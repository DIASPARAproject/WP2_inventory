
# 1. Initial preparations #####################################################################################################################

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

# delete everything from the R environment
rm(list = ls())

#create directory 
mkdir("data")
mkdir("data/LHTsite")
mkdir("data/LHTemu")
mkdir("data/issues/removals")
mkdir("data/issues/odd_data")
mkdir("data/issues/data_issues_call")
mkdir("data/issues/plots")
mkdir("data/issues/plots/patterns")
mkdir("data/issues/tables")



#load data 
load("boot/data/compilation/individual_wide.RData")
load("boot/data/compilation_base/individual_wide_base.RData")




# 2. INITIAL DATA FORMATTING ###########################################################################################################################

# 2.1 Add columns with sex and life stage ####

#the ifelse excludes fish where method sex is not provided for collection of sex from female_proportion
#the ifelse also excludes fish where method_sex is length since we can do that ourselves. However, some Y and < 45cm are given a sex by length. I do not trust that... 

# 2.1.1 base (original data before corrections)
individual_all_base <- individual_wide_base %>%
  rename(method_sex = "method_sex_(1=visual,0=use_length)") %>% 
  mutate(SI = names(select(., which(colnames(individual_wide_base) == "S1"):which(colnames(individual_wide_base) == "S6")))[max.col(select(., which(colnames(individual_wide_base) == "S1"):which(colnames(individual_wide_base) == "S6")))], #any way to do this by colname to avoid stupid hidden errors?
         lfs_SI = ifelse(SI == "S1" | SI == "S2" | SI == "S3", "Y",
                         ifelse(SI == "S4" | SI == "S5" | SI == "S6", "S", NA)),
         fi_lfs_code = ifelse(fi_lfs_code == "NA", NA, fi_lfs_code),
         lfs_code = ifelse(!is.na(fi_lfs_code), fi_lfs_code, lfs_SI),
         lfs_code_source = ifelse(!is.na(fi_lfs_code), "fi_lfs_code", 
                                  ifelse(!is.na(lfs_SI), "lfs_SI", NA)),
         sex_provided_visual = ifelse(female_proportion == 1 & method_sex == 1, "f",
                                      ifelse(female_proportion == 0 & method_sex == 1, "m", NA)),
         sex_provided_length = ifelse(female_proportion == 1 & method_sex == 0, "f",
                                      ifelse(female_proportion == 0 & method_sex == 0, "m", NA)),
         sex_provided_nomethod = ifelse(female_proportion == 1 & is.na(method_sex), "f",
                                      ifelse(female_proportion == 0 & is.na(method_sex), "m", NA)),
         sex_SI = ifelse(SI == "S6", "m",
                         ifelse(SI == "S5" | SI == "S4", "f", NA)),
         sex_length = ifelse(lfs_code == "S" & lengthmm < 451, "m",
                             ifelse(lengthmm > 450, "f", NA)), 
         sex = case_when(!is.na(sex_provided_visual) ~ sex_provided_visual,
                         !is.na(sex_provided_nomethod) ~ sex_provided_nomethod,
                         !is.na(sex_provided_length) ~ sex_provided_length,
                         !is.na(sex_SI) ~ sex_SI,
                         !is.na(sex_length) ~ sex_length,
                         TRUE ~ NA),
         sex_source = case_when(!is.na(sex_provided_visual) ~ "sex_provided_visual",
                                !is.na(sex_provided_nomethod) ~ "sex_provided_nomethod",
                                !is.na(sex_provided_length) ~ "sex_provided_length",
                                !is.na(sex_SI) ~ "sex_SI",
                                !is.na(sex_length) ~ "sex_length",
                                TRUE ~ NA)) %>% 
  select(-S1, -S2, -S3, -S4, -S5, -S6) %>% 
  relocate(sex_provided_visual, sex_provided_nomethod, sex_provided_length, sex_SI, sex_length, .after = sex) %>% 
  relocate(fi_lfs_code, ser_lfs_code, lfs_SI, .after = sex)


# 2.1.2 recent (most recent data)
individual_all <- individual_wide %>%
  rename(method_sex = "method_sex_(1=visual,0=use_length)") %>% 
  mutate(SI = names(select(., which(colnames(individual_wide) == "S1"):which(colnames(individual_wide) == "S6")))[max.col(select(., which(colnames(individual_wide) == "S1"):which(colnames(individual_wide) == "S6")))], #any way to do this by colname to avoid stupid hidden errors?
         lfs_SI = ifelse(SI == "S1" | SI == "S2" | SI == "S3", "Y",
                         ifelse(SI == "S4" | SI == "S5" | SI == "S6", "S", NA)),
         fi_lfs_code = ifelse(fi_lfs_code == "NA", NA, fi_lfs_code),
         lfs_code = ifelse(!is.na(fi_lfs_code), fi_lfs_code, lfs_SI),
         lfs_code_source = ifelse(!is.na(fi_lfs_code), "fi_lfs_code", 
                                  ifelse(!is.na(lfs_SI), "lfs_SI", NA)),
         sex_provided_visual = ifelse(female_proportion == 1 & method_sex == 1, "f",
                                      ifelse(female_proportion == 0 & method_sex == 1, "m", NA)),
         sex_provided_length = ifelse(female_proportion == 1 & method_sex == 0, "f",
                                      ifelse(female_proportion == 0 & method_sex == 0, "m", NA)),
         sex_provided_nomethod = ifelse(female_proportion == 1 & is.na(method_sex), "f",
                                        ifelse(female_proportion == 0 & is.na(method_sex), "m", NA)),
         sex_SI = ifelse(SI == "S6", "m",
                         ifelse(SI == "S5" | SI == "S4", "f", NA)),
         sex_length = ifelse(lfs_code == "S" & lengthmm < 451, "m",
                             ifelse(lengthmm > 450, "f", NA)), 
         sex = case_when(!is.na(sex_provided_visual) ~ sex_provided_visual,
                         !is.na(sex_provided_nomethod) ~ sex_provided_nomethod,
                         !is.na(sex_provided_length) ~ sex_provided_length,
                         !is.na(sex_SI) ~ sex_SI,
                         !is.na(sex_length) ~ sex_length,
                         TRUE ~ NA),
         sex_source = case_when(!is.na(sex_provided_visual) ~ "sex_provided_visual",
                                !is.na(sex_provided_nomethod) ~ "sex_provided_nomethod",
                                !is.na(sex_provided_length) ~ "sex_provided_length",
                                !is.na(sex_SI) ~ "sex_SI",
                                !is.na(sex_length) ~ "sex_length",
                                TRUE ~ NA)) %>% 
  select(-S1, -S2, -S3, -S4, -S5, -S6) %>% 
  relocate(sex_provided_visual, sex_provided_nomethod, sex_provided_length, sex_SI, sex_length, .after = sex) %>% 
  relocate(fi_lfs_code, ser_lfs_code, lfs_SI, .after = sex)




# 2.2 Remove data not needed and create dataframe for spatial only analyses ####

# 2.2.1 base

#create a dataframe with all individuals where length is less than 30mm
individual_removed_base <- individual_all_base %>% 
  filter(lengthmm < 30)

#create dataframe where fish with length less than 30mm are removed
individual_cleaned_base <- anti_join(individual_all_base, individual_removed_base, by = "mei_fi_id")

#create dataframe where fish with length less than 30mm are removed for the most recent 10 years (for spatial analyses)
individual_cleaned_spatial_base <- individual_cleaned_base %>% 
  filter(!is.na(fi_year) & fi_year > 2012 & fi_year < 2024)


# 2.2.2 recent

#create a dataframe with all individuals where length is less than 30mm
individual_removed <- individual_all %>% 
  filter(lengthmm < 30)

#create dataframe where fish with length less than 30mm are removed
individual_cleaned <- anti_join(individual_all, individual_removed, by = "mei_fi_id")

#create dataframe where fish with length less than 30mm are removed for the most recent 10 years (for spatial analyses)
individual_cleaned_spatial <- individual_cleaned %>% 
  filter(!is.na(fi_year) & fi_year > 2012 & fi_year < 2024)


# 2.3 Store data ####
save(individual_all_base, file = "data/individual_all_base.RData")
save(individual_removed_base, file = "data/individual_removed_base.RData")
save(individual_cleaned_base, individual_cleaned_spatial_base, file = "data/individual_cleaned_base.RData")
save(individual_all, file = "data/individual_all.RData")
save(individual_removed, file = "data/individual_removed.RData")
save(individual_cleaned, individual_cleaned_spatial, file = "data/individual_cleaned.RData")
