
# 1. Load Libraries ###########################################################################################

#define libraries needed
libs <- c("tidyverse", "icesTAF", "sf") 

#define libraries already installed
installed_libs <- libs %in% rownames(installed.packages())

#install libraries that are not installed already
if (any(installed_libs == F)) {
  install.packages(libs[!installed_libs])
}

#load libraries needed
invisible(lapply(libs, library, character.only = T))




# 2. Load Data/create directory/rename  ######################################################################

#load data
load("data/individual_cleaned.RData")

#create directory
mkdir("data/LHTsite")




# 3. ALL: Collate all time series data that are available (by site)  #############

# 3.1 Spatiotemporal analyses data ####

# 3.1.1 Create summaries that are not sex-specific to left_join sex and analyses specific data in the end

#reduce to site specific data 
individual_site_spatiotemp <- individual_cleaned %>% 
  filter(source == "series")

#create summary of all site specific data (not sex specific, to left_join all sex specific data in the end)
summary_site_spatiotemp <- individual_site_spatiotemp %>% 
  group_by(ser_nameshort) %>% 
  summarize(ser_x = max(ser_x),
            ser_y = min(ser_y),
            n_spatiotemp = length(unique(mei_fi_id)),
            n_silver_spatiotemp = length(unique(mei_fi_id[which(lfs_code == "S")])),
            n_yellow_spatiotemp = length(unique(mei_fi_id[which(lfs_code == "Y")])),
            no_of_years_spatiotemp = length(unique(fi_year)),
            min_year_spatiotemp = min(fi_year),
            max_year_spatiotemp = max(fi_year)) %>% 
  arrange(n_spatiotemp)


# 3.1.2 Create sex-specific summaries of all site specific data 

#reduce to site specific data
individual_site_spatiotemp_f <- individual_site_spatiotemp %>%
  filter(sex == "f") 

individual_site_spatiotemp_m <- individual_site_spatiotemp %>%
  filter(sex == "m") 

#create summary of site specific data for male/female
summary_site_spatiotemp_f <- individual_site_spatiotemp_f %>% 
  group_by(ser_nameshort) %>% 
  summarize(n_spatiotemp_f = length(unique(mei_fi_id)),
            n_silver_spatiotemp_f = length(unique(mei_fi_id[which(lfs_code == "S")])),
            n_yellow_spatiotemp_f = length(unique(mei_fi_id[which(lfs_code == "Y")])),
            no_of_years_spatiotemp_f = length(unique(fi_year)),
            min_year_spatiotemp_f = min(fi_year),
            max_year_spatiotemp_f = max(fi_year))

summary_site_spatiotemp_m <- individual_site_spatiotemp_m %>% 
  group_by(ser_nameshort) %>% 
  summarize(n_spatiotemp_m = length(unique(mei_fi_id)),
            n_silver_spatiotemp_m = length(unique(mei_fi_id[which(lfs_code == "S")])),
            n_yellow_spatiotemp_m = length(unique(mei_fi_id[which(lfs_code == "Y")])),
            no_of_years_spatiotemp_m = length(unique(fi_year)),
            min_year_spatiotemp_m = min(fi_year),
            max_year_spatiotemp_m = max(fi_year)) 


# 3.2 Spatial-only analyses data ####

# 3.2.1 Create a summary that is not sex-specific

#reduce to site specific data 
individual_site_spatial <- individual_cleaned_spatial %>%
  filter(source == "series")

#create summary of all site specific data (not sex specific, to left_join all sex specific data in the end)
summary_site_spatial <- individual_site_spatial %>% 
  group_by(ser_nameshort) %>% 
  summarize(ser_x = max(ser_x),
            ser_y = min(ser_y),
            n_spatial = length(unique(mei_fi_id)),
            n_silver_spatial = length(unique(mei_fi_id[which(lfs_code == "S")])),
            n_yellow_spatial = length(unique(mei_fi_id[which(lfs_code == "Y")])),
            no_of_years_spatial = length(unique(fi_year)),
            min_year_spatial = min(fi_year),
            max_year_spatial = max(fi_year))


# 3.2.2 Create sex-specific summaries of all site specific data 

#create dataframe for individual data
individual_site_spatial_f <- individual_cleaned_spatial %>%
  filter(sex == "f") 

individual_site_spatial_m <- individual_cleaned_spatial %>%
  filter(sex == "m") 

#create dataframe with summary of available data
summary_site_spatial_f <- individual_site_spatial_f %>% 
  group_by(ser_nameshort) %>% 
  summarize(n_spatial_f = length(unique(mei_fi_id)),
            no_of_years_spatial_f = length(unique(fi_year)),
            min_year_spatial_f = min(fi_year),
            max_year_spatial_f = max(fi_year))

summary_site_spatial_m <- individual_site_spatial_m %>% 
  group_by(ser_nameshort) %>% 
  summarize(n_spatial_m = length(unique(mei_fi_id)),
            no_of_years_spatial_m = length(unique(fi_year)),
            min_year_spatial_m = min(fi_year),
            max_year_spatial_m = max(fi_year))




# 4. GROWTH: Collate data that are suitable for analyses of growth (by site)  #############

# 4.1 spatiotemporal analyses data ####

#create a dataframe individual data of yellow eels, where age and length are available
individual_site_growth_spatiotemp_f <- individual_site_spatiotemp_f %>%
  filter(lfs_code == "Y") %>% 
  filter(!is.na(ageyear) & !is.na(lengthmm) & !is.na(fi_year)) 

individual_site_growth_spatiotemp_m <- individual_site_spatiotemp_m %>%
  filter(lfs_code == "Y") %>% 
  filter(!is.na(ageyear) & !is.na(lengthmm) & !is.na(fi_year))

#create a dataframe summarizing data availability for yellow eels, where age, length and year are available
summary_site_growth_spatiotemp_f <- individual_site_growth_spatiotemp_f %>% 
  group_by(ser_nameshort) %>% 
  summarize(n_growth_spatiotemp_f = length(unique(mei_fi_id)),
            no_of_years_growth_spatiotemp_f = length(unique(fi_year)),
            min_year_growth_spatiotemp_f = min(fi_year),
            max_year_growth_spatiotemp_f = max(fi_year)) %>% 
  mutate(quality_growth_spatiotemp_f = ifelse(no_of_years_growth_spatiotemp_f > 14 & n_growth_spatiotemp_f/no_of_years_growth_spatiotemp_f >9, 1,
                                            ifelse(no_of_years_growth_spatiotemp_f > 9 & n_growth_spatiotemp_f/no_of_years_growth_spatiotemp_f >9, 2, 3)))

summary_site_growth_spatiotemp_m <- individual_site_growth_spatiotemp_m %>% 
  group_by(ser_nameshort) %>% 
  summarize(n_growth_spatiotemp_m = length(unique(mei_fi_id)),
            no_of_years_growth_spatiotemp_m = length(unique(fi_year)),
            min_year_growth_spatiotemp_m = min(fi_year),
            max_year_growth_spatiotemp_m = max(fi_year)) %>% 
  mutate(quality_growth_spatiotemp_m = ifelse(no_of_years_growth_spatiotemp_m > 14 & n_growth_spatiotemp_m/no_of_years_growth_spatiotemp_m >9, 1,
                                              ifelse(no_of_years_growth_spatiotemp_m > 9 & n_growth_spatiotemp_m/no_of_years_growth_spatiotemp_m >9, 2, 3)))


# 4.2 Spatial only analyses data ####  

#create a dataframe individual data of yellow eels, where age and length are available (2018-2023)
individual_site_growth_spatial_f <- individual_site_spatial_f %>%
  filter(lfs_code == "Y") %>% 
  filter(!is.na(ageyear) & !is.na(lengthmm))

individual_site_growth_spatial_m <- individual_site_spatial_m %>%
  filter(lfs_code == "Y") %>% 
  filter(!is.na(ageyear) & !is.na(lengthmm))

#create a dataframe summarizing data availability for yellow eels, where age and length are available
summary_site_growth_spatial_f <- individual_site_growth_spatial_f %>% 
  group_by(ser_nameshort) %>% 
  summarize(n_growth_spatial_f = length(unique(mei_fi_id)),
            no_of_years_growth_spatial_f = length(unique(fi_year)),
            min_year_growth_spatial_f = min(fi_year),
            max_year_growth_spatial_f = max(fi_year)) %>% 
  mutate(quality_growth_spatial_f = ifelse(n_growth_spatial_f/no_of_years_growth_spatial_f >29, 1,
                                         ifelse(n_growth_spatial_f/no_of_years_growth_spatial_f >9, 2, 3)))

summary_site_growth_spatial_m <- individual_site_growth_spatial_m %>% 
  group_by(ser_nameshort) %>% 
  summarize(n_growth_spatial_m = length(unique(mei_fi_id)),
            no_of_years_growth_spatial_m = length(unique(fi_year)),
            min_year_growth_spatial_m = min(fi_year),
            max_year_growth_spatial_m = max(fi_year)) %>% 
  mutate(quality_growth_spatial_m = ifelse(n_growth_spatial_m/no_of_years_growth_spatial_m >29, 1,
                                         ifelse(n_growth_spatial_m/no_of_years_growth_spatial_m >9, 2, 3)))




# 5. TRANSITION: Collate data that are suitable for analyses of stage transition (by site)  #############           

# 5.1 Spatiotempoal analyses data ####

#create a temporary dataframe where for a given combination of year and site there are both yellow and silver eel lengths available
temp_transition_site_spatiotemp_f <- individual_site_spatiotemp_f %>% 
  group_by(ser_nameshort, fi_year) %>% 
  summarize(n_yellow_f = sum((lfs_code == "Y" & !is.na(lengthmm))),
            n_silver_f = sum((lfs_code == "S" & !is.na(lengthmm)))) %>% 
  mutate(site_year = paste(ser_nameshort, fi_year, sep = "_")) %>% 
  filter(n_yellow_f > 2 & !is.na(n_yellow_f) & n_silver_f > 2 & !is.na(n_silver_f))

temp_transition_site_spatiotemp_m <- individual_site_spatiotemp_m %>% 
  group_by(ser_nameshort, fi_year) %>% 
  summarize(n_yellow_m = sum((lfs_code == "Y" & !is.na(lengthmm))),
            n_silver_m = sum((lfs_code == "S" & !is.na(lengthmm)))) %>% 
  mutate(site_year = paste(ser_nameshort, fi_year, sep = "_")) %>% 
  filter(n_yellow_m > 2 & !is.na(n_yellow_m) & n_silver_m > 2 & !is.na(n_silver_m))

#create a dataframe containing only individual data of eel where for a given combination of site/year both yellow and silver length are available
individual_site_transition_spatiotemp_f <- individual_site_spatiotemp_f %>% 
  mutate(site_year = paste(ser_nameshort, fi_year, sep = "_")) %>% 
  filter(site_year %in% temp_transition_site_spatiotemp_f$site_year) %>%
  select(-site_year)

individual_site_transition_spatiotemp_m <- individual_site_spatiotemp_m %>% 
  mutate(site_year = paste(ser_nameshort, fi_year, sep = "_")) %>% 
  filter(site_year %in% temp_transition_site_spatiotemp_m$site_year) %>%
  select(-site_year) 

#create a dataframe summarizing data availability for site/year combinations with length of both, yellow and silver eel
summary_site_transition_spatiotemp_f <- individual_site_transition_spatiotemp_f %>% 
  group_by(ser_nameshort) %>% 
  summarize(n_transition_spatiotemp_f = length(unique(mei_fi_id)),
            no_of_years_transition_spatiotemp_f = length(unique(fi_year)),
            min_year_transition_spatiotemp_f = min(fi_year),
            max_year_transition_spatiotemp_f = max(fi_year)) %>% 
  mutate(quality_transition_spatiotemp_f = ifelse(no_of_years_transition_spatiotemp_f > 14 & n_transition_spatiotemp_f/no_of_years_transition_spatiotemp_f >9, 1,
                                                ifelse(no_of_years_transition_spatiotemp_f > 9 & n_transition_spatiotemp_f/no_of_years_transition_spatiotemp_f >9, 2, 3)))

summary_site_transition_spatiotemp_m <- individual_site_transition_spatiotemp_m %>% 
  group_by(ser_nameshort) %>% 
  summarize(n_transition_spatiotemp_m = length(unique(mei_fi_id)),
            no_of_years_transition_spatiotemp_m = length(unique(fi_year)),
            min_year_transition_spatiotemp_m = min(fi_year),
            max_year_transition_spatiotemp_m = max(fi_year)) %>% 
  mutate(quality_transition_spatiotemp_m = ifelse(no_of_years_transition_spatiotemp_m > 14 & n_transition_spatiotemp_m/no_of_years_transition_spatiotemp_m >9, 1,
                                                ifelse(no_of_years_transition_spatiotemp_m > 9 & n_transition_spatiotemp_m/no_of_years_transition_spatiotemp_m >9, 2, 3)))


# 5.2 Spatial-only analyses data ####

#create a temporary dataframe where for a site there are both yellow and silver eel available
temp_transition_site_spatial_f <- individual_site_spatial_f %>% 
  group_by(ser_nameshort) %>% 
  summarize(n_yellow_f = sum((lfs_code == "Y" & !is.na(lengthmm))),
            n_silver_f = sum((lfs_code == "S" & !is.na(lengthmm)))) %>% 
  filter(n_yellow_f > 0 & !is.na(n_yellow_f) & n_silver_f > 0 & !is.na(n_silver_f))

temp_transition_site_spatial_m <- individual_site_spatial_m %>% 
  group_by(ser_nameshort) %>% 
  summarize(n_yellow_m = sum((lfs_code == "Y" & !is.na(lengthmm))),
            n_silver_m = sum((lfs_code == "S" & !is.na(lengthmm)))) %>% 
  filter(n_yellow_m > 0 & !is.na(n_yellow_m) & n_silver_m > 0 & !is.na(n_silver_m))

#create a dataframe containing only individual data of eel where for a given combination of site/year both yellow and silver length are available
individual_site_transition_spatial_f <- individual_site_spatial_f %>% 
  filter(ser_nameshort %in% temp_transition_site_spatial_f$ser_nameshort)

individual_site_transition_spatial_m <- individual_site_spatial_m %>% 
  filter(ser_nameshort %in% temp_transition_site_spatial_m$ser_nameshort)

#create a dataframe summarizing data availability for site/year combinations with length of both, yellow and silver eel
summary_site_transition_spatial_f <- individual_site_transition_spatial_f %>% 
  group_by(ser_nameshort) %>% 
  summarize(n_transition_Y_spatial_f = sum((lfs_code == "Y")),
            n_transition_S_spatial_f = sum((lfs_code == "S")),
            no_of_years_transition_spatial_f = length(unique(fi_year)),
            min_year_transition_spatial_f = min(fi_year),
            max_year_transition_spatial_f = max(fi_year)) %>% 
  mutate(quality_transition_spatial_f = ifelse(n_transition_Y_spatial_f >29 & n_transition_S_spatial_f >29, 1,
                                             ifelse(n_transition_Y_spatial_f >9 & n_transition_S_spatial_f > 9, 2, 3)))

summary_site_transition_spatial_m <- individual_site_transition_spatial_m %>% 
  group_by(ser_nameshort) %>% 
  summarize(n_transition_Y_spatial_m = sum((lfs_code == "Y")),
            n_transition_S_spatial_m = sum((lfs_code == "S")),
            no_of_years_transition_spatial_m = length(unique(fi_year)),
            min_year_transition_spatial_m = min(fi_year),
            max_year_transition_spatial_m = max(fi_year)) %>% 
  mutate(quality_transition_spatial_m = ifelse(n_transition_Y_spatial_m >29 & n_transition_S_spatial_m >29, 1,
                                             ifelse(n_transition_Y_spatial_m >9 & n_transition_S_spatial_m > 9, 2, 3)))




# 6. SILVER-LENGTH: Collate data that are suitable for analyses of silver eel length (by site)  #############

# 6.1 Spatiotemporal analyses data ####

#create a dataframe individual data of silver eels where length is available
individual_site_silverlength_spatiotemp_f <- individual_site_spatiotemp_f %>%
  filter(lfs_code == "S" & !is.na(lengthmm) & !is.na(fi_year)) 

individual_site_silverlength_spatiotemp_m <- individual_site_spatiotemp_m %>%
  filter(lfs_code == "S" & !is.na(lengthmm) & !is.na(fi_year)) 

#create a dataframe summarizing data availability of silver eels where length is available
summary_site_silverlength_spatiotemp_f <- individual_site_silverlength_spatiotemp_f %>% 
  group_by(ser_nameshort) %>% 
  summarize(n_silverlength_spatiotemp_f = length(unique(mei_fi_id)),
            no_of_years_silverlength_spatiotemp_f = length(unique(fi_year)),
            min_year_silverlength_spatiotemp_f = min(fi_year),
            max_year_silverlength_spatiotemp_f = max(fi_year)) %>% 
  mutate(quality_silverlength_spatiotemp_f = ifelse(no_of_years_silverlength_spatiotemp_f > 14 & n_silverlength_spatiotemp_f/no_of_years_silverlength_spatiotemp_f >9, 1,
                                            ifelse(no_of_years_silverlength_spatiotemp_f > 9 & n_silverlength_spatiotemp_f/no_of_years_silverlength_spatiotemp_f >9, 2, 3)))

summary_site_silverlength_spatiotemp_m <- individual_site_silverlength_spatiotemp_m %>% 
  group_by(ser_nameshort) %>% 
  summarize(n_silverlength_spatiotemp_m = length(unique(mei_fi_id)),
            no_of_years_silverlength_spatiotemp_m = length(unique(fi_year)),
            min_year_silverlength_spatiotemp_m = min(fi_year),
            max_year_silverlength_spatiotemp_m = max(fi_year)) %>% 
  mutate(quality_silverlength_spatiotemp_m = ifelse(no_of_years_silverlength_spatiotemp_m > 14 & n_silverlength_spatiotemp_m/no_of_years_silverlength_spatiotemp_m >9, 1,
                                            ifelse(no_of_years_silverlength_spatiotemp_m > 9 & n_silverlength_spatiotemp_m/no_of_years_silverlength_spatiotemp_m >9, 2, 3)))


# 6.2 Spatial-only analyses data ####

#create a dataframe individual data of silver eels where length is available
individual_site_silverlength_spatial_f <- individual_site_spatial_f %>%
  filter(lfs_code == "S" & !is.na(lengthmm))  

individual_site_silverlength_spatial_m <- individual_site_spatial_m %>%
  filter(lfs_code == "S" & !is.na(lengthmm)) 

#create a dataframe summarizing data availability of silver eels where length is available
summary_site_silverlength_spatial_f <- individual_site_silverlength_spatial_f %>% 
  group_by(ser_nameshort) %>% 
  summarize(n_silverlength_spatial_f = length(unique(mei_fi_id)),
            no_of_years_silverlength_spatial_f = length(unique(fi_year)),
            min_year_silverlength_spatial_f = min(fi_year),
            max_year_silverlength_spatial_f = max(fi_year)) %>% 
  mutate(quality_silverlength_spatial_f = ifelse(n_silverlength_spatial_f >49, 1,
                                         ifelse(n_silverlength_spatial_f >24, 2, 3)))

summary_site_silverlength_spatial_m <- individual_site_silverlength_spatial_m %>% 
  group_by(ser_nameshort) %>% 
  summarize(n_silverlength_spatial_m = length(unique(mei_fi_id)),
            no_of_years_silverlength_spatial_m = length(unique(fi_year)),
            min_year_silverlength_spatial_m = min(fi_year),
            max_year_silverlength_spatial_m = max(fi_year)) %>% 
  mutate(quality_silverlength_spatial_m = ifelse(n_silverlength_spatial_m >49, 1,
                                         ifelse(n_silverlength_spatial_m >24, 2, 3)))




# 7. EXPORT: Collate data,  save and export summarized and individual datasets ##################

# summarize spatiotemporal analyses data in one table
summary_site_spatiotemp <- summary_site_spatiotemp %>%
  left_join(summary_site_spatiotemp_f, by ="ser_nameshort") %>%
  left_join(summary_site_spatiotemp_m, by ="ser_nameshort") %>%
  left_join(summary_site_growth_spatiotemp_f, by ="ser_nameshort") %>% 
  left_join(summary_site_growth_spatiotemp_m, by ="ser_nameshort") %>%
  left_join(summary_site_transition_spatiotemp_f, by ="ser_nameshort") %>% 
  left_join(summary_site_transition_spatiotemp_m, by ="ser_nameshort") %>%
  left_join(summary_site_silverlength_spatiotemp_f, by ="ser_nameshort") %>%
  left_join(summary_site_silverlength_spatiotemp_m, by ="ser_nameshort") 

# summarize spatial analyses data in one table
summary_site_spatial <- summary_site_spatial %>%
  left_join(summary_site_spatial_f, by ="ser_nameshort") %>%
  left_join(summary_site_spatial_m, by ="ser_nameshort") %>%
  left_join(summary_site_growth_spatial_f, by ="ser_nameshort") %>%
  left_join(summary_site_growth_spatial_m, by ="ser_nameshort") %>%
  left_join(summary_site_transition_spatial_f, by ="ser_nameshort") %>%
  left_join(summary_site_transition_spatial_m, by ="ser_nameshort") %>%
  left_join(summary_site_silverlength_spatial_f, by ="ser_nameshort") %>%
  left_join(summary_site_silverlength_spatial_m, by ="ser_nameshort")

#add quality to individual site 
individual_site_spatiotemp_quality <- individual_site_spatiotemp %>% 
  left_join(summary_site_spatiotemp %>% select(ser_nameshort, quality_growth_spatiotemp_f, quality_growth_spatiotemp_m, 
                                               quality_transition_spatiotemp_f, quality_transition_spatiotemp_m, 
                                               quality_silverlength_spatiotemp_f, quality_silverlength_spatiotemp_m), by = "ser_nameshort") %>% 
  mutate(quality_growth_spatiotemp_f = ifelse(mei_fi_id %in% individual_site_growth_spatiotemp_f$mei_fi_id, quality_growth_spatiotemp_f, NA),
         quality_growth_spatiotemp_m = ifelse(mei_fi_id %in% individual_site_growth_spatiotemp_m$mei_fi_id, quality_growth_spatiotemp_m, NA),
         quality_transition_spatiotemp_f = ifelse(mei_fi_id %in% individual_site_transition_spatiotemp_f$mei_fi_id, quality_transition_spatiotemp_f, NA),
         quality_transition_spatiotemp_m = ifelse(mei_fi_id %in% individual_site_transition_spatiotemp_m$mei_fi_id, quality_transition_spatiotemp_m, NA),
         quality_silverlength_spatiotemp_f = ifelse(mei_fi_id %in% individual_site_silverlength_spatiotemp_f$mei_fi_id, quality_silverlength_spatiotemp_f, NA),
         quality_silverlength_spatiotemp_m = ifelse(mei_fi_id %in% individual_site_silverlength_spatiotemp_m$mei_fi_id, quality_silverlength_spatiotemp_m, NA))

#add quality to individual site spatial
individual_site_spatial_quality <- individual_site_spatial %>% 
  left_join(summary_site_spatial %>% select(ser_nameshort, quality_growth_spatial_f, quality_growth_spatial_m, 
                                               quality_transition_spatial_f, quality_transition_spatial_m, 
                                               quality_silverlength_spatial_f, quality_silverlength_spatial_m), by = "ser_nameshort") %>% 
  mutate(quality_growth_spatial_f = ifelse(mei_fi_id %in% individual_site_growth_spatial_f$mei_fi_id, quality_growth_spatial_f, NA),
         quality_growth_spatial_m = ifelse(mei_fi_id %in% individual_site_growth_spatial_m$mei_fi_id, quality_growth_spatial_m, NA),
         quality_transition_spatial_f = ifelse(mei_fi_id %in% individual_site_transition_spatial_f$mei_fi_id, quality_transition_spatial_f, NA),
         quality_transition_spatial_m = ifelse(mei_fi_id %in% individual_site_transition_spatial_m$mei_fi_id, quality_transition_spatial_m, NA),
         quality_silverlength_spatial_f = ifelse(mei_fi_id %in% individual_site_silverlength_spatial_f$mei_fi_id, quality_silverlength_spatial_f, NA),
         quality_silverlength_spatial_m = ifelse(mei_fi_id %in% individual_site_silverlength_spatial_m$mei_fi_id, quality_silverlength_spatial_m, NA))

#save summariy by site for spatiotemporal data
save(summary_site_spatiotemp, file = "data/LHTsite/summary_site_spatiotemp.RData")

#save summariy by site for spatial data
save(summary_site_spatial, file = "data/LHTsite/summary_site_spatial.RData")

#save individual data alongside quality
save(individual_site_spatiotemp_quality, file = "data/LHTsite/individual_site_spatiotemp_quality.RData")

#save individual spatial only data alongside quality
save(individual_site_spatial_quality, file = "data/LHTsite/individual_site_spatial_quality.RData")


