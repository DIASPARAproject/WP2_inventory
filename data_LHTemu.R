
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
mkdir("data/LHTemu")




# 3. ALL: Collate all time series data that are available (by emu)  #############

# 3.1 Spatiotemporal analyses data ####

# 3.1.1 Create summaries that are not sex-specific to left_join sex and analyses specific data in the end

#create summary of all emu specific data (not sex specific, to left_join all sex specific data in the end)
summary_emu_spatiotemp <- individual_cleaned %>% 
  group_by(ser_emu_nameshort) %>% 
  summarize(ser_x = max(ser_x),
            ser_y = min(ser_y),
            n_spatiotemp = length(unique(mei_fi_id)),
            n_silver_spatiotemp = length(unique(mei_fi_id[which(lfs_code == "S")])),
            n_yellow_spatiotemp = length(unique(mei_fi_id[which(lfs_code == "Y")])),
            no_of_years_spatiotemp = length(unique(fi_year)),
            min_year_spatiotemp = min(fi_year),
            max_year_spatiotemp = max(fi_year)) %>% 
  arrange(n_spatiotemp)


# 3.1.2 Create sex-specific summaries of all emu specific data 

#reduce to emu specific data
individual_emu_spatiotemp_f <- individual_cleaned %>%
  filter(sex == "f") 

individual_emu_spatiotemp_m <- individual_cleaned %>%
  filter(sex == "m") 

#create summary of emu specific data for male/female
summary_emu_spatiotemp_f <- individual_emu_spatiotemp_f %>% 
  group_by(ser_emu_nameshort) %>% 
  summarize(n_spatiotemp_f = length(unique(mei_fi_id)),
            n_silver_spatiotemp_f = length(unique(mei_fi_id[which(lfs_code == "S")])),
            n_yellow_spatiotemp_f = length(unique(mei_fi_id[which(lfs_code == "Y")])),
            no_of_years_spatiotemp_f = length(unique(fi_year)),
            min_year_spatiotemp_f = min(fi_year),
            max_year_spatiotemp_f = max(fi_year))

summary_emu_spatiotemp_m <- individual_emu_spatiotemp_m %>% 
  group_by(ser_emu_nameshort) %>% 
  summarize(n_spatiotemp_m = length(unique(mei_fi_id)),
            n_silver_spatiotemp_m = length(unique(mei_fi_id[which(lfs_code == "S")])),
            n_yellow_spatiotemp_m = length(unique(mei_fi_id[which(lfs_code == "Y")])),
            no_of_years_spatiotemp_m = length(unique(fi_year)),
            min_year_spatiotemp_m = min(fi_year),
            max_year_spatiotemp_m = max(fi_year)) 


# 3.2 Spatial-only analyses data ####

# 3.2.1 Create a summary that is not sex-specific

#create summary of all emu specific data (not sex specific, to left_join all sex specific data in the end)
summary_emu_spatial <- individual_cleaned_spatial %>% 
  group_by(ser_emu_nameshort) %>% 
  summarize(ser_x = max(ser_x),
            ser_y = min(ser_y),
            n_spatial = length(unique(mei_fi_id)),
            n_silver_spatial = length(unique(mei_fi_id[which(lfs_code == "S")])),
            n_yellow_spatial = length(unique(mei_fi_id[which(lfs_code == "Y")])),
            no_of_years_spatial = length(unique(fi_year)),
            min_year_spatial = min(fi_year),
            max_year_spatial = max(fi_year))


# 3.2.2 Create sex-specific summaries of all emu specific data 

#create dataframe for individual data
individual_emu_spatial_f <- individual_cleaned_spatial %>%
  filter(sex == "f") 

individual_emu_spatial_m <- individual_cleaned_spatial %>%
  filter(sex == "m") 

#create dataframe with summary of available data
summary_emu_spatial_f <- individual_emu_spatial_f %>% 
  group_by(ser_emu_nameshort) %>% 
  summarize(n_spatial_f = length(unique(mei_fi_id)),
            no_of_years_spatial_f = length(unique(fi_year)),
            min_year_spatial_f = min(fi_year),
            max_year_spatial_f = max(fi_year))

summary_emu_spatial_m <- individual_emu_spatial_m %>% 
  group_by(ser_emu_nameshort) %>% 
  summarize(n_spatial_m = length(unique(mei_fi_id)),
            no_of_years_spatial_m = length(unique(fi_year)),
            min_year_spatial_m = min(fi_year),
            max_year_spatial_m = max(fi_year))




# 4. GROWTH: Collate data that are suitable for analyses of growth (by emu)  #############

# 4.1 spatiotemporal analyses data ####

#create a dataframe individual data of yellow eels, where age and length are available
individual_emu_growth_spatiotemp_f <- individual_emu_spatiotemp_f %>%
  filter(lfs_code == "Y") %>% 
  filter(!is.na(ageyear) & !is.na(lengthmm) & !is.na(fi_year)) 

individual_emu_growth_spatiotemp_m <- individual_emu_spatiotemp_m %>%
  filter(lfs_code == "Y") %>% 
  filter(!is.na(ageyear) & !is.na(lengthmm) & !is.na(fi_year))

#create a dataframe summarizing data availability for yellow eels, where age and length are available
summary_emu_growth_spatiotemp_f <- individual_emu_growth_spatiotemp_f %>% 
  group_by(ser_emu_nameshort) %>% 
  summarize(n_growth_spatiotemp_f = length(unique(mei_fi_id)),
            no_of_years_growth_spatiotemp_f = length(unique(fi_year)),
            min_year_growth_spatiotemp_f = min(fi_year),
            max_year_growth_spatiotemp_f = max(fi_year)) %>% 
  mutate(quality_growth_spatiotemp_f = ifelse(no_of_years_growth_spatiotemp_f > 14 & n_growth_spatiotemp_f/no_of_years_growth_spatiotemp_f >9, 1,
                                              ifelse(no_of_years_growth_spatiotemp_f > 9 & n_growth_spatiotemp_f/no_of_years_growth_spatiotemp_f >9, 2, 3)))

summary_emu_growth_spatiotemp_m <- individual_emu_growth_spatiotemp_m %>% 
  group_by(ser_emu_nameshort) %>% 
  summarize(n_growth_spatiotemp_m = length(unique(mei_fi_id)),
            no_of_years_growth_spatiotemp_m = length(unique(fi_year)),
            min_year_growth_spatiotemp_m = min(fi_year),
            max_year_growth_spatiotemp_m = max(fi_year)) %>% 
  mutate(quality_growth_spatiotemp_m = ifelse(no_of_years_growth_spatiotemp_m > 14 & n_growth_spatiotemp_m/no_of_years_growth_spatiotemp_m >9, 1,
                                              ifelse(no_of_years_growth_spatiotemp_m > 9 & n_growth_spatiotemp_m/no_of_years_growth_spatiotemp_m >9, 2, 3)))


# 4.2 Spatial only analyses data ####  

#create a dataframe individual data of yellow eels, where age and length are available (2018-2023)
individual_emu_growth_spatial_f <- individual_emu_spatial_f %>%
  filter(lfs_code == "Y") %>% 
  filter(!is.na(ageyear) & !is.na(lengthmm))

individual_emu_growth_spatial_m <- individual_emu_spatial_m %>%
  filter(lfs_code == "Y") %>% 
  filter(!is.na(ageyear) & !is.na(lengthmm))

#create a dataframe summarizing data availability for yellow eels, where age and length are available
summary_emu_growth_spatial_f <- individual_emu_growth_spatial_f %>% 
  group_by(ser_emu_nameshort) %>% 
  summarize(n_growth_spatial_f = length(unique(mei_fi_id)),
            no_of_years_growth_spatial_f = length(unique(fi_year)),
            min_year_growth_spatial_f = min(fi_year),
            max_year_growth_spatial_f = max(fi_year)) %>% 
  mutate(quality_growth_spatial_f = ifelse(n_growth_spatial_f/no_of_years_growth_spatial_f >29, 1,
                                           ifelse(n_growth_spatial_f/no_of_years_growth_spatial_f >9, 2, 3)))

summary_emu_growth_spatial_m <- individual_emu_growth_spatial_m %>% 
  group_by(ser_emu_nameshort) %>% 
  summarize(n_growth_spatial_m = length(unique(mei_fi_id)),
            no_of_years_growth_spatial_m = length(unique(fi_year)),
            min_year_growth_spatial_m = min(fi_year),
            max_year_growth_spatial_m = max(fi_year)) %>% 
  mutate(quality_growth_spatial_m = ifelse(n_growth_spatial_m/no_of_years_growth_spatial_m >29, 1,
                                           ifelse(n_growth_spatial_m/no_of_years_growth_spatial_m >9, 2, 3)))




# 5. TRANSITION: Collate data that are suitable for analyses of stage transition (by emu)  #############           

# 5.1 Spatiotempoal analyses data ####

#create a temporary dataframe where for a given combination of year and emu there are both yellow and silver eel lengths available
temp_transition_emu_spatiotemp_f <- individual_emu_spatiotemp_f %>% 
  group_by(ser_emu_nameshort, fi_year) %>% 
  summarize(n_yellow_f = sum((lfs_code == "Y" & !is.na(lengthmm))),
            n_silver_f = sum((lfs_code == "S" & !is.na(lengthmm)))) %>% 
  mutate(emu_year = paste(ser_emu_nameshort, fi_year, sep = "_")) %>% 
  filter(n_yellow_f > 2 & !is.na(n_yellow_f) & n_silver_f > 2 & !is.na(n_silver_f))

temp_transition_emu_spatiotemp_m <- individual_emu_spatiotemp_m %>% 
  group_by(ser_emu_nameshort, fi_year) %>% 
  summarize(n_yellow_m = sum((lfs_code == "Y" & !is.na(lengthmm))),
            n_silver_m = sum((lfs_code == "S" & !is.na(lengthmm)))) %>% 
  mutate(emu_year = paste(ser_emu_nameshort, fi_year, sep = "_")) %>% 
  filter(n_yellow_m > 2 & !is.na(n_yellow_m) & n_silver_m > 2 & !is.na(n_silver_m))

#create a dataframe containing only individual data of eel where for a given combination of emu/year both yellow and silver length are available
individual_emu_transition_spatiotemp_f <- individual_emu_spatiotemp_f %>% 
  mutate(emu_year = paste(ser_emu_nameshort, fi_year, sep = "_")) %>% 
  filter(emu_year %in% temp_transition_emu_spatiotemp_f$emu_year) %>%
  select(-emu_year)

individual_emu_transition_spatiotemp_m <- individual_emu_spatiotemp_m %>% 
  mutate(emu_year = paste(ser_emu_nameshort, fi_year, sep = "_")) %>% 
  filter(emu_year %in% temp_transition_emu_spatiotemp_m$emu_year) %>%
  select(-emu_year) 

#create a dataframe summarizing data availability for emu/year combinations with length of both, yellow and silver eel
summary_emu_transition_spatiotemp_f <- individual_emu_transition_spatiotemp_f %>% 
  group_by(ser_emu_nameshort) %>% 
  summarize(n_transition_spatiotemp_f = length(unique(mei_fi_id)),
            no_of_years_transition_spatiotemp_f = length(unique(fi_year)),
            min_year_transition_spatiotemp_f = min(fi_year),
            max_year_transition_spatiotemp_f = max(fi_year)) %>% 
  mutate(quality_transition_spatiotemp_f = ifelse(no_of_years_transition_spatiotemp_f > 14 & n_transition_spatiotemp_f/no_of_years_transition_spatiotemp_f >9, 1,
                                                  ifelse(no_of_years_transition_spatiotemp_f > 9 & n_transition_spatiotemp_f/no_of_years_transition_spatiotemp_f >9, 2, 3)))

summary_emu_transition_spatiotemp_m <- individual_emu_transition_spatiotemp_m %>% 
  group_by(ser_emu_nameshort) %>% 
  summarize(n_transition_spatiotemp_m = length(unique(mei_fi_id)),
            no_of_years_transition_spatiotemp_m = length(unique(fi_year)),
            min_year_transition_spatiotemp_m = min(fi_year),
            max_year_transition_spatiotemp_m = max(fi_year)) %>% 
  mutate(quality_transition_spatiotemp_m = ifelse(no_of_years_transition_spatiotemp_m > 14 & n_transition_spatiotemp_m/no_of_years_transition_spatiotemp_m >9, 1,
                                                  ifelse(no_of_years_transition_spatiotemp_m > 9 & n_transition_spatiotemp_m/no_of_years_transition_spatiotemp_m >9, 2, 3)))


# 5.2 Spatial-only analyses data ####

#create a temporary dataframe where for a emu there are both yellow and silver eel available
temp_transition_emu_spatial_f <- individual_emu_spatial_f %>% 
  group_by(ser_emu_nameshort) %>% 
  summarize(n_yellow_f = sum((lfs_code == "Y" & !is.na(lengthmm))),
            n_silver_f = sum((lfs_code == "S" & !is.na(lengthmm)))) %>% 
  filter(n_yellow_f > 0 & !is.na(n_yellow_f) & n_silver_f > 0 & !is.na(n_silver_f))

temp_transition_emu_spatial_m <- individual_emu_spatial_m %>% 
  group_by(ser_emu_nameshort) %>% 
  summarize(n_yellow_m = sum((lfs_code == "Y")),
            n_silver_m = sum((lfs_code == "S"))) %>% 
  filter(n_yellow_m > 0 & !is.na(n_yellow_m) & n_silver_m > 0 & !is.na(n_silver_m))

#create a dataframe containing only individual data of eel where for a given combination of emu/year both yellow and silver length are available
individual_emu_transition_spatial_f <- individual_emu_spatial_f %>% 
  filter(ser_emu_nameshort %in% temp_transition_emu_spatial_f$ser_emu_nameshort)

individual_emu_transition_spatial_m <- individual_emu_spatial_m %>% 
  filter(ser_emu_nameshort %in% temp_transition_emu_spatial_m$ser_emu_nameshort)

#create a dataframe summarizing data availability for emu/year combinations with length of both, yellow and silver eel
summary_emu_transition_spatial_f <- individual_emu_transition_spatial_f %>% 
  group_by(ser_emu_nameshort) %>% 
  summarize(n_transition_Y_spatial_f = sum((lfs_code == "Y")),
            n_transition_S_spatial_f = sum((lfs_code == "S")),
            no_of_years_transition_spatial_f = length(unique(fi_year)),
            min_year_transition_spatial_f = min(fi_year),
            max_year_transition_spatial_f = max(fi_year)) %>% 
  mutate(quality_transition_spatial_f = ifelse(n_transition_Y_spatial_f >29 & n_transition_S_spatial_f >29, 1,
                                               ifelse(n_transition_Y_spatial_f >9 & n_transition_S_spatial_f > 9, 2, 3)))

summary_emu_transition_spatial_m <- individual_emu_transition_spatial_m %>% 
  group_by(ser_emu_nameshort) %>% 
  summarize(n_transition_Y_spatial_m = sum((lfs_code == "Y")),
            n_transition_S_spatial_m = sum((lfs_code == "S")),
            no_of_years_transition_spatial_m = length(unique(fi_year)),
            min_year_transition_spatial_m = min(fi_year),
            max_year_transition_spatial_m = max(fi_year)) %>% 
  mutate(quality_transition_spatial_m = ifelse(n_transition_Y_spatial_m >29 & n_transition_S_spatial_m >49, 1,
                                               ifelse(n_transition_Y_spatial_m >9 & n_transition_S_spatial_m > 9, 2, 3)))




# 6. SILVER-LENGTH: Collate data that are suitable for analyses of silver eel length (by emu)  #############

# 6.1 Spatiotemporal analyses data ####

#create a dataframe individual data of silver eels where length is available
individual_emu_silverlength_spatiotemp_f <- individual_emu_spatiotemp_f %>%
  filter(lfs_code == "S" & !is.na(lengthmm) & !is.na(fi_year)) 

individual_emu_silverlength_spatiotemp_m <- individual_emu_spatiotemp_m %>%
  filter(lfs_code == "S" & !is.na(lengthmm) & !is.na(fi_year)) 

#create a dataframe summarizing data availability of silver eels where length is available
summary_emu_silverlength_spatiotemp_f <- individual_emu_silverlength_spatiotemp_f %>% 
  group_by(ser_emu_nameshort) %>% 
  summarize(n_silverlength_spatiotemp_f = length(unique(mei_fi_id)),
            no_of_years_silverlength_spatiotemp_f = length(unique(fi_year)),
            min_year_silverlength_spatiotemp_f = min(fi_year),
            max_year_silverlength_spatiotemp_f = max(fi_year)) %>% 
  mutate(quality_silverlength_spatiotemp_f = ifelse(no_of_years_silverlength_spatiotemp_f > 14 & n_silverlength_spatiotemp_f/no_of_years_silverlength_spatiotemp_f >9, 1,
                                                    ifelse(no_of_years_silverlength_spatiotemp_f > 9 & n_silverlength_spatiotemp_f/no_of_years_silverlength_spatiotemp_f >9, 2, 3)))

summary_emu_silverlength_spatiotemp_m <- individual_emu_silverlength_spatiotemp_m %>% 
  group_by(ser_emu_nameshort) %>% 
  summarize(n_silverlength_spatiotemp_m = length(unique(mei_fi_id)),
            no_of_years_silverlength_spatiotemp_m = length(unique(fi_year)),
            min_year_silverlength_spatiotemp_m = min(fi_year),
            max_year_silverlength_spatiotemp_m = max(fi_year)) %>% 
  mutate(quality_silverlength_spatiotemp_m = ifelse(no_of_years_silverlength_spatiotemp_m > 14 & n_silverlength_spatiotemp_m/no_of_years_silverlength_spatiotemp_m >9, 1,
                                                    ifelse(no_of_years_silverlength_spatiotemp_m > 9 & n_silverlength_spatiotemp_m/no_of_years_silverlength_spatiotemp_m >9, 2, 3)))


# 6.2 Spatial-only analyses data ####

#create a dataframe individual data of silver eels where length is available
individual_emu_silverlength_spatial_f <- individual_emu_spatial_f %>%
  filter(lfs_code == "S")  

individual_emu_silverlength_spatial_m <- individual_emu_spatial_m %>%
  filter(lfs_code == "S") 

#create a dataframe summarizing data availability of silver eels where length is available
summary_emu_silverlength_spatial_f <- individual_emu_silverlength_spatial_f %>% 
  group_by(ser_emu_nameshort) %>% 
  summarize(n_silverlength_spatial_f = length(unique(mei_fi_id)),
            no_of_years_silverlength_spatial_f = length(unique(fi_year)),
            min_year_silverlength_spatial_f = min(fi_year),
            max_year_silverlength_spatial_f = max(fi_year)) %>% 
  mutate(quality_silverlength_spatial_f = ifelse(n_silverlength_spatial_f >49, 1,
                                                 ifelse(n_silverlength_spatial_f >24, 2, 3)))

summary_emu_silverlength_spatial_m <- individual_emu_silverlength_spatial_m %>% 
  group_by(ser_emu_nameshort) %>% 
  summarize(n_silverlength_spatial_m = length(unique(mei_fi_id)),
            no_of_years_silverlength_spatial_m = length(unique(fi_year)),
            min_year_silverlength_spatial_m = min(fi_year),
            max_year_silverlength_spatial_m = max(fi_year)) %>% 
  mutate(quality_silverlength_spatial_m = ifelse(n_silverlength_spatial_m >49, 1,
                                                 ifelse(n_silverlength_spatial_m >24, 2, 3)))




# 7. EXPORT: Collate data,  save and export summarized and individual datasets ##################

# summarize spatiotemporal analyses data in one table
summary_emu_spatiotemp <- summary_emu_spatiotemp %>%
  left_join(summary_emu_spatiotemp_f, by ="ser_emu_nameshort") %>%
  left_join(summary_emu_spatiotemp_m, by ="ser_emu_nameshort") %>%
  left_join(summary_emu_growth_spatiotemp_f, by ="ser_emu_nameshort") %>% 
  left_join(summary_emu_growth_spatiotemp_m, by ="ser_emu_nameshort") %>%
  left_join(summary_emu_transition_spatiotemp_f, by ="ser_emu_nameshort") %>% 
  left_join(summary_emu_transition_spatiotemp_m, by ="ser_emu_nameshort") %>%
  left_join(summary_emu_silverlength_spatiotemp_f, by ="ser_emu_nameshort") %>%
  left_join(summary_emu_silverlength_spatiotemp_m, by ="ser_emu_nameshort") 

# summarize spatial analyses data in one table
summary_emu_spatial <- summary_emu_spatial %>%
  left_join(summary_emu_spatial_f, by ="ser_emu_nameshort") %>%
  left_join(summary_emu_spatial_m, by ="ser_emu_nameshort") %>%
  left_join(summary_emu_growth_spatial_f, by ="ser_emu_nameshort") %>%
  left_join(summary_emu_growth_spatial_m, by ="ser_emu_nameshort") %>%
  left_join(summary_emu_transition_spatial_f, by ="ser_emu_nameshort") %>%
  left_join(summary_emu_transition_spatial_m, by ="ser_emu_nameshort") %>%
  left_join(summary_emu_silverlength_spatial_f, by ="ser_emu_nameshort") %>%
  left_join(summary_emu_silverlength_spatial_m, by ="ser_emu_nameshort")

#add quality to individual emu 
individual_emu_spatiotemp_quality <- individual_cleaned %>% 
  left_join(summary_emu_spatiotemp %>% select(ser_emu_nameshort, quality_growth_spatiotemp_f, quality_growth_spatiotemp_m, 
                                               quality_transition_spatiotemp_f, quality_transition_spatiotemp_m, 
                                               quality_silverlength_spatiotemp_f, quality_silverlength_spatiotemp_m), by = "ser_emu_nameshort") %>% 
  mutate(quality_growth_spatiotemp_f = ifelse(mei_fi_id %in% individual_emu_growth_spatiotemp_f$mei_fi_id, quality_growth_spatiotemp_f, NA),
         quality_growth_spatiotemp_m = ifelse(mei_fi_id %in% individual_emu_growth_spatiotemp_m$mei_fi_id, quality_growth_spatiotemp_m, NA),
         quality_transition_spatiotemp_f = ifelse(mei_fi_id %in% individual_emu_transition_spatiotemp_f$mei_fi_id, quality_transition_spatiotemp_f, NA),
         quality_transition_spatiotemp_m = ifelse(mei_fi_id %in% individual_emu_transition_spatiotemp_m$mei_fi_id, quality_transition_spatiotemp_m, NA),
         quality_silverlength_spatiotemp_f = ifelse(mei_fi_id %in% individual_emu_silverlength_spatiotemp_f$mei_fi_id, quality_silverlength_spatiotemp_f, NA),
         quality_silverlength_spatiotemp_m = ifelse(mei_fi_id %in% individual_emu_silverlength_spatiotemp_m$mei_fi_id, quality_silverlength_spatiotemp_m, NA))

#add quality to individual emu spatial
individual_emu_spatial_quality <- individual_cleaned_spatial %>% 
  left_join(summary_emu_spatial %>% select(ser_emu_nameshort, quality_growth_spatial_f, quality_growth_spatial_m, 
                                            quality_transition_spatial_f, quality_transition_spatial_m, 
                                            quality_silverlength_spatial_f, quality_silverlength_spatial_m), by = "ser_emu_nameshort") %>% 
  mutate(quality_growth_spatial_f = ifelse(mei_fi_id %in% individual_emu_growth_spatial_f$mei_fi_id, quality_growth_spatial_f, NA),
         quality_growth_spatial_m = ifelse(mei_fi_id %in% individual_emu_growth_spatial_m$mei_fi_id, quality_growth_spatial_m, NA),
         quality_transition_spatial_f = ifelse(mei_fi_id %in% individual_emu_transition_spatial_f$mei_fi_id, quality_transition_spatial_f, NA),
         quality_transition_spatial_m = ifelse(mei_fi_id %in% individual_emu_transition_spatial_m$mei_fi_id, quality_transition_spatial_m, NA),
         quality_silverlength_spatial_f = ifelse(mei_fi_id %in% individual_emu_silverlength_spatial_f$mei_fi_id, quality_silverlength_spatial_f, NA),
         quality_silverlength_spatial_m = ifelse(mei_fi_id %in% individual_emu_silverlength_spatial_m$mei_fi_id, quality_silverlength_spatial_m, NA))

#load emu shapefile & attatch to summaries
EMUs <- read_sf("utility/emu_shapefile/emu.shp")
summary_emu_spatiotemp_sf <- EMUs %>% left_join(summary_emu_spatiotemp, by = c("emu_namesh" = "ser_emu_nameshort"))
summary_emu_spatial_sf <- EMUs %>% left_join(summary_emu_spatial, by = c("emu_namesh" = "ser_emu_nameshort"))

#save summary by emu for spatiotemporal data
save(summary_emu_spatiotemp, file = "data/LHTemu/summary_emu_spatiotemp.RData")
save(summary_emu_spatiotemp_sf, file = "data/LHTemu/summary_emu_spatiotemp_sf.RData")

#save summary by emu for spatial data
save(summary_emu_spatial, file = "data/LHTemu/summary_emu_spatial.RData")
save(summary_emu_spatial_sf, file = "data/LHTemu/summary_emu_spatial_sf.RData")

#save individual data alongside quality
save(individual_emu_spatiotemp_quality, file = "data/LHTemu/individual_emu_spatiotemp_quality.RData")

#save individual spatial only data alongside quality
save(individual_emu_spatial_quality, file = "data/LHTemu/individual_emu_spatial_quality.RData")

