
# 1. Load Libraries ###########################################################################################

# define libraries needed
libs <- c("tidyverse", "icesTAF", "stringr") 

#define libraries already installed
installed_libs <- libs %in% rownames(installed.packages())

# install libraries that are not installed already
if (any(installed_libs == F)) {
  install.packages(libs[!installed_libs])
}

# load libraries needed
invisible(lapply(libs, library, character.only = T))




# 2. Load data & create some useful lists ####################################################################

#load data
load("../../initial/data/biometry_sampling_before_corr.RData") 
biometry_individual_sampling <- biometry_individual_sampling %>% mutate(source = "sampling")
load("../../initial/data/biometry_series_before_corr.RData")
biometry_individual_series <- biometry_individual_series %>% mutate(source = "series") #%>% rename(lfs_code_ser = fi_lfs_code, fi_lfs_code = ser_lfs_code)
#create some useful lists
headers_sampling <- colnames(biometry_individual_sampling)
variables_sampling <- unique(biometry_individual_sampling$mty_name)

headers_series <- colnames(biometry_individual_series)
variables_series <- unique(biometry_individual_series$mty_name)




# 3. Combine in one dataframe, transform to wide format and add SI ####################################################################

#rename columns
biometry_individual_sampling <- biometry_individual_sampling %>% 
  rename(fiser_ser_id = fisa_sai_id,
         ser_x = fisa_x_4326,
         ser_y = fisa_y_4326,
         ser_name = sai_name,
         ser_cou_code = sai_cou_code,
         ser_emu_nameshort  = sai_emu_nameshort,
         ser_hty_code = sai_hty_code,
         ser_qal_id = sai_qal_id) %>%
  select(-sai_id)

#stitch data frames together (introducing NAs in unique columns)
individual_raw <- bind_rows(biometry_individual_sampling, biometry_individual_series)

#turn to wide data, calculate SI and add columns for stage and sex 
individual_wide_base <- individual_raw %>% 
  select(mei_fi_id, fi_id_cou, mei_value, mty_name, fi_lfs_code, fi_year, fi_date, ser_x, ser_y, ser_cou_code, ser_nameshort, ser_emu_nameshort, fi_comment, 
         ser_hty_code, sai_comment, sai_samplingstrategy, sai_samplingobjective, ser_qal_id, source, sam_samplingtype, ser_lfs_code) %>% 
  pivot_wider(values_from = mei_value, names_from = mty_name) %>% 
  mutate(S1 = (-61.276+0.242*lengthmm+(-0.108*weightg)+5.546*eye_diam_meanmm+0.614*pectoral_lengthmm),
         S2 = (-87.995+0.286*lengthmm+(-0.125*weightg)+6.627*eye_diam_meanmm+0.838*pectoral_lengthmm),
         S3 = (-109.014+0.28*lengthmm+(-0.127*weightg)+9.108*eye_diam_meanmm+1.182*pectoral_lengthmm),
         S4 = (-113.556+0.218*lengthmm+(-0.103*weightg)+12.187*eye_diam_meanmm+1.23*pectoral_lengthmm),
         S5 = (-128.204+0.242*lengthmm+(-0.136*weightg)+12.504*eye_diam_meanmm+1.821*pectoral_lengthmm),
         S6 = (-84.672+0.176*lengthmm+(-0.116*weightg)+12.218*eye_diam_meanmm+1.295*pectoral_lengthmm),
         ser_emu_nameshort = str_replace_all(ser_emu_nameshort, "LT_total", "LT_Lith"),
         ser_lfs_code = ifelse(!is.na(ser_lfs_code), ser_lfs_code, fi_lfs_code)) 

# 4. SAVE DATA ####################################################################

#save as RData
save(individual_wide_base, file = "individual_wide_base.RData")
