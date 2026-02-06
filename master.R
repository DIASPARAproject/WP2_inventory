############################ IMPORTANT ###########################

# There are two initial datasets, before corrections were applied, and after. By default, the scripts are set up to run on the most recent dataset.
# IF you want to run the scripts on the dataset before any corrections were provided by data providers, you need to go to the script 
# "boot/compilation.R" and change the loaded files in lines 24 and 26 to biometry_sampling_before_corr.RData  & biometry_series_before_corr.RData



# 1. PREPARATION TO RUN SCRIPT #################################################  

# define libraries needed
libs <- c("icesTAF", "rmarkdown")

#define libraries already installed
installed_libs <- libs %in% rownames(installed.packages())

# install libraries that are not installed already
if (any(installed_libs == F)) {
  install.packages(libs[!installed_libs])
}

# load libraries needed
invisible(lapply(libs, library, character.only = T))


# set working directory and run taf.skeleton to create folder structure
taf.skeleton()


# 2. CREATE METADATA (.bib file) ###############################################

#create metadata for script
draft.data(data.scripts = "compilation_base.R",
           data.files = NULL,
           originator = "WGEEL",
           title = "Individual eel data pulled from WGEEL database as of 01-16-2025, individual biometric data (time series & sampling series combined)",
           file = TRUE,
           period = "x-2025",
           access = "Restricted",
           append = F
)

draft.data(data.scripts = "compilation.R",
           data.files = NULL,
           originator = "WGEEL",
           title = "Individual eel data pulled from WGEEL database as of 02-04-2026, individual biometric data (time series & sampling series combined)",
           file = TRUE,
           period = "x-2025",
           access = "Restricted",
           append = T
)

# 3. IMPORT DATA FROM ABOVE TO BOOTSTRAP/DATA FOLDER ###########################

# bring all in DATA.bib to the bootstrap/data folder (from "initial/data"). Existing data will not be overwritten(? it re-downloads... Also, files not in DATA.bib that are already in data folder will be deleted!)! Delete those where an update is required!
taf.boot(software = FALSE) 

#create output.rmd
#if (!file.exists("output_inventory.rmd")) {
#  file.create("output_inventory.rmd")
#} else {
#  message("output_inventory.rmd already exists")
#}

#create data_issues.rmd
#if (!file.exists("data_issues.rmd")) {
#  file.create("data_issues.rmd")
#} else {
#  message("data_issues.rmd already exists")
#}


#--------------------------------------------------------------------------------#
#####----- RUN SCRIPT FROM HERE IF YOU DO NOT WANT TO IMPORT DATA AGAIN -----#####
#--------------------------------------------------------------------------------#

# 4. (RE)LOAD LIBRARIES ########################################################

# define libraries needed
libs <- c("icesTAF", "rmarkdown")

#define libraries already installed
installed_libs <- libs %in% rownames(installed.packages())

# install libraries that are not installed already
if (any(installed_libs == F)) {
  install.packages(libs[!installed_libs])
}

# load libraries needed
invisible(lapply(libs, library, character.only = T))




# 5. SOURCE SCRIPTS  ########################################################### 

# remove all folders (including what's in them, that were created by the scripts below)
clean()

# delete everything from the R environment
rm(list = ls())

# run the scripts
source("data.R")
  rmarkdown::render('data_issues_report_base.Rmd', output_file = "data/issues/issues_report_base.html")
  source("data_call.R")
  #source("data_call_response.R")
  rmarkdown::render('data_issues_report.Rmd', output_file = "data/issues/issues_report.html")
  #source("data_LHTsite.R")
  #source("data_LHTemu.R")
#source("model.R")
source("output.R")
  rmarkdown::render('output_inventory.Rmd', output_file = "output/inventory/inventory.docx")
  #rmarkdown::render('output_overview.Rmd', output_file = "output/overview/overview.docx")
#source("report.R")

