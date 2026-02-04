
library(RPostgres)
library(sf)
library(getPass)
library(ggforce)
library(ggplot2)
library(flextable)
library(tidyverse)
library(yaml)

cred <- read_yaml("boot/initial/data/credentials.yml")

con_wgeel <- dbConnect(Postgres(), dbname=cred$dbname,host=cred$host,port=cred$port,user=cred$user, password=cred$password)



## download data from the database

biometry_individual_series=dbGetQuery(con_wgeel,
"WITH sel as(SELECT d.*, t_series.*, mty_name FROM
datawg.t_metricindseries_meiser d, 
datawg.t_fishseries_fiser t_series,
ref.tr_metrictype_mty 
WHERE mty_id=mei_mty_id 
AND mei_fi_id = fi_id 
AND (mei_qal_id IS NULL OR mei_qal_id IN (1,2,4)))
select sel.*, 
ser_nameshort, 
ser_cou_code, 
ser_x,
ser_y,
ser_emu_nameshort,
ser_hty_code,
CASE WHEN fi_lfs_code IS NULL THEN ser_lfs_code 
     WHEN fi_lfs_code IS NOT NULL AND fi_lfs_code IN ('YS', 'GY') THEN ser_lfs_code
     ELSE fi_lfs_code END AS ser_lfs_code,
gea_name_en, 
sam_samplingtype,
t_series_ser.ser_qal_id from sel, 
datawg.t_series_ser, 
ref.tr_gear_gea, 
ref.tr_samplingtype_sam where ser_id=fiser_ser_id
and ser_sam_gear=gea_id and ser_sam_id=sam_id;")



biometry_individual_series <- biometry_individual_series %>% filter(fi_id!=421454)



biometry_individual_sampling <- dbGetQuery(con_wgeel,
"WITH sel as(
SELECT d.*, 
t_samp.fi_id,
t_samp.fi_date,
CASE WHEN t_samp.fi_year IS NULL AND t_samp.fi_date IS NOT NULL THEN 
EXTRACT('YEAR' FROM fi_date) ELSE fi_year END AS fi_year,
t_samp.fi_comment,
t_samp.fi_lastupdate,
t_samp.fi_dts_datasource,
t_samp.fi_lfs_code,
t_samp.fisa_sai_id,
t_samp.fisa_x_4326,
t_samp.fisa_y_4326,
t_samp.fi_id_cou,
 mty_name FROM datawg.t_metricindsamp_meisa d,
datawg.t_fishsamp_fisa t_samp, 
ref.tr_metrictype_mty 
WHERE mty_id=mei_mty_id 
AND mei_fi_id = fi_id 
AND (mei_qal_id IS NULL OR mei_qal_id IN (1,2,4))
)
select * from sel, 
datawg.t_samplinginfo_sai 
where sai_id=fisa_sai_id")

getwd()
path_save <- "boot/initial/data/"
#path_save <- "C:/Users/cedric.briand/OneDrive - EPTB Vilaine/partage/diaspara"
save(biometry_individual_sampling, file = file.path(path_save,"biometry_sampling.RData"))
save(biometry_individual_series, file =  file.path(path_save,"biometry_series.RData"))
