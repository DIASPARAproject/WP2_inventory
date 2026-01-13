
library(RPostgres)
library(sf)
library(getPass)
library(ggforce)
library(ggplot2)
library(flextable)
library(tidyverse)
library(yaml)

cred=read_yaml("boot/initial/data/credentials.yml")

con_wgeel = dbConnect(Postgres(), dbname=cred$dbname,host=cred$host,port=cred$port,user=cred$user, password=cred$password)



## download data from the database

biometry_individual_series=dbGetQuery(con_wgeel,"with sel as(SELECT d.*, t_series.*, mty_name FROM datawg.t_metricindseries_meiser d, datawg.t_fishseries_fiser t_series, ref.tr_metrictype_mty WHERE mty_id=mei_mty_id AND mei_fi_id = fi_id AND (mei_qal_id IS NULL OR mei_qal_id IN (1,2,4)))

       select sel.*, ser_nameshort, ser_cou_code, ser_x,ser_y,ser_emu_nameshort, ser_hty_code,ser_lfs_code,gea_name_en, sam_samplingtype,t_series_ser.ser_qal_id from sel, datawg.t_series_ser, ref.tr_gear_gea, ref.tr_samplingtype_sam where ser_id=fiser_ser_id and ser_sam_gear=gea_id and ser_sam_id=sam_id")



biometry_individual_series<-biometry_individual_series %>% filter(fi_id!=421454)



biometry_individual_sampling=dbGetQuery(con_wgeel,"with sel as(SELECT d.*, t_samp.*, mty_name FROM datawg.t_metricindsamp_meisa d, datawg.t_fishsamp_fisa t_samp, ref.tr_metrictype_mty WHERE mty_id=mei_mty_id AND mei_fi_id = fi_id AND (mei_qal_id IS NULL OR mei_qal_id IN (1,2,4)))

       select * from sel, datawg.t_samplinginfo_sai where sai_id=fisa_sai_id")

getwd()

save(biometry_individual_sampling, file = "boot/initial/data/biometry_sampling.RData")
save(biometry_individual_series, file = "boot/initial/data/biometry_series.RData")
