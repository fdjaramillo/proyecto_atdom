# ============================================================
# 06_Walking_times_Analysis.R
# ============================================================
source(here("scripts", "00_setup.R"))

Patients_locations<-readRDS(here("data","processed","adreces_SF.rds"))
Center_location<-readRDS(here("data","external","Centres_estudi_adreces_sf.rds"))


ors_api_key("eyJvcmciOiI1YjNjZTM1OTc4NTExMTAwMDFjZjYyNDgiLCJpZCI6IjgzMjA5MjM5MDM4YjRlNzVhZWEyNjgzNmZlYTJjNzFkIiwiaCI6Im11cm11cjY0In0=")