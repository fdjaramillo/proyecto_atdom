# ============================================================
# 02_4 Modelling access.R
# ============================================================

source(here("scripts", "00_0 setup.R"))

#Data load

U_cens <- readRDS(
  here("data", "SF", "unitats_censals_estudi_sf.rds"))%>%
  st_drop_geometry()

Data_population_UC <- readRDS(
  here("data", "SF", "unitats_censals_estudi_sf.rds"))

Data_centres_UC <- readRDS(
  here("data", "SF", "Centres_estudi_SF.rds"))%>%
  st_drop_geometry()
