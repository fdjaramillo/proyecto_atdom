# Carga explícita de dependencias
library(tidyverse)
library(compareGroups)
source("R/utils_transformations.R") # Requerido para cat2 y cat3

if (!file.exists("data/processed/df_cleaned.rds")) {
  stop("El archivo df_cleaned.rds no existe. Ejecuta primero scripts/01_data_prep.R")
}
df <- readRDS("data/processed/df_cleaned.rds")

### Descript

# compare groups ----------------------------------------------------------

method <- c(
  DOMICILI_INF_TOT = 2,
  TOTAL_VISITS_INF = 2,
  coc_nurse = 2,
  DOMICILI_MF_TOT = 2,
  TOTAL_VISITS_MF = 2,
  coc_physician = 2,
  DOMICILI_CONJ = 2,
  TOTAL_VISITS_CONJF = 2,
  coc_conj = 2,
  SEM_num = 2,
  emergency_visits= 2,
  INGRES_num= 2
)

# Descritiva todos los pa. Atdom

descriptiva <- descrTable(
  ~ . - ID,
  data = df,
  method = method,
  max.xlev = 25,
  hide.no = "no",
  include.miss = T,
  extra.labels = c("", "", "", "")
)
export2md(descriptiva, format = "html")

# Descritiva en función PHC center

descriptiva_strat_1 <- descrTable(
  USUA_UAB_UP ~ . - ID - organit_atdom_1 - organit_atdom_2,
  data = df,
  max.ylev = 7,
  max.xlev = 25,
  show.all = T,
  chisq.test.perm = T,
  method = method,
  hide.no = "no",
  include.miss = T,
  extra.labels = c("", "", "", "")
)
export2md(descriptiva_strat_1, format = "html")

# Descritiva en función Equip_Atdom, Equip_Inf	y  UAB_consulta

descriptiva_strat_2 <- descrTable(
  organit_atdom_1 ~ . - ID - USUA_UAB_UP - organit_atdom_2,
  data = df,
  show.all = T,
  chisq.test.perm = T,
  method = method,
  hide.no = "no",
  include.miss = T,
  extra.labels = c("", "", "", "")
)
export2md(descriptiva_strat_2, format = "html")

# Descritiva Equip_Atdom, Equip_Inf,  UAB_consulta y UAB_consulta_reforç

descriptiva_strat_3 <- descrTable(
  organit_atdom_2 ~ . - ID - USUA_UAB_UP - organit_atdom_1,
  data = df,
  show.all = T,
  chisq.test.perm = T,
  method = method,
  hide.no = "no",
  include.miss = T,
  extra.labels = c("", "", "", "")
)
export2md(descriptiva_strat_3, format = "html")


# new ---------------------------------------------------------------------

descriptiva_strat_2_2 <- descrTable(
  organit_atdom_1 ~ SEM_num + emergency_visits + INGRES_num,
  data = df,
  method = method,
  show.all = T,
  chisq.test.perm = T,
  hide.no = "no",
  include.miss = T,
  extra.labels = c("", "", "", "")
)
export2md(descriptiva_strat_2_2, format = "html")

descriptiva_strat_3_3 <- descrTable(
  organit_atdom_2 ~ SEM_num + emergency_visits + INGRES_num,
  data = df,
  method = method,
  show.all = T,
  chisq.test.perm = T,
  hide.no = "no",
  include.miss = T,
  extra.labels = c("", "", "", "")
)
export2md(descriptiva_strat_3_3, format = "html")


# categorizacion ----------------------------------------------------------

vars <- c("SEM_num", "emergency_visits", "INGRES_num","Exitus")

df <- df |> 
  mutate(
    across(all_of(vars), cat3, .names = "{.col}_cat3"),
    across(all_of(vars), cat2, .names = "{.col}_cat2")
  )

descriptiva_strat_2_2_cat <- descrTable(
  organit_atdom_1 ~ SEM_num_cat2 + emergency_visits_cat2 + INGRES_num_cat2 +
    SEM_num_cat3 + emergency_visits_cat3 + INGRES_num_cat3,
  data = df,
  method = method,
  show.all = T,
  chisq.test.perm = T,
  hide.no = "no",
  include.miss = T,
  extra.labels = c("", "", "", "")
)
export2md(descriptiva_strat_2_2_cat, format = "html")

descriptiva_strat_3_3_cat <- descrTable(
  organit_atdom_2 ~ SEM_num+SEM_num_cat2 + emergency_visits+emergency_visits_cat2 + INGRES_num+INGRES_num_cat2
  +Exitus,
  data = df,
  method = method,
  show.all = T,
  chisq.test.perm = T,
  hide.no = "no",
  include.miss = T,
  extra.labels = c("", "", "", "")
)
export2md(descriptiva_strat_3_3_cat, format = "html")
