library(tidyverse)
library(compareGroups)
library(labelled)

# Cargar el diccionario de metadatos desde csv
metadata_dict <- read_csv("metadata_dict.csv")

# Cargar funciones de ayuda
source("scripts/utils_functions.R")

# Carga de datos
load("data/DF_work.RData")
load("data/DF_work2.RData")

df <- DF_work |>
  as_tibble() |>
  left_join(DF_work_2 |> select(ID, C_GMA_N_CRONIQUES, VC_VIU_SOL_VALOR, VC_ADEQ_LLAR_VALOR), by = "ID")

# Flujo
df <- apply_all_transformations(df, metadata_dict) # transformar
df <- set_names_to_df(df, metadata_dict) # poner etiquetas


# compare groups ----------------------------------------------------------

method <- c(
  DOMICILI_INF_TOT = 2,
  TOTAL_VISITS_INF = 2,
  coc_nurse = 2,
  DOMICILI_MF_TOT = 2,
  TOTAL_VISITS_MF = 2,
  coc_physician = 2
)

descriptiva <- descrTable(
  ~ . - ID,
  data = df,
  method = method,
  hide.no = "no",
  include.miss = T,
  extra.labels = c("", "", "", "")
)
export2md(descriptiva, format = "html")


# descriptiva_strat_1 <- descrTable(
#   USUA_UAB_UP ~ . -ID -organit_atdom_1 -organit_atdom_2,
#   data = df,
#   method = method,
#   hide.no = "no",
#   include.miss = T,
#   extra.labels = c("","","","")
# )
# export2md(descriptiva_strat_1, format="html")
# number of groups must be less or equal to 5


descriptiva_strat_2 <- descrTable(
  organit_atdom_1 ~ . - ID - USUA_UAB_UP - organit_atdom_2,
  data = df,
  method = method,
  hide.no = "no",
  include.miss = T,
  extra.labels = c("", "", "", "")
)
export2md(descriptiva_strat_2, format = "html")


descriptiva_strat_3 <- descrTable(
  organit_atdom_2 ~ . - ID - USUA_UAB_UP - organit_atdom_1,
  data = df,
  method = method,
  hide.no = "no",
  include.miss = T,
  extra.labels = c("", "", "", "")
)
export2md(descriptiva_strat_3, format = "html")
