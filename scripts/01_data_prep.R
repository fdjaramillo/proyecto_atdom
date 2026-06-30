library(tidyverse)
library(compareGroups)
library(labelled)
library(WeightIt)
library(cobalt)
library(broom)
library(purrr)

# Carga de datos raw
load("data/DF_work.RData")
load("data/DF_work2.RData")

# Cargar funciones de ayuda
source("R/utils_validation.R")
source("R/utils_transformations.R")

# descriptiva enfermedades ------------------------------------------------
patologias <- get_disease_summary(DF_work_2, `Abuso de sustancias`, VIH)

# Cargar el diccionario de metadatos desde csv
metadata_dict <- read_csv2("metadata_dict.csv") |>
  # eliminar filas con todo NA
  filter(if_any(everything(), ~ !is.na(.)))

# preparar datos para descirptiva -----------------------------------------

df <- DF_work |>
  as_tibble() |>
  left_join(DF_work_2 |> select(ID, C_GMA_N_CRONIQUES, VC_VIU_SOL_VALOR, VC_ADEQ_LLAR_VALOR,
                                C_GMA_COMPLEXITAT,PR_MACA_DATA,PR_PCC_DATA),
    by = "ID"
  )

# Flujo
validate_input_data(df, metadata_dict) # valida y lanza warnings/errors
df <- apply_all_transformations(df, metadata_dict) # transformar
df <- set_names_to_df(df, metadata_dict) # poner etiquetas

# Guardar dataset procesado de forma explícita
dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)
saveRDS(df, "data/processed/df_cleaned.rds")
message("Dataset guardado exitosamente en data/processed/df_cleaned.rds")

###SF DATA
library(sf)
Adreces<- "https://opendata-ajuntament.barcelona.cat/data/dataset/25752522-3528-4c14-b68d-5f09a3e393bd/resource/661fe190-67c8-423a-b8eb-8140f547fde2/download"

download.file(
  url = Adreces,
  destfile = "data/adreces.csv",
  mode = "wb"
)

BCN_adreces<- st_read("data/adreces.csv")

BCN_adreces<- BCN_adreces %>%
  mutate(
    x_etrs89 = na_if(x_etrs89, ""),
    y_etrs89 = na_if(y_etrs89, ""),
    x_etrs89 = as.numeric(x_etrs89),
    y_etrs89 = as.numeric(y_etrs89)
  ) %>%
  filter(
    !is.na(x_etrs89),
    !is.na(y_etrs89)
  )%>%
  st_as_sf(
    .,
    coords = c("x_etrs89", "y_etrs89"),
    crs = 25831,
    remove = FALSE
  )

BCN_adreces<-BCN_adreces %>%
  mutate(nom_carrer=toupper(nom_carrer))
