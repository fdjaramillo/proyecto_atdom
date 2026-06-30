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
source("scripts/utils_functions.R")

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
