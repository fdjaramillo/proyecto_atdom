# setup -------------------------------------------------------------------

source(here("scripts", "00_setup.R"))

# Carga de datos raw
load(here("data", "DF_work.RData"))
load(here("data", "DF_work2.RData"))


# Descriptiva enfermedades ------------------------------------------------
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

# Mapeo de centros de salud externalizado
# 1. Cargar la configuración uab
conf_centros_uab <- config::get("centros_uab")

# 2. Convertir la sección a un Data Frame
df_centros_uab <- purrr::map_dfr(conf_centros_uab, as.data.frame)

# 3. Unir con el dataset principal
df <- df |> left_join(df_centros_uab, by = "USUA_UAB_UP")

df <- apply_all_transformations(df, metadata_dict) # transformar
df <- set_names_to_df(df, metadata_dict) # poner etiquetas

# Guardar dataset procesado de forma explícita
dir.create(here("data", "processed"), showWarnings = FALSE, recursive = TRUE)
saveRDS(df, here("data", "processed", "df_cleaned.rds"))
message("Dataset guardado exitosamente en data/processed/df_cleaned.rds")

# sf data -----------------------------------------------------------------

BCN_adreces<- read_csv(here("data", "adreces.csv"))

BCN_adreces<- BCN_adreces %>%
  filter(
    !is.na(nom_carrer),
    !is.na(numpost_i)
  ) |>
  # seleccionar columnas que aportan valor
  select(
    nom_carrer, numpost_i,              # Las llaves
    nom_districte, nom_barri,           # Las zonas
    dist_post,                          # El código postal
    latitud_wgs84, longitud_wgs84       # Las coordenadas GPS
  ) |>
  distinct() |>
  mutate(
    # factor: nom_carrer, nom_districte, nom_barri
    across(c(nom_carrer, nom_districte, nom_barri), as.factor),
    # integer: numpost_i, dist_post
    across(c(numpost_i, dist_post), as.integer),
    # double: latitud_wgs84, longitud_wgs84
    across(c(latitud_wgs84, longitud_wgs84), as.double)
  )


BCN_adreces <- BCN_adreces |>
  arrange(nom_carrer, numpost_i, latitud_wgs84, longitud_wgs84) |>
  group_by(nom_carrer, numpost_i) |>
  slice(1) |>
  ungroup()


User_adreces<- read_csv2(here("data", "external", "USER_adreces_original.csv"))




# data renta media --------------------------------------------------------

Renta_media<- read_csv2(here("data", "external", "renta_media_hogar.csv"))




# centres -----------------------------------------------------------------

# Coordenades Centres

Centres_adreces <- read_csv(
  here("data", "external", "centres.csv"),
  locale = locale(encoding = "UTF-16LE")
)

Centres_adreces <- Centres_adreces %>%
  mutate(
    # eliminar caracteres invisibles
    # comprobar ejecutando charToRaw(Centres_adreces$register_id[1]) y charToRaw("92086002201")
    register_id = str_replace_all(register_id, "[^0-9]", "")
  )



Centres_estudi_adreces <- Centres_adreces %>%
  filter(register_id %in% df_centros_uab$id_centres_csv)






