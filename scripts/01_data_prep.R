# setup -------------------------------------------------------------------

source(here("scripts", "00_setup.R"))

# Carga de datos raw
load("data/DF_work.RData")
load("data/DF_work2.RData")


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
dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)
saveRDS(df, "data/processed/df_cleaned.rds")
message("Dataset guardado exitosamente en data/processed/df_cleaned.rds")

# sf data -----------------------------------------------------------------

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

BCN_adreces<-BCN_adreces %>%
  mutate(
    USUA_NUMERO = as.integer(str_remove(numpost_i, "^0+"))
  )

User_adreces<- read.csv2(here("data", "external", "USER_adreces_original.csv"),
                         stringsAsFactors = FALSE
)


BCN_adreces_users_SF <- BCN_adreces  %>%
  inner_join(
    User_adreces[,1:3],
    by = c(
      "nom_carrer" = "USUA_CARRER",
      "USUA_NUMERO" = "USUA_NUMERO"
    )
  )

saveRDS(
  BCN_adreces_users_SF,
  here("data", "processed", "adreces_SF.rds")
)

# data renta media --------------------------------------------------------

Renta_media<- read.csv2(here("data", "external", "renta_media_hogar.csv"),
                         stringsAsFactors = FALSE
)

Renta_media <- Renta_media %>%
  mutate(
    Seccion.Censal = str_pad(
      as.character(Seccion.Censal),
      width = 3,
      side = "left",
      pad = "0"
    ),
    Distrito = str_pad(
      as.character(Distrito),
      width = 2,
      side = "left",
      pad = "0"
  )
)  

BCN_adreces_users__renda_SF <- BCN_adreces_users_SF  %>%
  inner_join(
    Renta_media[,4:5],
    by = c(
      "secc_cens" = "Seccion.Censal"
    )
  )

BCN_adreces_users__renda_SF <- BCN_adreces_users_SF %>%
  left_join(
    Renta_media,
    by = c(
      "districte" = "Distrito",
      "secc_cens" = "Seccion.Censal"
    )
  )

saveRDS(
  BCN_adreces_users__renda_SF,
  here("data", "processed", "adreces_SF_Renda.rds")
)

# centres -----------------------------------------------------------------

# Coordenades Centres

Centres_adreces <- read.csv(
  "data/external/centres.csv",
  fileEncoding = "UTF-16LE"
)

Centres_adreces_sf <- st_as_sf(
  Centres_adreces,
  coords = c("geo_epgs_25831_x", "geo_epgs_25831_y"),
  crs = 25831,
  remove = FALSE
)

saveRDS(
  Centres_adreces_sf,
  here("data", "external", "Centres_adreces_sf.rds")
)


Centres_estudi_adreces_sf <- Centres_adreces_sf%>%
  filter(name %in% c("Centre d'Atenció Primària Comte Borrell",
                     "Centre d'Atenció Primària Ernest Lluch",
                     "Centre d'Atenció Primària Casanova",
                     "Centre d'Atenció Primària Montnegre",
                     "Centre d'Atenció Primària Adrià")
                    )

saveRDS(
  Centres_estudi_adreces_sf,
  here("data", "external", "Centres_estudi_adreces_sf.rds")
)

#Pacients a centre amb dades SF per a routes.

Patients_locations <- Patients_locations %>%
  st_drop_geometry() %>%
  transmute(
    ID = ID,
    codi_carrer = codi_carrer,
    nom_carrer = nom_carrer,
    numero_Carrer = numpost_i,
    secc_censal = secc_cens,
    districte = districte,
    barri = barri,
    nom_barri = nom_barri,
    lon_paciente = as.numeric(longitud_wgs84),
    lat_paciente = as.numeric(latitud_wgs84)
  )

Center_location <- Center_location %>%
  st_drop_geometry() %>%
  transmute(
    Centre_ID = register_id,
    Nom_centre = name,
    codi_carrer = addresses_road_id,
    nom_carrer = addresses_road_name,
    numero_Carrer = addresses_start_street_number,
    districte_centre_id = addresses_district_id,
    districte = addresses_district_name,
    barri_centre_id = addresses_neighborhood_id,
    barri = addresses_neighborhood_name,
    CP_centre = addresses_zip_code,
    x_centro_etrs89 = as.numeric(geo_epgs_25831_x),
    y_centro_etrs89 = as.numeric(geo_epgs_25831_y),
    lon_centro = as.numeric(geo_epgs_4326_lon),
    lat_centro = as.numeric(geo_epgs_4326_lat)
  )

df_pacients<- readRDS("data/processed/df_cleaned.rds")

df_pacients<-df_pacients%>%
  select(ID,USUA_UAB_UP)%>%
  mutate(Centre_ID= case_when(USUA_UAB_UP=="Borrell"~ "99400282464",
                              USUA_UAB_UP=="Casanova"~ "92086002684",
                              USUA_UAB_UP=="Montnegre_1"~ "93056132443",
                              USUA_UAB_UP=="Montnegre_2"~ "93056132443",
                              USUA_UAB_UP=="Marc_Aureli"~ "92086002931",
                              USUA_UAB_UP=="Sant_Elies"~ "92086002931",
                              USUA_UAB_UP=="Lluch"~ "94354121938"))


Center_location <- Center_location %>%
  mutate(
    Centre_ID = str_replace_all(Centre_ID, "[^0-9]", ""))

Patients_locations <- Patients_locations %>%
  left_join(
    df_pacients %>% select(ID, Centre_ID),
    by = "ID") %>%
  left_join(
    Center_location %>%
      select(Centre_ID, Nom_centre, lon_centro, lat_centro),
    by = "Centre_ID"
  )

saveRDS(
  Patients_locations,
  here("data", "processed", "pacients_adreces_i_centre_sf.rds")
)

