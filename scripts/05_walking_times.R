# ============================================================
# 06_Walking_times_Analysis.R
# ============================================================
source(here("scripts", "00_setup.R"))

Patients_locations<-readRDS(here("data","processed","pacients_adreces_i_centre_sf.rds"))


ors_api_key("eyJvcmciOiI1YjNjZTM1OTc4NTExMTAwMDFjZjYyNDgiLCJpZCI6IjgzMjA5MjM5MDM4YjRlNzVhZWEyNjgzNmZlYTJjNzFkIiwiaCI6Im11cm11cjY0In0=")

pacients_sf <- Patients_locations %>%
  st_as_sf(
    coords = c("lon_paciente", "lat_paciente"),
    crs = 4326,
    remove = FALSE
  )

centros_sf <- Patients_locations %>%
  st_as_sf(
    coords = c("lon_centro", "lat_centro"),
    crs = 4326,
    remove = FALSE
  )

Patients_locations$distancia_recta_m <- as.numeric(
  st_distance(
    pacients_sf,
    centros_sf,
    by_element = TRUE
  )
)

# 1. Extraer coordenadas únicas y asignarles un ID temporal

rutas_unicas <- Patients_locations %>%
  filter(!is.na(lon_paciente), !is.na(lat_paciente)) %>%
  distinct(lon_paciente, lat_paciente) %>%
  mutate(id_ruta = row_number())

paciente_ruta <- Patients_locations %>%
  filter(!is.na(lon_paciente), !is.na(lat_paciente)) %>%
  select(ID, lon_paciente, lat_paciente) %>%
  left_join(
    rutas_unicas,
    by = c("lon_paciente", "lat_paciente")
  )

centros_unicos <- Patients_locations %>%
  distinct(lon_centro, lat_centro) %>%
  filter(!is.na(lon_centro), !is.na(lat_centro)) %>%
  mutate(id_centro = row_number())

# 2. Calcular tamaño del bloque dinámicamente según los centros

n_centros <- nrow(centros_unicos)
tamano_bloque <- 30 - n_centros  # Asegura no pasarnos del límite de 50 de la API

rutas_unicas <- rutas_unicas %>%
  mutate(bloque = (row_number() - 1) %/% tamano_bloque)

  
#### Llamada a ruta. ####

resultados_matrix <- rutas_unicas  %>%
  group_split(bloque) %>%
  map_df(~ {
    Sys.sleep(3)
    procesar_bloque_matrix(.x)
  })

#Checks

resultados_matrix %>%
  summarise(
    n_total = n(),
    n_distancia_na = sum(is.na(distancia_caminando_m)),
    n_tiempo_na = sum(is.na(tiempo_caminando_min))
  )

# 5. Recomponer el dataframe de rutas

rutas_unicas 
str(resultados_matrix)
head(Patients_locations)

table(Patients_locations$nom_barri,Patients_locations$barri)

paciente_ruta_final <- Patients_locations %>%
  mutate(id_centro= case_when(
                              Nom_centre=="Centre d'Atenció Primària Ernest Lluch"~5,
                              Nom_centre=="Centre d'Atenció Primària Montnegre"~1,
                              Nom_centre=="Centre d'Atenció Primària Comte Borrell"~3,
                              Nom_centre=="Centre d'Atenció Primària Casanova"~4,
                              Nom_centre=="Centre d'Atenció Primària Adrià"~2))%>%
  left_join(
    paciente_ruta[,c(1,4)],
    by = "ID")%>%
  left_join(
    resultados_matrix,
    by = c("id_ruta","id_centro"))%>%
  select(
    ID,
    id_ruta,
    id_centro,
    Nom_centre,
    secc_censal,
    lon_paciente,
    lat_paciente,
    lon_centro,
    lat_centro,
    distancia_caminando_m,
    tiempo_caminando_min
  )

paciente_ruta_final %>%
  summarise(
    n_filas = n(),
    n_pacientes = n_distinct(ID),
    n_rutas = n_distinct(id_ruta),
    n_centros = n_distinct(id_centro),
    n_distancia_na = sum(is.na(distancia_caminando_m)),
    n_tiempo_na = sum(is.na(tiempo_caminando_min))
  )

saveRDS(paciente_ruta_final,here("data", "processed", "paciente_ruta_final.rds"))

# categorizar renta media quintiles ---------------------------------------

adreces_SF_Renda <- readRDS(here("data", "processed", "adreces_SF_Renda.rds"))

paciente_renta <- paciente_ruta_final |> 
  left_join(
    adreces_SF_Renda|> select(ID, Media_renta_Hogar),
    by = "ID"
  ) |> 
  mutate(
    categoria_renta = cut(
      Media_renta_Hogar,
      breaks = quantile(Media_renta_Hogar, probs = seq(0, 1, 0.2), na.rm = TRUE),
      include.lowest = TRUE,
      labels = c("1q", "2q", "3q", "4q", "5q")
    )
  )


