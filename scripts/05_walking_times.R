# ============================================================
# 06_Walking_times_Analysis.R
# ============================================================
source(here("scripts", "00_setup.R"))

Patients_locations<-readRDS(here("data","processed","pacients_adreces_i_centre_sf.rds"))

ors_api_key("eyJvcmciOiI1YjNjZTM1OTc4NTExMTAwMDFjZjYyNDgiLCJpZCI6IjgzMjA5MjM5MDM4YjRlNzVhZWEyNjgzNmZlYTJjNzFkIiwiaCI6Im11cm11cjY0In0=")


pacientes_sf <- Patients_locations %>%
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
    pacientes_sf,
    centros_sf,
    by_element = TRUE
  )
)

saveRDS(
  Patients_locations,
  here("data", "processed", "Patients_locations.rds")
)

saveRDS(
  centros_sf,
  here("data", "processed", "centros_sf.rds")
)


##Més llunyans

Patients_locations %>%
  arrange(desc(distancia_recta_m)) %>%
  select(
    ID,
    Nom_centre,
    nom_carrer,
    numero_Carrer,
    lon_paciente,
    lat_paciente,
    lon_centro,
    lat_centro,
    distancia_recta_m
  ) %>%
  head(20)

Rutes_uniques <-Patients_locations %>%
  filter(
    !is.na(lon_paciente),
    !is.na(lat_paciente),
    !is.na(lon_centro),
    !is.na(lat_centro)
  ) %>%
  distinct(
    lon_paciente,
    lat_paciente,
    lon_centro,
    lat_centro
  ) %>%
  mutate(
    ruta_id = row_number()
  )

##pacients i rutes
nrow(Patients_locations)
nrow(Rutes_uniques)


ruta_check <- ors_directions(
  coordinates = list(
    c(Rutes_uniques$lon_paciente[2], Rutes_uniques$lat_paciente[2]),
    c(Rutes_uniques$lon_centro[2], Rutes_uniques$lat_centro[2])
  ),
  profile = "foot-walking",
  output = "parsed"
)

ruta_check$features[[1]]$properties$summary$distance
ruta_check$features[[1]]$properties$summary$duration / 60

## Cálcul rutes
dim(Rutes_uniques)

Rutes <- pmap(
  list(
    Rutes_uniques$lon_paciente,
    Rutes_uniques$lat_paciente,
    Rutes_uniques$lon_centro,
    Rutes_uniques$lat_centro
  ),
  calcular_ruta_ors
)

#Unir rutes
Rutes_uniques <- bind_cols(
  Rutes_uniques,
  Rutes
)

## Summary
head(Rutes_uniques)
summary(Rutes_uniques$tiempo_caminando_min)


Patients_locations_rutas <- Patients_locations %>%
  left_join(
    Rutes_uniques,
    by = c(
      "lon_paciente",
      "lat_paciente",
      "lon_centro",
      "lat_centro"
    )
  )

Patients_locations_rutas %>%
  select(
    ID,
    Nom_centre,
    nom_carrer,
    numero_Carrer,
    distancia_recta_m,
    distancia_caminando_m,
    tiempo_caminando_min
  ) %>%
  head()