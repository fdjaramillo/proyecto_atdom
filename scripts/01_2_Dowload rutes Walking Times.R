# ============================================================
# 01_2_Dowload rutes Walking Times.R
# ============================================================

source(here("scripts", "00_0 setup.R"))

Patients_centre_locations<-readRDS(here("data", "SF", "pacients_adreces_i_centre_sf_Data_Table.rds"))

ors_api_key("eyJvcmciOiI1YjNjZTM1OTc4NTExMTAwMDFjZjYyNDgiLCJpZCI6ImM2YjdlNGExN2M1Y2Q0NzBmNDg5MTZlZmYxYzFkYzUwYjdhZjllYTI2MDVlMTc4NzNmNTQ1MDAyIiwiaCI6Im11cm11cjY0In0=")

#Opcional
ors_api_key("eyJvcmciOiI1YjNjZTM1OTc4NTExMTAwMDFjZjYyNDgiLCJpZCI6IjJlZGJkMGY4N2JjOTQzNWNiZGJkODgyNDVkMTU2MmVjIiwiaCI6Im11cm11cjY0In0=")

#Hint: solo admite 4326 (lat/longitud)

pacients_sf <- Patients_centre_locations %>%
  st_as_sf(
    coords = c("geo_epgs_4326_lon","geo_epgs_4326_lat"),
    crs = 4326,
    remove = FALSE
  )

centros_sf <- Patients_centre_locations %>%
  st_as_sf(
    coords = c("geo_epgs_centro_4326_lon","geo_epgs_centro_4326_lat"),
    crs = 4326,
    remove = FALSE
  )

pacients_utm <- st_transform(pacients_sf, 25831)
centros_utm  <- st_transform(centros_sf,  25831)

Patients_centre_locations$distancia_recta_m <- as.numeric(
  st_distance(
    pacients_utm,
    centros_utm,
    by_element = TRUE
  )
)

# 1. Extraer coordenadas únicas y asignarles un ID temporal

rutas_unicas <- Patients_centre_locations %>%
  distinct(
    geo_epgs_4326_lon, geo_epgs_4326_lat,
    geo_epgs_centro_4326_lon, geo_epgs_centro_4326_lat,
    Centre_ID
  ) %>%
  mutate(id_ruta = row_number())

# 2. Asignar id_ruta a cada paciente
paciente_ruta <- Patients_centre_locations %>%
  select(
    ID, Centre_ID,
    geo_epgs_4326_lon, geo_epgs_4326_lat,
    geo_epgs_centro_4326_lon, geo_epgs_centro_4326_lat
  ) %>%
  left_join(
    rutas_unicas,
    by = c(
      "geo_epgs_4326_lon", "geo_epgs_4326_lat",
      "geo_epgs_centro_4326_lon", "geo_epgs_centro_4326_lat",
      "Centre_ID"
    )
  )

# 2. Calcular tamaño del bloque dinámicamente según los centros

tamano_bloque <- 40

rutas_bloques <- rutas_unicas %>%
  mutate(bloque = (row_number() - 1) %/% tamano_bloque) %>%
  group_split(bloque)

#### Llamada a ruta. ####

# Procesar con guardado parcial
dir.create("resultados_bloques", showWarnings = FALSE)

walk(seq_along(rutas_bloques), function(i) {
  f <- sprintf("resultados_bloques/bloque_%03d.rds", i)
  if (file.exists(f)) { message("Bloque ", i, " ya existe."); return(invisible(NULL)) }
  
  Sys.sleep(5)
  res <- procesar_bloque_matrix(rutas_bloques[[i]])
  
  if (nrow(res) > 0 && !all(is.na(res$distancia_caminando_m))) {
    saveRDS(res, f)
    message("Bloque ", i, " guardado (", nrow(res), " filas).")
  }
})

# Unir bloques
resultados_matrix <- list.files("resultados_bloques", "\\.rds$", full.names = TRUE) %>%
  map(readRDS) %>%
  list_rbind()

saveRDS(resultados_matrix,here("data", "processed", "rutas_distancia.rds"))


# Recomponer el dataframe de rutas

paciente_ruta_con_dist <- paciente_ruta %>%
  left_join(resultados_matrix, by = "id_ruta")%>%
  select(ID,
         Centre_ID,
         id_ruta,
         distancia_caminando_m,
         tiempo_caminando_min
         )%>%
  mutate(tiempo_caminando_hms=as_hms(round(tiempo_caminando_min * 60)))


saveRDS(paciente_ruta_con_dist,here("data", "processed", "paciente_ruta_distancia.rds"))

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


