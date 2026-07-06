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


# OPTIMIZACIÓN: remplazo de pmap
# 1. Extraer coordenadas únicas y asignarles un ID temporal
pacientes_unicos <- Patients_locations %>%
  distinct(lon_paciente, lat_paciente) %>%
  filter(!is.na(lon_paciente), !is.na(lat_paciente)) %>%
  mutate(id_paciente = row_number())

centros_unicos <- Patients_locations %>%
  distinct(lon_centro, lat_centro) %>%
  filter(!is.na(lon_centro), !is.na(lat_centro)) %>%
  mutate(id_centro = row_number())

# 2. Calcular tamaño del bloque dinámicamente según los centros
n_centros <- nrow(centros_unicos)
tamano_bloque <- 50 - n_centros  # Asegura no pasarnos del límite de 50 de la API

pacientes_unicos <- pacientes_unicos %>%
  mutate(bloque = (row_number() - 1) %/% tamano_bloque)

# 3. Función interna para consultar la Matrix API por cada bloque
procesar_bloque_matrix <- function(df_pacientes_bloque) {
  
  # Combinar coordenadas limpiando los nombres de columnas para evitar conflictos
  coordenadas <- rbind(
    unname(as.matrix(df_pacientes_bloque[, c("lon_paciente", "lat_paciente")])),
    unname(as.matrix(centros_unicos[, c("lon_centro", "lat_centro")]))
  )
  
  n_pacientes_bloque <- nrow(df_pacientes_bloque)
  
  # Corregido: Restamos 1 para convertir la indexación de R (1-based) a la de la API (0-based)
  idx_sources <- (1:n_pacientes_bloque) - 1
  idx_destinations <- ((n_pacientes_bloque + 1):(n_pacientes_bloque + n_centros)) - 1
  
  # Llamada masiva a la API de Matrices
  res <- ors_matrix(
    locations = coordenadas,
    sources = idx_sources,
    destinations = idx_destinations,
    profile = "foot-walking",
    metrics = c("duration", "distance"),
    output = "parsed"
  )
  
  if (is.null(res$distances)) return(data.frame())
  
  # Reestructurar las matrices resultantes
  expand.grid(
    id_paciente = df_pacientes_bloque$id_paciente,
    id_centro = centros_unicos$id_centro
  ) %>%
    mutate(
      distancia_caminando_m = as.vector(res$distances),
      tiempo_caminando_min = as.vector(res$durations) / 60  # ORS devuelve segundos
    )
}

# 4. Iterar por bloques con una pequeña pausa para respetar el Rate Limit por minuto
resultados_matrix <- pacientes_unicos %>%
  group_split(bloque) %>%
  map_df(~ {
    Sys.sleep(2) # Pausa para evitar bloqueos de la API
    procesar_bloque_matrix(.x)
  })

# 5. Recomponer el dataframe de rutas únicas
Rutes_uniques <- pacientes_unicos %>%
  left_join(resultados_matrix, by = "id_paciente") %>%
  left_join(centros_unicos, by = "id_centro") %>%
  select(lon_paciente, lat_paciente, lon_centro, lat_centro, distancia_caminando_m, tiempo_caminando_min)

# 6. Unir los resultados finales de vuelta a tu dataset maestro
Patients_locations_rutas <- Patients_locations %>%
  left_join(Rutes_uniques, by = c("lon_paciente", "lat_paciente", "lon_centro", "lat_centro"))


# categorizar renta media quintiles ---------------------------------------

adreces_SF_Renda <- readRDS(here("data", "processed", "adreces_SF_Renda.rds"))

Patients_locations_rutas <- Patients_locations_rutas |> 
  left_join(
    adreces_SF_Renda|> select(ID, Media_renta_Hogar),
    by = "ID"
  ) |> 
  mutate(
    categoria_renta = cut(
      Media_renta_Hogar,
      breaks = quantile(Media_renta_Hogar, probs = seq(0, 1, 0.2), na.rm = TRUE),
      include.lowest = TRUE,
      labels = c("Muy Baja", "Baja", "Media", "Alta", "Muy Alta")
    )
  )
