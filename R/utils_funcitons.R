# ============================================================
# Utils_functions.R
# ============================================================

source(here("scripts", "00_setup.R"))

calcular_ruta_ors <- function(lon_origen, lat_origen, lon_destino, lat_destino) {
  
  resultado <- tryCatch(
    {
      ruta <- ors_directions(
        coordinates = list(
          c(lon_origen, lat_origen),
          c(lon_destino, lat_destino)
        ),
        profile = "foot-walking",
        output = "parsed"
      )
      
      tibble(
        distancia_caminando_m = ruta$features[[1]]$properties$summary$distance,
        tiempo_caminando_min = ruta$features[[1]]$properties$summary$duration / 60
      )
    },
    error = function(e) {
      tibble(
        distancia_caminando_m = NA_real_,
        tiempo_caminando_min = NA_real_
      )
    }
  )
  
  return(resultado)
}