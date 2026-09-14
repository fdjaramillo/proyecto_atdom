# ============================================================
# Utils_functions.R

# Función para calcular distancias y tiempos caminando entre un bloque de pacientes
#   y todos los centros disponibles mediante la API de matrices de OpenRouteService.

# Argumentos:
#   df_pacientes_bloque: data.frame con los pacientes del bloque actual.
#                        Debe contener id_paciente, lon_paciente y lat_paciente.
#   max_intentos: número máximo de intentos si la llamada a la API falla.
#
# Requiere objetos externos definidos previamente:
#   centros_unicos: data.frame con id_centro, lon_centro y lat_centro.
#   n_centros: número total de centros incluidos en centros_unicos.
#
# Devuelve:
#   Un data.frame con todas las combinaciones paciente-centro del bloque,
#   incluyendo distancia caminando en metros y tiempo caminando en minutos.
#   Si la API falla tras todos los intentos, devuelve un data.frame vacío.

procesar_bloque_matrix <- function(df_rutas_bloque, max_intentos = 3) {
  
  n_rutas <- nrow(df_rutas_bloque)
  
  # Coordenadas: rutas (coords del paciente) + destinos (coords del centro)
  coordenadas <- rbind(
    unname(as.matrix(df_rutas_bloque[, c("geo_epgs_4326_lon", "geo_epgs_4326_lat")])),
    unname(as.matrix(df_rutas_bloque[, c("geo_epgs_centro_4326_lon", "geo_epgs_centro_4326_lat")]))
  )
  
  idx_sources      <- 0:(n_rutas - 1)
  idx_destinations <- n_rutas:(2 * n_rutas - 1)
  
  intento <- 1
  res <- NULL
  quota_agotada <- FALSE
  
  while (intento <= max_intentos) {
    message("Intento ", intento, "/", max_intentos,
            " para bloque de ", n_rutas, " rutas")
    
    quota_local <- FALSE
    
    res <- tryCatch(
      {
        ors_matrix(
          locations    = coordenadas,
          sources      = idx_sources,
          destinations = idx_destinations,
          profile      = "foot-walking",
          metrics      = c("duration", "distance"),
          output       = "parsed"
        )
      },
      error = function(e) {
        msg <- conditionMessage(e)
        if (grepl("403|Quota exceeded", msg)) {
          message("Quota agotada (403). Abortando reintentos.")
          quota_local <<- TRUE
          return(NULL)
        }
        message("Error en intento ", intento, ": ", msg)
        return(NULL)
      }
    )
    
    if (quota_local) { quota_agotada <- TRUE; break }
    if (!is.null(res) && !is.null(res$distances)) break
    
    Sys.sleep(5 * intento)
    intento <- intento + 1
  }
  
  if (is.null(res) || is.null(res$distances)) {
    if (quota_agotada) message("Bloque abortado: cuota agotada.")
    else message("Bloque fallido tras ", max_intentos, " intentos.")
    
    return(tibble(
      id_ruta               = df_rutas_bloque$id_ruta,
      distancia_caminando_m = NA_real_,
      tiempo_caminando_min  = NA_real_
    ))
  }
  
  tibble(
    id_ruta               = df_rutas_bloque$id_ruta,
    distancia_caminando_m = diag(as.matrix(res$distances)),
    tiempo_caminando_min  = diag(as.matrix(res$durations)) / 60
  )
}

##Normalizar carrer
normaliza_carrer <- function(x) {
  x %>%
    as.character() %>%
    str_to_upper() %>%
    stringi::stri_trans_general("Latin-ASCII") %>%   # quita acentos
    str_replace_all("\\?", "Ñ") %>%                  # arregla casos tipo IBA?EZ -> IBANEZ
    str_replace_all("·", ".") %>%
    str_replace_all("\\.", " ") %>%
    str_replace_all("-", " ") %>%
    str_replace_all("'", " ") %>%
    str_squish()
}
  
