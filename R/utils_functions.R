# ============================================================
# Utils_functions.R

# Función para calcular distancias y tiempos caminando entre un bloque de pacientes
# y todos los centros disponibles mediante la API de matrices de OpenRouteService.

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


procesar_bloque_matrix <- function(df_pacientes_bloque, max_intentos = 5) {
  
  # Combina en una única matriz las coordenadas de pacientes y centros.    
  
  coordenadas <- rbind(
    unname(as.matrix(df_pacientes_bloque[, c("lon_paciente", "lat_paciente")])),
    unname(as.matrix(centros_unicos[, c("lon_centro", "lat_centro")]))
  )
  
  n_pacientes_bloque <- nrow(df_pacientes_bloque)
  
  # Índices de origen: corresponden a las filas de pacientes dentro de la matriz
  # de coordenadas. Se resta 1 porque la API usa indexación 0-based,
  # mientras que R usa indexación 1-based. 
  
  idx_sources <- (1:n_pacientes_bloque) - 1
  
  # Índices de destino: corresponden a las filas de centros dentro de la misma
  # matriz de coordenadas. Como los centros se añadieron después de los pacientes,
  # sus posiciones empiezan justo después de n_pacientes_bloque.
  # También se resta 1 para adaptar la indexación a la API.
  
  idx_destinations <- ((n_pacientes_bloque + 1):(n_pacientes_bloque + n_centros)) - 1
  
  intento <- 1
  res <- NULL
  
  # Bucle de reintentos. Se repite hasta obtener una respuesta válida
  # o hasta alcanzar el número máximo de intentos.  
  
  while (intento <= max_intentos) {
    
    message(
      "Intento ", intento, "/", max_intentos,
      " para bloque de ", n_pacientes_bloque, " pacientes"
    )
    # Llamada a la API de matriz de OpenRouteService.
    # tryCatch evita que un error puntual interrumpa todo el procesamiento.  
    res <- tryCatch(
      {
        ors_matrix(
          locations = coordenadas,
          sources = idx_sources,
          destinations = idx_destinations,
          profile = "foot-walking",
          metrics = c("duration", "distance"),
          output = "parsed"
        )
      },
      error = function(e) {
        message("Error en intento ", intento, ": ", conditionMessage(e))
        return(NULL)
      }
    )
    
    # Si la respuesta existe y contiene distancias, se considera válida
    # y se sale del bucle de reintentos.
    
    if (!is.null(res) && !is.null(res$distances)) {
      break
    }
    
    # Espera progresiva antes del siguiente intento.
    # Por ejemplo: 5, 10, 15, 20... segundos.
    # Esto reduce la probabilidad de nuevos bloqueos o errores temporales.
    
    Sys.sleep(5 * intento)  # espera progresiva: 5, 10, 15, 20...
    intento <- intento + 1
  }
  
  # Si después de todos los intentos no hay respuesta válida,
  # se devuelve un data.frame vacío para que el proceso global pueda continuar.
  
  if (is.null(res) || is.null(res$distances)) {
    message("Bloque fallido tras ", max_intentos, " intentos")
    return(data.frame())
  }
  
  # Construye todas las combinaciones paciente-centro del bloque
  # y añade las distancias y tiempos devueltos por la API.
  
  expand.grid(
    id_ruta = df_pacientes_bloque$id_ruta,
    id_centro = centros_unicos$id_centro
  ) %>%
    mutate(
      distancia_caminando_m = as.vector(res$distances),
      tiempo_caminando_min = as.vector(res$durations) / 60
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
  
