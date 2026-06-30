# Validaciones de Datos

validate_input_data <- function(df, dict) {
  # Identificar variables requeridas y presentes
  required_vars <- unique(dict$orig_var)
  present_vars <- colnames(df)

  # Detectar ausencias
  missing_vars <- setdiff(required_vars, present_vars)

  if (length(missing_vars) > 0) {
    # Cruzar con el diccionario para ver cuáles son críticas
    missing_info <- dict |>
      filter(orig_var %in% missing_vars) |>
      select(orig_var, critical) |>
      distinct()

    critical_missing <- missing_info |>
      filter(critical == TRUE) |>
      pull(orig_var)
    optional_missing <- missing_info |>
      filter(critical == FALSE) |>
      pull(orig_var)

    # Error fatal si falta alguna crítica
    if (length(critical_missing) > 0) {
      stop(paste(
        "\n[ERROR FATAL] Faltan variables críticas obligatorias:",
        paste(critical_missing, collapse = ", ")
      ))
    }

    # Warning si faltan opcionales
    if (length(optional_missing) > 0) {
      warning(paste(
        "\n[AVISO] Las siguientes variables opcionales no están presentes y se omitirán:",
        paste(optional_missing, collapse = ", ")
      ))
    }
  } else {
    message("Validación exitosa: Todas las variables necesarias están presentes.")
  }
}
