# capa de validacion ------------------------------------------------------

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

# transformaciones especificas --------------------------------------------

# Transformador Barthel
apply_barthel <- function(x) {
  cut(x,
    breaks = c(-Inf, 19, 35, 55, 99, Inf),
    labels = c("Total (<20)", "Severe (20-35)", "Moderate (40-55)", "Low (60-99)", "None (100)")
  )
}

# Transformador Pfeiffer
apply_pfeiffer <- function(x) {
  cut(x,
    breaks = c(-Inf, 2, 4, 7, Inf),
    labels = c("No deficit (0-2 mistakes)", "Low (3-4 mistakes)", "Moderate (5-7 mistakes)", "Severe (8-10 mistakes)")
  )
}

# Transformador Incontinencia
apply_incontinence <- function(df) {
  if (!all(c("IN_URINARIA", "IN_FECAL") %in% colnames(df))) {
    return(factor(NA))
  }

  res <- case_when(
    df$IN_URINARIA == 1 & df$IN_FECAL == 1 ~ "Fecal and urinary",
    df$IN_URINARIA == 0 & df$IN_FECAL == 0 ~ "None",
    df$IN_URINARIA == 1 ~ "Urinary",
    df$IN_FECAL == 1 ~ "Fecal",
    TRUE ~ NA_character_
  )
  factor(res, levels = c("Fecal and urinary", "Fecal", "Urinary", "None"))
}

# Transformador Lógica (Viu sol / Adeq llar)
apply_logic_cat <- function(x, target_name) {
  if (target_name == "living_alone") {
    return(factor(x, levels = c("No viu sol/a", "Viu sol/a"), labels = c("No", "Yes")))
  }
  if (target_name == "need_household_adapt") {
    return(factor(x, levels = c("Cal actuar en alguna de les millores descrites", "No precisa cap de les millores descrites"), labels = c("Yes", "No")))
  }
  return(x)
}

# transformador UAB
apply_uab_mapping <- function(x, output_type = "name") {
  # 1. Definir el factor base con etiquetas
  levels_up <- c("00460", "00462", "00474", "00475", "00477", "00478", "01004")
  labels_up <- c("Borrell", "Casanova", "Montnegre_1", "Montnegre_2", "Marc_Aureli", "Sant_Elies", "Lluch")

  f_up <- factor(x, levels = levels_up, labels = labels_up)

  # 2. Retornar según lo solicitado
  if (output_type == "name") {
    return(f_up)
  }

  if (output_type == "org1") {
    return(fct_collapse(
      f_up,
      "Equip_Atdom"         = c("Borrell", "Lluch"),
      "Equip_Inf"           = "Casanova",
      "UAB_consulta"        = c("Montnegre_1", "Montnegre_2", "Sant_Elies", "Marc_Aureli")
    ))
  }

  if (output_type == "org2") {
    return(fct_collapse(
      f_up,
      "Equip_Atdom"         = c("Borrell", "Lluch"),
      "Equip_Inf"           = "Casanova",
      "UAB_consulta"        = c("Montnegre_1", "Montnegre_2"),
      "UAB_consulta_reforc" = c("Sant_Elies", "Marc_Aureli")
    ))
  }
}

# orquestador -------------------------------------------------------------

apply_all_transformations <- function(df, dict) {
  df_trans <- df

  # 1. Transformaciones directas columna a columna
  for (i in 1:nrow(dict)) {
    row <- dict[i, ]
    if (!row$orig_var %in% colnames(df)) next

    val <- df[[row$orig_var]]

    df_trans[[row$target_var]] <- switch(row$type,
      "numeric"       = as.numeric(val),
      "factor_status" = factor(ifelse(val == "A", "Yes", "No")), # Específico status
      "factor_sex"    = factor(ifelse(val == "D", "Yes", "No")), # Específico sexo
      "date_diff"     = as.numeric((as.Date("2024-12-16") - val) / 365.25),
      "gma_strat"     = factor(ifelse(val %in% c(3, 4), "Yes", "No")),
      "barthel"       = apply_barthel(val),
      "pfeiffer"      = apply_pfeiffer(val),
      "gijon"         = factor(ifelse(val > 11, "Yes", "No")),
      "logic_cat"     = apply_logic_cat(val, row$target_var),
      "percentage"    = val * 100,
      "uab_name"      = apply_uab_mapping(val, "name"),
      "uab_org1"      = apply_uab_mapping(val, "org1"),
      "uab_org2"      = apply_uab_mapping(val, "org2"),
      df_trans[[row$target_var]] # Default: no tocar
    )
  }

  # 2. Casos especiales multivariable (Incontinencia)
  if ("incontinence" %in% dict$type) {
    df_trans$incontinence_cat <- apply_incontinence(df)
  }

  # 3. Limpieza final: solo columnas target presentes en dict
  final_vars <- unique(dict$target_var)
  df_trans <- df_trans |> select(any_of(final_vars))

  return(df_trans)
}


# setear nombres ----------------------------------------------------------

set_names_to_df <- function(df, dict) {
  # Filtramos el dict para quedarnos solo con las variables del df final
  labels_list <- dict |>
    filter(target_var %in% colnames(df)) |>
    select(target_var, label) |>
    distinct(target_var, .keep_all = TRUE)

  labels_vector <- setNames(as.list(labels_list$label), labels_list$target_var)
  var_label(df) <- labels_vector

  return(df)
}
