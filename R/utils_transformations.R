# Funciones de Transformación, Limpieza y Categorización

# Transformador Barthel
apply_barthel <- function(x) {
  cut(x,
    breaks = c(-Inf, 19, 35, 55, 99, Inf),
    labels = c("Total (<20)", "Severe (20-35)", "Moderate (40-55)", "Low (60-99)", "None (100)")
  )
}

# Transformador TIRS
apply_TIRS <- function(x) {
  case_when(x>0~"si",
            x==0~"no",
            TRUE~NA_character_)
}

# Transformador MACA
apply_MACA <-  function(x) {
  factor(ifelse(is.na(x), "no", "si"))
}

# Transformador PCC
apply_PCC <- function(x) {
  factor(ifelse(is.na(x), "no", "si"))
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
    df$IN_FECAL == 1 & df$IN_URINARIA == 1 ~ "Fecal and urinary",
    df$IN_FECAL == 1 ~ "Fecal",
    df$IN_URINARIA == 1 ~ "Urinary",
    TRUE ~ "None/None registered"
  )
  factor(res, levels = c("Fecal and urinary", "Fecal", "Urinary", "None/None registered"))
}

# Transformador Lógica (Viu sol / Adeq llar)
apply_logic_cat <- function(x, target_name) {
  if (target_name == "living_alone") {
  x <- if_else(is.na(x),"No viu sol/a",as.character(x))
  return(factor(x, levels = c("No viu sol/a", "Viu sol/a"), labels = c("No", "Yes")))
  }
  if (target_name == "need_household_adapt") {
  x <- if_else(is.na(x),"No precisa cap de les millores descrites",as.character(x))
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
      "GMA_groups"    = as.factor(val),
      "barthel"       = apply_barthel(val),
      "pfeiffer"      = apply_pfeiffer(val),
      "PCC"           = apply_PCC(val),
      "GMA_CODE"      = numeric(val),
      "MACA"          = apply_MACA(val),
      "TIRS_cat"      = factor(apply_TIRS(val)),
      "TIRS"          = numeric(val),
      "gijon"         = factor(ifelse(val > 11, "Yes", "No")),
      "logic_cat"     = apply_logic_cat(val, row$target_var),
      "percentage"    = val * 100,
      "PHC_name"      = apply_uab_mapping(val, "name"),
      "Single_PHC"             = apply_uab_mapping(val, "org1"),
      "Home_based_PHC_org"    = apply_uab_mapping(val, "org2"),
      "Home_Ambulances"        = ifelse(is.na(val), 0, val), # Si Na, 0
      "ED_visits"              = ifelse(is.na(val), 0, val), # Si Na, 0
      "PC_Emergency_unit"      = ifelse(is.na(val), 0, val), # Si Na, 0
      "Hospital_Admissions"    = ifelse(is.na(val), 0, val), # Si Na, 0,
      "Time_follow_up_ambulance"           = ifelse(is.na(val), 407, val), # Si Na, 407
      "Time_Follow_Up_PC_Emergency_unit"   = ifelse(is.na(val), 407, val), # Si Na, 407
      "Time_follow_up_emergency_d"         = ifelse(is.na(val), 407, val), # Si Na, 407
      "Time_follow_up_hospital_admission"  = ifelse(is.na(val), 407, val), # Si Na, 407
      
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


# descriptiva enfermedades ------------------------------------------------

get_disease_summary <- function(data, start_var, end_var) {
  data %>%
    as_tibble() |>
    select({{ start_var }}:{{ end_var }}) |>
    # Conversión masiva: maneja factores y asegura tipo numérico
    mutate(across(everything(), ~ as.numeric(as.character(.x)))) |>
    pivot_longer(
      everything(),
      names_to = "Variable",
      values_to = "Valor"
    ) |>
    group_by(Variable) |>
    summarise(
      N = sum(Valor == 1, na.rm = TRUE),
      pct = round((N / n()) * 100, 1),
      .groups = "drop"
    ) |>
    arrange(desc(N))
}


# categorizacion 2 y 3 variables ------------------------------------------

cat3 <- function(x) factor(
  dplyr::case_when(
    is.na(x) ~ "0",
    x == 0   ~ "0",
    x == 1   ~ "1",
    x >= 2   ~ "2+"
  ),
  levels = c("0","1","2+")
)

cat2 <- function(x) factor(
  dplyr::case_when(
    is.na(x) ~ "0",
    x == 0   ~ "0",
    x >= 1   ~ "1+"
  ),
  levels = c("0","1+")
)
