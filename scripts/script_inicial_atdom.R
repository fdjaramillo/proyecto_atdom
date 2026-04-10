library(tidyverse)
library(compareGroups)
library(labelled)

# load data
load("data/DF_work.RData")
DF_work <- DF_work |> as_tibble()

# inspeccion inicial ------------------------------------------------------

DF_work |> 
  select(!(Ansiedad:TDA)) |> str()

DF_work |> 
  select(ID, DOMICILI_INF_TOT, TELEFON_INF_TOT, TOTAL_VISITS_INF, UPC_TOTAL_INF)



# prepare dataframe for descriptive table ---------------------------------

vars_tabla <- c(
  "USUA_SEXE", "EDAT", "TIME_FOLLOW_UP_TOTAL", "ESTRATGMA", 
  "BARTHEL", "PFEIFFER", "IN_URINARIA", "IN_FECAL", "GIJON", 
  "DOMICILI_INF_TOT", "TOTAL_VISITS_INF", "UPC_TOTAL_INF", 
  "DOMICILI_MF_TOT", "TOTAL_VISITS_MF", "UPC_TOTAL_MF"
)

descriptiva_inicial <- DF_work |> 
  select(ID, any_of(vars_tabla)) |> 
  mutate(
    # Sexo femenino
    sex_female = factor(ifelse(USUA_SEXE == "D", "Yes", "No")),
    
    # Años en el programa (asumiendo que TIME_FOLLOW_UP_TOTAL son días)
    years_home_based = TIME_FOLLOW_UP_TOTAL / 365.25,
    
    # Alta comorbilidad (Estratos 3 y 4 de GMA)
    high_comorbidity = factor(ifelse(ESTRATGMA %in% c(3, 4), "Yes", "No")),
    
    # Índice de Barthel
    barthel_cat = cut(
      BARTHEL,
      breaks = c(-Inf, 19, 35, 55, 99, Inf),
      labels = c("Total (<20)", "Severe (20-35)", "Moderate (40-55)", "Low (60-99)", "None (100)"),
    ),
    
    # Pfeiffer
    pfeiffer_cat = cut(
      PFEIFFER,
      breaks = c(-Inf, 2, 4, 7, Inf),
      labels = c("No deficit (0-2 mistakes)", "Low (3-4 mistakes)", "Moderate (5-7 mistakes)", "Severe (8-10 mistakes)"),
    ),
    
    # Incontinencia
    incontinence_cat = case_when(
      IN_URINARIA == 1 & IN_FECAL == 1 ~ "Fecal and urinary",
      IN_URINARIA == 0 & IN_FECAL == 0 ~ "None",
      IN_URINARIA == 1                 ~ "Urinary", # Captura 1 & 0, y 1 & NA
      IN_FECAL == 1                    ~ "Fecal",   # Captura 0 & 1, y NA & 1
      TRUE                             ~ NA_character_ # Captura 0 & NA, NA & 0, y NA & NA
      # que pasa con los que tienen NA en alguna de las dos variables?
    ),
    
    # Convertir a factor directamente con los niveles finales
    incontinence_cat = factor(
      incontinence_cat, 
      levels = c("Fecal and urinary", "Fecal", "Urinary", "None")
    ),
    
    # Riesgo social GIJON > 11 ??
    social_risk = factor(ifelse(GIJON > 11, "Yes", "No")),
    
    # Continuidad asistencial (CoC) a porcentajes
    coc_nurse = UPC_TOTAL_INF * 100,
    coc_physician = UPC_TOTAL_MF * 100
  )

descriptiva_inicial <- descriptiva_inicial |> 
  select(!c(
    USUA_SEXE, TIME_FOLLOW_UP_TOTAL, ESTRATGMA, BARTHEL, PFEIFFER, 
    IN_URINARIA, IN_FECAL, GIJON, 
    UPC_TOTAL_INF, UPC_TOTAL_MF
  )) |> 
  relocate(c(DOMICILI_INF_TOT, TOTAL_VISITS_INF), .before = coc_nurse) |> 
  relocate(c(DOMICILI_MF_TOT, TOTAL_VISITS_MF), .before = coc_physician)

# generate descriptive table ----------------------------------------------


# poner etiquetas para la tabla final
etiquetas <- list(
  sex_female       = "Sex, female, No. (%)",
  EDAT             = "Age, y, mean (SD)",
  years_home_based = "Years in home-based program, mean (SD)",
  high_comorbidity = "High comorbidity status (AMG index ≥p85), No. (%)",
  barthel_cat      = "Dependency according to Barthel Index for Activities of Daily Living, No. (%)",
  pfeiffer_cat     = "Short Portable Mental Status Questionnaire, No. (%)",
  incontinence_cat = "Incontinence, No. (%)",
  social_risk      = "Social risk, No. (%)",
  DOMICILI_INF_TOT = "Home-based visits, mean (SD)",
  TOTAL_VISITS_INF = "All visits (telephone and come based), mean (SD)",
  coc_nurse        = "CoC-assigned PHC nurse, % (SD)",
  DOMICILI_MF_TOT  = "Home-based visits, mean (SD)",
  TOTAL_VISITS_MF  = "All visits (telephone and come based), mean (SD)",
  coc_physician    = "CoC-assigned physician, % (SD)"
)

# Aplicar a todo el dataframe
var_label(descriptiva_inicial) <- etiquetas


# Compare groups
res <- compareGroups(~ . -ID, data = descriptiva_inicial)

estimador <- createTable(res, hide.no = "no", sd.type = 1)
estimador_mx <- estimador[["descr"]]

conf_int <- createTable(
  res, hide.no = "no", sd.type = 1, show.n = FALSE, show.ci = TRUE
  )
conf_int_mx <- conf_int[["descr"]]
conf_int_mx[, 1] <- conf_int_mx[, 1] |> 
  sub(pattern = ".+\\[", replacement = "") |> 
  sub(pattern = "\\]$", replacement = "") |> 
  sub(pattern = ";", replacement = " - ")
  

matrix_final <- cbind(estimador_mx, conf_int_mx)

estimador[["descr"]] <- matrix_final

# faltan estas variables para la tabla descriptiva inicial
# No. registered chronic diseases GMA_N_CRONIQUES
# Living alone VIU_SOL
# Need for any household adaptation ADEQ_LLAR


# exportar a word ---------------------------------------------------------
dir.create("results", recursive = TRUE)
export2word(estimador, file = file.path(getwd(), "results", "Table1_descriptiva.docx"))
