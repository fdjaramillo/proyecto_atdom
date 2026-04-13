library(tidyverse)
library(compareGroups)
library(labelled)

# load data
load("data/DF_work.RData")
DF_work <- DF_work |> as_tibble()

# load data 2
load("data/DF_work2.RData")
DF_work_2 <- DF_work_2 |> select(ID, C_GMA_N_CRONIQUES, VC_VIU_SOL_VALOR, VC_ADEQ_LLAR_VALOR)

# join
DF_work <- DF_work |> left_join(DF_work_2, by = "ID")

# inspeccion inicial ------------------------------------------------------

DF_work |> 
  select(!(Ansiedad:TDA)) |> str()

DF_work |> 
  select(ID, DOMICILI_INF_TOT, TELEFON_INF_TOT, TOTAL_VISITS_INF, UPC_TOTAL_INF)



# prepare dataframe for descriptive table ---------------------------------

vars_tabla <- c(
  "ATDOM", "USUA_SEXE", "EDAT", "TIME_FOLLOW_UP_TOTAL", "ESTRATGMA", 
  "C_GMA_N_CRONIQUES", "BARTHEL", "PFEIFFER", "IN_URINARIA", "IN_FECAL", 
  "GIJON", "VC_VIU_SOL_VALOR", "VC_ADEQ_LLAR_VALOR",
  "DOMICILI_INF_TOT", "TOTAL_VISITS_INF", "UPC_TOTAL_INF", 
  "DOMICILI_MF_TOT", "TOTAL_VISITS_MF", "UPC_TOTAL_MF"
)

descriptiva_inicial <- DF_work |> 
  select(ID, any_of(vars_tabla)) |> 
  mutate(
    # Sexo femenino
    sex_female = factor(ifelse(USUA_SEXE == "D", "Yes", "No")),
    
    # Años en el programa (asumiendo que TIME_FOLLOW_UP_TOTAL son días)
    years_home_based = as.numeric((as.Date("2024-12-16") - ATDOM) / 365.25),
    
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
    
    # living alone
    living_alone = factor(
      VC_VIU_SOL_VALOR,
      levels = c("No viu sol/a", "Viu sol/a"),
      labels = c("No", "Yes")
    ),
    
    # need house hold adaptation
    need_household_adapt = factor(
      VC_ADEQ_LLAR_VALOR,
      levels = c("Cal actuar en alguna de les millores descrites", "No precisa cap de les millores descrites"),
      labels = c("Yes", "No")
    ),
    
    # Continuidad asistencial (CoC) a porcentajes
    coc_nurse = UPC_TOTAL_INF * 100,
    coc_physician = UPC_TOTAL_MF * 100
  )

descriptiva_inicial <- descriptiva_inicial |> 
  select(!c(
    ATDOM, USUA_SEXE, TIME_FOLLOW_UP_TOTAL, ESTRATGMA, BARTHEL, PFEIFFER, 
    IN_URINARIA, IN_FECAL, GIJON, VC_VIU_SOL_VALOR, VC_ADEQ_LLAR_VALOR,
    UPC_TOTAL_INF, UPC_TOTAL_MF
  )) |> 
  relocate(C_GMA_N_CRONIQUES, .after = high_comorbidity) |> 
  relocate(c(DOMICILI_INF_TOT, TOTAL_VISITS_INF), .before = coc_nurse) |> 
  relocate(c(DOMICILI_MF_TOT, TOTAL_VISITS_MF), .before = coc_physician)

# generate descriptive table ----------------------------------------------


# poner etiquetas para la tabla final
etiquetas <- list(
  sex_female           = "Sex, female",
  EDAT                 = "Age, y",
  years_home_based     = "Years in home-based program",
  high_comorbidity     = "High comorbidity status (AMG index ≥p85)",
  C_GMA_N_CRONIQUES    = "No. registered chronic diseases",
  barthel_cat          = "Dependency according to Barthel Index for Activities of Daily Living",
  pfeiffer_cat         = "Short Portable Mental Status Questionnaire",
  incontinence_cat     = "Incontinence",
  social_risk          = "Social risk",
  living_alone         = "Living alone",
  need_household_adapt = "Need for any household adaptation",
  DOMICILI_INF_TOT     = "PHC Nurse - Home-based visits",
  TOTAL_VISITS_INF     = "PHC Nurse - All visits (telephone and home based)",
  coc_nurse            = "PHC Nurse - CoC-assigned",
  DOMICILI_MF_TOT      = "GP - Home-based visits",
  TOTAL_VISITS_MF      = "GP - All visits (telephone and home based)",
  coc_physician        = "GP - CoC-assigned"
)

# Aplicar a todo el dataframe
var_label(descriptiva_inicial) <- etiquetas


# Compare groups
method <- c(
  DOMICILI_INF_TOT = 2,
  TOTAL_VISITS_INF = 2,
  coc_nurse = 2,
  DOMICILI_MF_TOT = 2,
  TOTAL_VISITS_MF = 2,
  coc_physician = 2
)

table <- descrTable(
  ~ . -ID, 
  data = descriptiva_inicial,
  method = method,
  hide.no = "no",
  include.miss = T,
  extra.labels = c("","","","")
  )

export2md(table,format="html")

