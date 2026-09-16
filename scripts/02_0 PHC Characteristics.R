# ============================================================
# 02_0 PHC Characteristics.R
# ============================================================

source(here("scripts", "00_0 setup.R"))

############ Prep data_frame ###############

# Data

Central_res<- read.csv2(here("data","external","DADES_CENTRAL_RESULTATS_2024.csv"))

Central_res_filt<- Central_res %>%
  mutate(UP_ABS = case_when(Centre_Territori=="Barcelona 2-C"~"00460",
                            Centre_Territori=="Barcelona 4-A"~"00474",
                            Centre_Territori=="Barcelona 5-B"~"00478",
                            Centre_Territori=="Barcelona 4-C"~"01004",
                            Centre_Territori=="Barcelona 2-E"~"00462",
                            Centre_Territori=="Barcelona 4-B"~"00475",
                            Centre_Territori=="Barcelona 5-A"~"00477"),
         
         Home_based_PHC_org = case_when(Centre_Territori=="Barcelona 2-C"~"Equip_Atdom",
                                       Centre_Territori=="Barcelona 4-A"~"UAB_consulta",
                                       Centre_Territori=="Barcelona 5-B"~"UAB_consulta",
                                       Centre_Territori=="Barcelona 4-C"~"Equip_Atdom",
                                       Centre_Territori=="Barcelona 2-E"~"Equip_Inf",
                                       Centre_Territori=="Barcelona 4-B"~"UAB_consulta_reforc",
                                       Centre_Territori=="Barcelona 5-A"~"UAB_consulta_reforc")
         )%>%
  filter(Indicador %in% c(
                          # Demografía y posición socioeconómica
    "Població assignada",
    "Percentatge de persones ateses",
    "Població adulta major de 65 anys",
    "Percentatge de persones ateses amb nivell socioeconòmic molt baix",
    
                          # Carga de enfermedad / necesidad poblacional
    "Prevalença de demència",
    "Prevalença de Parkinson",
    "Prevalença d'insuficiència cardíaca",
    "Prevalença de persones amb diagnòstic de malaltia cardiovascular",
    "Prevalença de malaltia renal crònica",
    "Prevalença de diabetis mellitus tipus 2",
    "Percentatge de persones polimedicades amb més de 10 principis actius",
    "Taxa de mortalitat",
    
                          # Contexto de utilización de atención primaria
    "Percentatge de persones ateses",
    "Mitjana de visites a l'atenció primària i comunitària",
    
    # Contexto organizativo
    "Índex del proveïdor assistencial principal anual",
    "Accessibilitat a 5 dies"),
        Grup_edat %in% c(
                          "Total",
                          "65-74",
                          "75-84",
                          "85+"
                          ),
        Sexe == "Total"
  )

##Table construction

Central_table1 <- Central_res_filt %>%
  filter(
    Any == 2024,
    Sexe == "Total",
    
    (
      # Total + grupos de edad superiores
      (
        Indicador %in% c(
          "Població assignada",
          "Percentatge de persones ateses",
          "Percentatge de persones ateses amb nivell socioeconòmic molt baix",
          "Mitjana de visites a l'atenció primària i comunitària",
          "Taxa de mortalitat"
        ) &
          Grup_edat %in% c(
            "Total",
            "65-74",
            "75-84",
            "85+"
          )
      ) |
        
      # Solo Total
      (
        Indicador %in% c(
          "Índex del proveïdor assistencial principal anual",
          "Accessibilitat a 5 dies"
        ) &
          Grup_edat == "Total"
      ) |
        
      # Solo grupos de edad superiores
      (
        Indicador %in% c(
          "Prevalença de demència",
          "Prevalença de Parkinson",
          "Prevalença d'insuficiència cardíaca",
          "Prevalença de persones amb diagnòstic de malaltia cardiovascular",
          "Prevalença de malaltia renal crònica",
          "Prevalença de diabetis mellitus tipus 2",
          "Percentatge de persones polimedicades amb més de 10 principis actius"
        ) &
          Grup_edat %in% c(
            "65-74",
            "75-84",
            "85+"
          )
      )
    ),
    
    # Población asignada solo del ámbito de Atención Primaria
    Indicador != "Població assignada" |
      Àmbit == "Atenció Primària i Comunitària"
  )


Table1 <- Central_table1 %>%
  
  mutate(
    
    # Sufijo según grupo de edad
    age_suffix = case_when(
      Grup_edat == "Total" ~ "Total",
      Grup_edat == "65-74" ~ "65_74",
      Grup_edat == "75-84" ~ "75_84",
      Grup_edat == "85+"   ~ "85plus",
      TRUE ~ gsub("[^A-Za-z0-9]+", "_", Grup_edat)
    ),
    
    # Nombre de variable
    variable = case_when(
      
      Indicador == "Població assignada" ~
        paste0("Assigned_population_", age_suffix),
      
      Indicador == "Percentatge de persones ateses" ~
        paste0("Perc_Population_attended_", age_suffix),
      
      Indicador == "Percentatge de persones ateses amb nivell socioeconòmic molt baix" ~
        paste0("Very_low_SES_", age_suffix),
      
      Indicador == "Mitjana de visites a l'atenció primària i comunitària" ~
        paste0("Mean_PHC_visits_", age_suffix),
      
      Indicador == "Prevalença de demència" ~
        paste0("Dementia_", age_suffix),
      
      Indicador == "Prevalença de Parkinson" ~
        paste0("Parkinson_", age_suffix),
      
      Indicador == "Prevalença d'insuficiència cardíaca" ~
        paste0("Heart_failure_", age_suffix),
      
      Indicador == "Prevalença de persones amb diagnòstic de malaltia cardiovascular" ~
        paste0("Cardiovascular_disease_", age_suffix),
      
      Indicador == "Prevalença de malaltia renal crònica" ~
        paste0("Chronic_kidney_disease_", age_suffix),
      
      Indicador == "Prevalença de diabetis mellitus tipus 2" ~
        paste0("Diabetes_", age_suffix),
      
      Indicador == "Percentatge de persones polimedicades amb més de 10 principis actius" ~
        paste0("Polypharmacy_", age_suffix),
      
      Indicador == "Taxa de mortalitat" ~
        paste0("Standardized_mortality_rate_", age_suffix),
      
      Indicador == "Índex del proveïdor assistencial principal anual" ~
        "Annual_main_provider_index",
      
      Indicador == "Accessibilitat a 5 dies" ~
        "Accessibility_5_days",
      
      TRUE ~ NA_character_
    )
  ) %>%
  
  filter(!is.na(variable)) %>%
  
  select(
    UP_ABS,
    Centre_Territori,
    Home_based_PHC_org,
    variable,
    Resultat
  ) %>%
  
  distinct() %>%
  
  pivot_wider(
    names_from = variable,
    values_from = Resultat
  ) %>%
  
  mutate(
    across(
      -c(
        UP_ABS,
        Centre_Territori,
        Home_based_PHC_org
      ),
      as.numeric
    )
  )


Table1 <- Table1 %>%
  mutate(
    
    # Número estimado de personas atendidas
    Population_attended_Total = round(
      Assigned_population_Total *
        Perc_Population_attended_Total / 100
    ),
    
    Population_attended_65_74 = round(
      Assigned_population_65_74 *
        Perc_Population_attended_65_74 / 100
    ),
    
    Population_attended_75_84 = round(
      Assigned_population_75_84 *
        Perc_Population_attended_75_84 / 100
    ),
    
    Population_attended_85plus = round(
      Assigned_population_85plus *
        Perc_Population_attended_85plus / 100
    ),
    
    # Estructura de edad respecto a la población total
    Population_65_74_pct =
      100 * Assigned_population_65_74 /
      Assigned_population_Total,
    
    Population_75_84_pct =
      100 * Assigned_population_75_84 /
      Assigned_population_Total,
    
    Population_85plus_pct =
      100 * Assigned_population_85plus /
      Assigned_population_Total
  )

Central_table1 <- Central_res_filt %>%
  filter(
    Any == 2024,
    Sexe == "Total",
    
    (
      # Total + grupos de edad superiores
      (
        Indicador %in% c(
          "Població assignada",
          "Percentatge de persones ateses",
          "Percentatge de persones ateses amb nivell socioeconòmic molt baix",
          "Mitjana de visites a l'atenció primària i comunitària",
          "Taxa de mortalitat"
        ) &
          Grup_edat %in% c(
            "Total",
            "65-74",
            "75-84",
            "85+"
          )
      ) |
        
      # Solo Total
      (
        Indicador %in% c(
          "Índex del proveïdor assistencial principal anual",
          "Accessibilitat a 5 dies"
        ) &
          Grup_edat == "Total"
      ) |
        
      # Solo grupos de edad superiores
      (
        Indicador %in% c(
          "Prevalença de demència",
          "Prevalença de Parkinson",
          "Prevalença d'insuficiència cardíaca",
          "Prevalença de persones amb diagnòstic de malaltia cardiovascular",
          "Prevalença de malaltia renal crònica",
          "Prevalença de diabetis mellitus tipus 2",
          "Percentatge de persones polimedicades amb més de 10 principis actius"
        ) &
          Grup_edat %in% c(
            "65-74",
            "75-84",
            "85+"
          )
      )
    ),
    
    # Población asignada solo del ámbito de Atención Primaria
    Indicador != "Població assignada" |
      Àmbit == "Atenció Primària i Comunitària"
  )


Table1 <- Central_table1 %>%
  
  mutate(
    
    # Sufijo según grupo de edad
    age_suffix = case_when(
      Grup_edat == "Total" ~ "Total",
      Grup_edat == "65-74" ~ "65_74",
      Grup_edat == "75-84" ~ "75_84",
      Grup_edat == "85+"   ~ "85plus",
      TRUE ~ gsub("[^A-Za-z0-9]+", "_", Grup_edat)
    ),
    
    # Nombre de variable
    variable = case_when(
      
      Indicador == "Població assignada" ~
        paste0("Assigned_population_", age_suffix),
      
      Indicador == "Percentatge de persones ateses" ~
        paste0("Perc_Population_attended_", age_suffix),
      
      Indicador == "Percentatge de persones ateses amb nivell socioeconòmic molt baix" ~
        paste0("Very_low_SES_", age_suffix),
      
      Indicador == "Mitjana de visites a l'atenció primària i comunitària" ~
        paste0("Mean_PHC_visits_", age_suffix),
      
      Indicador == "Prevalença de demència" ~
        paste0("Dementia_", age_suffix),
      
      Indicador == "Prevalença de Parkinson" ~
        paste0("Parkinson_", age_suffix),
      
      Indicador == "Prevalença d'insuficiència cardíaca" ~
        paste0("Heart_failure_", age_suffix),
      
      Indicador == "Prevalença de persones amb diagnòstic de malaltia cardiovascular" ~
        paste0("Cardiovascular_disease_", age_suffix),
      
      Indicador == "Prevalença de malaltia renal crònica" ~
        paste0("Chronic_kidney_disease_", age_suffix),
      
      Indicador == "Prevalença de diabetis mellitus tipus 2" ~
        paste0("Diabetes_", age_suffix),
      
      Indicador == "Percentatge de persones polimedicades amb més de 10 principis actius" ~
        paste0("Polypharmacy_", age_suffix),
      
      Indicador == "Taxa de mortalitat" ~
        paste0("Standardized_mortality_rate_", age_suffix),
      
      Indicador == "Índex del proveïdor assistencial principal anual" ~
        "Annual_main_provider_index",
      
      Indicador == "Accessibilitat a 5 dies" ~
        "Accessibility_5_days",
      
      TRUE ~ NA_character_
    )
  ) %>%
  
  filter(!is.na(variable)) %>%
  
  select(
    UP_ABS,
    Centre_Territori,
    Home_based_PHC_org,
    variable,
    Resultat
  ) %>%
  
  distinct() %>%
  
  pivot_wider(
    names_from = variable,
    values_from = Resultat
  ) %>%
  
  mutate(
    across(
      -c(
        UP_ABS,
        Centre_Territori,
        Home_based_PHC_org
      ),
      as.numeric
    )
  )


Table1 <- Table1 %>%
  mutate(
    
    # Número estimado de personas atendidas
    Population_attended_Total = round(
      Assigned_population_Total *
        Perc_Population_attended_Total / 100
    ),
    
    Population_attended_65_74 = round(
      Assigned_population_65_74 *
        Perc_Population_attended_65_74 / 100
    ),
    
    Population_attended_75_84 = round(
      Assigned_population_75_84 *
        Perc_Population_attended_75_84 / 100
    ),
    
    Population_attended_85plus = round(
      Assigned_population_85plus *
        Perc_Population_attended_85plus / 100
    ),
    
    # Estructura de edad respecto a la población total
    Population_65_74_pct =
      100 * Assigned_population_65_74 /
      Assigned_population_Total,
    
    Population_75_84_pct =
      100 * Assigned_population_75_84 /
      Assigned_population_Total,
    
    Population_85plus_pct =
      100 * Assigned_population_85plus /
      Assigned_population_Total,
    
    Standardized_mortality_rate_85plus=
      Standardized_mortality_rate_85plus/1000
  )



### Descriptives

Table1_PHC_pub <- Table1 %>%
  select(
    UP_ABS,
    Assigned_population_Total,
    Perc_Population_attended_Total,
    Population_65_74_pct,
    Population_75_84_pct,
    Population_85plus_pct,
    Very_low_SES_85plus,
    Mean_PHC_visits_85plus,
    Dementia_85plus,
    Heart_failure_85plus,
    Chronic_kidney_disease_85plus,
    Polypharmacy_85plus,
    Standardized_mortality_rate_85plus,
    Annual_main_provider_index,
    Accessibility_5_days
  )%>%
  rename(
    Centre = UP_ABS,
    `Assigned population, n` = Assigned_population_Total,
    `Population aged 65–74 years, %` = Population_65_74_pct,
    `Population aged 75–84 years, %` = Population_75_84_pct,
    `Population aged ≥85 years, %` = Population_85plus_pct,
    `Very low socioeconomic status ≥85 years, %` = Very_low_SES_85plus,
    `Population visited last year primary care, %` = Perc_Population_attended_Total,
    `Mean primary care visits per person≥85 years` = Mean_PHC_visits_85plus,
    `Dementia prevalence among ≥85 years, %` = Dementia_85plus,
    `Heart failure prevalence among ≥85 years, %` = Heart_failure_85plus,
    `Chronic kidney disease prevalence among ≥85 years, %` =
      Chronic_kidney_disease_85plus,
    `Polypharmacy (>10 active substances) among ≥85 years, %` =
      Polypharmacy_85plus,
    `Mortality rate among ≥85 years` = Standardized_mortality_rate_85plus,
    `Annual main provider index, %` = Annual_main_provider_index,
    `Accessibility within 5 days, %` = Accessibility_5_days
  )%>%
  pivot_longer(
    cols = -Centre,
    names_to = "Characteristic",
    values_to = "Value"
  ) %>%
  pivot_wider(
    names_from = Centre,
    values_from = Value
  )

knitr::kable(
  Table1_PHC_pub,
  digits = 2,
  caption = "Characteristics of primary care centres and their catchment populations"
)

write.xlsx(Table1_PHC_pub, here("data","Tables_DB","Table1_PHC_pub.xlsx"))

