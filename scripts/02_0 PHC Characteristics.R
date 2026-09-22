# ============================================================
# 02_0 PHC Characteristics.R
# ============================================================

source(here("scripts", "00_0 setup.R"))

############ Prep data_frame ###############

# Load Data

Central_res<- read.csv2(here("data","external","DADES_CENTRAL_RESULTATS_2024.csv"))%>%
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
        Sexe == "Total")

unitats_cens_sel <- readRDS(here("data", "SF", "unitats_censals_estudi_sf.rds"))%>%
  st_drop_geometry()

Data_SES_UC <- readRDS(
  here("data", "processed", "Data_SES_UC.rds"))%>%
  st_drop_geometry()%>%
  inner_join(unitats_cens_sel,
             by="Seccio_Censal")

## Compute SES

vars_ses <- c(
  "Index_SES",
  "Renta_media_hogar",
  "Perc_estudis_baixos",
  "Perc_joves_no_estudis_postESO",
  "Perc_pob_estrangera_paisos_renda_baixa"
)

Data_SES_table<-Data_SES_UC%>%
  filter(!is.na(UP_ABS))%>%
  st_drop_geometry() %>%
  group_by(UP_ABS) %>%
  summarise(
    across(
      all_of(vars_ses),
      ~ sprintf(
        "%.2f (%.2f)",
        mean(.x, na.rm = TRUE),
        sd(.x, na.rm = TRUE)
      )
    )
  )

# Summary form Central de Resultats

Central_table1 <- Central_res %>%
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
  )%>%
  
  mutate(
    # Assigned population aged ≥65 years
    Assigned_population_65plus =
      Assigned_population_65_74 +
      Assigned_population_75_84 +
      Assigned_population_85plus,
    
    # Weighted percentage with ≥1 primary care visit
    Perc_Population_attended_65plus =
      (
        Assigned_population_65_74 * Perc_Population_attended_65_74 +
          Assigned_population_75_84 * Perc_Population_attended_75_84 +
          Assigned_population_85plus * Perc_Population_attended_85plus
      ) /
      Assigned_population_65plus
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


Table1_PHC_pub <- Table1 %>%
  left_join(
    Data_SES_table,
    by = "UP_ABS"
  ) %>%
  
  select(
    UP_ABS,
    
    # Population structure
    Assigned_population_Total,
    Perc_Population_attended_65plus,
    Population_65_74_pct,
    Population_75_84_pct,
    Population_85plus_pct,
    
    # Socioeconomic context
    Index_SES,
    Renta_media_hogar,
    Perc_estudis_baixos,
    Perc_joves_no_estudis_postESO,
    Perc_pob_estrangera_paisos_renda_baixa,
    Very_low_SES_85plus,
    
    # Health care use and clinical burden
    Mean_PHC_visits_85plus,
    Dementia_85plus,
    Heart_failure_85plus,
    Chronic_kidney_disease_85plus,
    Polypharmacy_85plus,
    Standardized_mortality_rate_85plus,
    
    # Primary care organisation/performance
    Annual_main_provider_index,
    Accessibility_5_days
  ) %>%
  
  # Format numeric variables before renaming
  mutate(
    Perc_Population_attended_65plus =
      round(Perc_Population_attended_65plus, 1),

    Population_65_74_pct =
      round(Population_65_74_pct, 1),
    
    Population_75_84_pct =
      round(Population_75_84_pct, 1),
    
    Population_85plus_pct =
      round(Population_85plus_pct, 1),
    
    Very_low_SES_85plus =
      round(Very_low_SES_85plus, 1),
    
    Mean_PHC_visits_85plus =
      round(Mean_PHC_visits_85plus, 1),
    
    Dementia_85plus =
      round(Dementia_85plus, 1),
    
    Heart_failure_85plus =
      round(Heart_failure_85plus, 1),
    
    Chronic_kidney_disease_85plus =
      round(Chronic_kidney_disease_85plus, 1),
    
    Polypharmacy_85plus =
      round(Polypharmacy_85plus, 1),
    
    Standardized_mortality_rate_85plus =
      round(Standardized_mortality_rate_85plus, 0),
    
    Annual_main_provider_index =
      round(Annual_main_provider_index, 1),
    
    Accessibility_5_days =
      round(Accessibility_5_days, 1)
  ) %>%
  
  rename(
    Centre = UP_ABS,
    
    `Assigned population, n` =
      Assigned_population_Total,
    
    `Population aged ≥65 years with ≥1 primary care visit in the previous year, %` =
      Perc_Population_attended_65plus,
    
    `Population aged 65–74 years, %` =
      Population_65_74_pct,
    
    `Population aged 75–84 years, %` =
      Population_75_84_pct,
    
    `Population aged ≥85 years, %` =
      Population_85plus_pct,
    
    `Census-area socioeconomic index (territorial mean = 100), mean (SD)` =
      Index_SES,
    
    `Mean household income, € thousands, mean (SD)` =
      Renta_media_hogar,
    
    `Census-area population with low educational attainment, %, mean (SD)` =
      Perc_estudis_baixos,
    
    `Census-area young population without post-compulsory education, %, mean (SD)` =
      Perc_joves_no_estudis_postESO,
    
    `Census-area foreign-born population from low-income countries, %, mean (SD)` =
      Perc_pob_estrangera_paisos_renda_baixa,
    
    `Very low socioeconomic status among persons aged ≥85 years, %` =
      Very_low_SES_85plus,
    
    `Primary care visits per person aged ≥85 years, mean` =
      Mean_PHC_visits_85plus,
    
    `Dementia prevalence among persons aged ≥85 years, %` =
      Dementia_85plus,
    
    `Heart failure prevalence among persons aged ≥85 years, %` =
      Heart_failure_85plus,
    
    `Chronic kidney disease prevalence among persons aged ≥85 years, %` =
      Chronic_kidney_disease_85plus,
    
    `Polypharmacy (≥10 active substances) among persons aged ≥85 years, %` =
      Polypharmacy_85plus,
    
    `Age-standardized mortality rate among persons aged ≥85 years, per 100,000 population` =
      Standardized_mortality_rate_85plus,
    
    `Annual main provider index, %` =
      Annual_main_provider_index,
    
    `Primary care accessibility within 5 days, %` =
      Accessibility_5_days
  ) %>%
  
  mutate(
    across(-Centre, as.character)
  ) %>%
  
  pivot_longer(
    cols = -Centre,
    names_to = "Characteristic",
    values_to = "Value"
  ) %>%
  
  pivot_wider(
    names_from = Centre,
    values_from = Value
  )


write.xlsx(Table1_PHC_pub , here("Output","Tables","Table1_PHC_pub.xlsx"))

