source(here("scripts", "00_0 setup.R"))

# ============================================================
# 02_2 Atdom inclusion rate calcs by census unit * 1000 inhabitants.R
# ============================================================


#Data dowload

#URLGeneralitat de Catalunya 

Sexe_edat_quinquenal_2024<- "https://www.idescat.cat/pub/?id=censph&n=539&by=sec&t=2024&f=zip&fi=ssv&lang=es"

#Download  
download.file(
    url = Sexe_edat_quinquenal_2024,
    destfile = "data/external/Sexe_edat_quinquenal_2024.csv",
    mode = "wb")


# Data Load

ABS_sf<-readRDS(here("data", "SF", "ABS_sel_SF.rds"))

U_cens <- readRDS(here("data", "SF", "unitats_censals_estudi_sf.rds")
)

Pob_u_censal_age_sex <- read_csv2(
  here("data", "external", "Sexe_edat_quinquenal_2024.csv"),
  show_col_types = T)

patients_locations_sf <- readRDS(here("data", "SF", "BCN_adreces_users_SF_included_SF.rds"))

Pacients_DATA<- readRDS(here("data","processed","DF_pacientes_inicial.RDS"))


# Data prep
# ------------------------------------------------------------
# 1. POBLACIÓN CENSAL ESTRATIFICADA POR EDAD
#
# Resultado:
# Una tabla en formato largo con una fila por:
#   Seccio_Censal × grupo de edad
#
# Para cada combinación se obtiene el denominador poblacional
# correspondiente.
#
# Variables principales resultantes:
#   - Seccio_Censal
#   - edat_cat: 18-64, 65-74, 75-84, ≥85
#   - poblacion_total: número de habitantes del estrato
# --------------------------------------------------------

Pob_u_censal_age_sex <-Pob_u_censal_age_sex%>%
 select(-estado)%>%
  filter(str_starts(as.character(`sección censal`), "Barcelona"))%>%
  mutate(
    Seccio_Censal = paste0(
      as.integer(str_extract(`sección censal`, "(?<=DC)\\d+")),
      str_pad(
        str_extract(`sección censal`, "(?<=SC)\\d+"),
        width = 3,
        pad = "0"
      )
    )
  )%>%
  inner_join(st_drop_geometry(U_cens[, 8]),
             by = "Seccio_Censal")%>%
  select(-`sección censal`)


pob_estratificada_u_censal <- Pob_u_censal_age_sex %>%
  
  # Excluir totales originales para evitar duplicidades
  filter(
    sexo %in% c("hombres", "mujeres"),
    edad != "total"
  ) %>%
  
  # Crear los grupos de edad del estudio
  mutate(
    edat_cat = case_when(
      edad %in% c(
        "de 15 a 19 años",
        "de 20 a 24 años",
        "de 25 a 29 años",
        "de 30 a 34 años",
        "de 35 a 39 años",
        "de 40 a 44 años",
        "de 45 a 49 años",
        "de 50 a 54 años",
        "de 55 a 59 años",
        "de 60 a 64 años"
      ) ~ "15_64",
      
      edad %in% c(
        "de 65 a 69 años",
        "de 70 a 74 años"
      ) ~ "65_74",
      
      edad %in% c(
        "de 75 a 79 años",
        "de 80 a 84 años"
      ) ~ "75_84",
      
      edad %in% c(
        "de 85 a 89 años",
        "de 90 a 94 años",
        "de 95 a 99 años",
        "100 años o más"
      ) ~ "85_plus",
      
      TRUE ~ NA_character_
    )
  ) %>%
  
  # Mantener población ≥15 años
  filter(!is.na(edat_cat)) %>%
  
  # Población por sección × edad × sexo
  group_by(
    Seccio_Censal,
    edat_cat,
    sexo
  ) %>%
  
  summarise(
    poblacion_total = sum(valor, na.rm = TRUE),
    .groups = "drop"
  )

# ------------------------------------------------------------
# 1. DENOMINADORES:
# POBLACIÓN CENSAL POR UNIDAD, GRUPO DE EDAD Y SEXO
#
# Resultado:
# Una tabla en formato largo con una fila por:
#
#   Seccio_Censal × edat_cat × sexo
#
# Para cada combinación se obtiene:
#   poblacion_total = número de habitantes del estrato
#
# Estos valores serán los denominadores de las tasas específicas
# de inclusión en ATDOM.
# ------------------------------------------------------------

pob_denominadores <- pob_estratificada_u_censal %>%
  select(
    Seccio_Censal,
    edat_cat,
    sexo,
    poblacion_total
  )

# ------------------------------------------------------------
# 2. NUMERADORES:
# PACIENTES ATDOM POR UNIDAD CENSAL, GRUPO DE EDAD Y SEXO
#
# Resultado:
# Una tabla en formato largo con una fila por:
#
#   Seccio_Censal × edat_cat × sexo
#
# Para cada combinación se obtiene:
#   n_atdom = número de pacientes incluidos en ATDOM
#
# Importante:
# Si en una unidad censal no existe ningún paciente ATDOM de un
# determinado grupo de edad y sexo, esa combinación todavía no
# aparecerá en esta tabla.
#
# Se incorporará posteriormente al cruzarla con los denominadores
# poblacionales.
# ------------------------------------------------------------

atdom_numeradores <- patients_locations_sf %>%
  st_drop_geometry() %>%
  
  left_join(
    Pacients_DATA %>%
      mutate(ID = as.integer(ID)),
    by = "ID"
  ) %>%
  
  mutate(
    # Asegurar el mismo tipo que en la tabla poblacional
    Seccio_Censal = as.character(Seccio_Censal),
    
    # Recodificación del sexo
    sexo = case_when(
      SEXE == "H" ~ "hombres",
      SEXE == "D" ~ "mujeres",
      TRUE ~ NA_character_
    ),
    
    # Se utilizan exactamente los mismos grupos de edad
    edat_cat = case_when(
      EDAT >= 15 & EDAT < 65 ~ "15_64",
      EDAT >= 65 & EDAT < 75 ~ "65_74",
      EDAT >= 75 & EDAT < 85 ~ "75_84",
      EDAT >= 85             ~ "85_plus",
      TRUE ~ NA_character_
    )
  ) %>%
  
  filter(
    !is.na(Seccio_Censal),
    !is.na(edat_cat),
    !is.na(sexo)
  ) %>%
  
  count(
    Seccio_Censal,
    edat_cat,
    sexo,
    name = "n_atdom"
  )

# ------------------------------------------------------------
# 3. CÁLCULO DE TASAS ESPECÍFICAS DE ATDOM POR 1.000 HABITANTES
#    SEGÚN GRUPO DE EDAD Y SEXO
#
# Se parte de los DENOMINADORES poblacionales para garantizar que
# todas las unidades censales y todos los estratos de edad y sexo
# con población estén presentes, incluso cuando n_atdom = 0.
#
# Resultado:
# Una tabla larga con:
#
#   Seccio_Censal
#   edat_cat
#   sexo
#   poblacion_total
#   n_atdom
#   tasa_atdom_1000
#
# La tasa representa:
#
#   pacientes ATDOM del estrato /
#   habitantes del mismo grupo de edad y sexo × 1.000
#
# Si no hay pacientes ATDOM en un estrato:
#   n_atdom = 0
#   tasa_atdom_1000 = 0
#
# Si el denominador fuese 0:
#   tasa_atdom_1000 = NA
# ------------------------------------------------------------

tasas_cobertura_atdom_long <- pob_denominadores %>%
  
  # Incorporar numeradores ATDOM por sección, edad y sexo
  left_join(
    atdom_numeradores,
    by = c(
      "Seccio_Censal",
      "edat_cat",
      "sexo"
    )
  ) %>%
  
  mutate(
    # Un estrato poblacional sin pacientes ATDOM corresponde
    # a 0 casos, no a un valor missing
    n_atdom = replace_na(n_atdom, 0L),
    
    # Tasa específica por 1.000 habitantes
    tasa_atdom_1000 = if_else(
      poblacion_total > 0,
      (n_atdom / poblacion_total) * 1000,
      NA_real_
    )
  )


saveRDS(
  tasas_cobertura_atdom_long,
  here(
    "data",
    "processed",
    "Age_sex_categorized_long_ATDOM_U_CENSAL.rds"
  )
)

# ------------------------------------------------------------
# 4. TRANSFORMACIÓN A FORMATO ANCHO
#
# Resultado:
# Una fila por unidad censal, con una columna para cada tasa
# específica por grupo de edad y sexo:
#
#   tasa_atdom_15_64_hombres
#   tasa_atdom_15_64_mujeres
#   tasa_atdom_65_74_hombres
#   tasa_atdom_65_74_mujeres
#   tasa_atdom_75_84_hombres
#   tasa_atdom_75_84_mujeres
#   tasa_atdom_85_plus_hombres
#   tasa_atdom_85_plus_mujeres
#
# Este formato es útil para:
#   - descripción de las unidades censales
#   - comprobación de resultados
#   - unión con cartografía
#   - mapas específicos por grupo de edad y sexo
#
# NOTA:
# Estas siguen siendo tasas ESPECÍFICAS por edad y sexo.
# Todavía no constituyen la tasa estandarizada por edad y sexo.
# ------------------------------------------------------------

tasas_cobertura_atdom_wide <- tasas_cobertura_atdom_long %>%
  select(
    Seccio_Censal,
    edat_cat,
    sexo,
    tasa_atdom_1000
  ) %>%
  
  pivot_wider(
    id_cols = Seccio_Censal,
    names_from = c(edat_cat, sexo),
    values_from = tasa_atdom_1000,
    names_glue = "tasa_atdom_{edat_cat}_{sexo}",
    values_fill = 0
  )

saveRDS(
  tasas_cobertura_atdom_wide,
  here(
    "data",
    "processed",
    "Age_sex_categorized_wide_ATDOM_U_CENSAL.rds"
  )
)

# ------------------------------------------------------------
# 5. PESOS ESTÁNDAR POR EDAD Y SEXO
#
# La población estándar corresponde a la población conjunta
# de todas las unidades censales incluidas en el estudio.
#
# Se obtiene un peso para cada combinación:
#   grupo de edad × sexo
#
# La suma de todos los pesos debe ser 1.
# ------------------------------------------------------------

pesos_edad_sexo <- pob_denominadores %>%
  group_by(
    edat_cat,
    sexo
  ) %>%
  summarise(
    poblacion_estandar = sum(poblacion_total, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    peso_edad_sexo = poblacion_estandar /
      sum(poblacion_estandar)
  )


# ------------------------------------------------------------
# 6. ESTANDARIZACIÓN DIRECTA POR EDAD Y SEXO
#
# Para cada unidad censal:
#
#   tasa estandarizada =
#       suma(
#         tasa específica por edad y sexo ×
#         peso estándar de edad y sexo
#       )
#
# Resultado:
# Una fila por unidad censal con una única tasa de inclusión
# en ATDOM estandarizada por edad y sexo,
# expresada por 1.000 habitantes.
# ------------------------------------------------------------

tasas_atdom_estandarizadas <- tasas_cobertura_atdom_long %>%
  
  left_join(
    pesos_edad_sexo,
    by = c(
      "edat_cat",
      "sexo"
    )
  ) %>%
  
  mutate(
    contribucion_edad_sexo =
      tasa_atdom_1000 * peso_edad_sexo
  ) %>%
  
  group_by(Seccio_Censal) %>%
  
  summarise(
    tasa_atdom_std_1000 = sum(
      contribucion_edad_sexo,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

saveRDS(
  tasas_atdom_estandarizadas,
  here(
    "data",
    "processed",
    "Age_sex_standardized_ATDOM_U_CENSAL.rds"
  )
)
