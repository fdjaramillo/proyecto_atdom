source(here("scripts", "00_0 setup.R"))

# ============================================================
# 02_2 Atdom inclusion rate calcs by census unit * 1000 inhabitants.R
# ============================================================

# Data Load

ABS_sf<-readRDS(here("data", "SF", "ABS_sel_SF.rds"))

U_cens <- readRDS(here("data", "SF", "unitats_censals_estudi_sf.rds")
)

Pob_u_censal_age <- read_csv(
  here("data", "external", "2024_pad_mdbas_edat-q.csv"),
  show_col_types = FALSE)

patients_locations_sf <- readRDS(here("data", "SF", "BCN_adreces_users_SF_included_SF.rds"))

TB_Descrip <- readRDS(here("data", "Final", "TB_Descrip.RDS"))%>%
                           select(ID, Edat)

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

pob_estratificada_u_censal <- Pob_u_censal_age %>%
  filter(EDAT_Q >= 3) %>%
  mutate(
    Codi_Barri = str_pad(
      as.character(Codi_Barri),
      width = 2,
      pad = "0"
    ),
    
    Seccio_Censal = as.character(Seccio_Censal),
    
    EDAT_Q = as.numeric(EDAT_Q),
    
    # Celda censurada: recuento <5 habitantes
    censurado = Valor == "..",
    
    # ".." se mantiene como NA; no se considera 0
    Valor_num = as.numeric(na_if(Valor, "..")),
    
    # Imputación principal para celdas <5
    Valor_imp = if_else(censurado, 2, Valor_num),
    
    edat_cat = case_when(
      EDAT_Q >= 3  & EDAT_Q <= 12 ~ "15_64",
      EDAT_Q %in% c(13, 14)       ~ "65_74",
      EDAT_Q %in% c(15, 16)       ~ "75_84",
      EDAT_Q >= 17                 ~ "85_plus",
      TRUE ~ NA_character_
    )
  ) %>%
  group_by(Seccio_Censal, edat_cat) %>%
  summarise(
    poblacion_total = sum(Valor_imp),
    n_censurados = sum(censurado),
    .groups = "drop"
  )


# 2. PACIENTES ATDOM ESTRATIFICADOS POR EDAD
#
# Resultado:
# Una tabla en formato largo con una fila por:
#   Seccio_Censal × grupo de edad
#
# Para cada combinación se obtiene el numerador:
# número de pacientes incluidos en ATDOM.
#
# Variables principales resultantes:
#   - Seccio_Censal
#   - edat_cat
#   - n_atdom: número de pacientes ATDOM del estrato
#
# Importante:
# Si una unidad censal no tiene ningún paciente ATDOM en un
# determinado grupo de edad, esa combinación todavía no aparece
# en esta tabla.

Pacients_stratificat <- patients_locations_sf %>%
  st_drop_geometry() %>%
  left_join(
    TB_Descrip%>%
      mutate(ID=as.integer(ID)),
    by = "ID"
  ) %>%
  mutate(
    edat_cat = case_when(
      Edat >= 15 & Edat < 64 ~ "15_64",
      Edat >= 65 & Edat < 75 ~ "65_74",
      Edat >= 75 & Edat < 85 ~ "75_84",
      Edat >= 85 ~ "85_plus",
      TRUE ~ NA_character_
    )
  ) %>%
  group_by(Seccio_Censal, edat_cat) %>%
  summarise(n_atdom = n(), .groups = "drop")

# 3. CÁLCULO DE TASAS ESPECÍFICAS DE ATDOM POR EDAD
#
# Resultado:
# Una tabla en formato largo con una fila por:
#   Seccio_Censal × grupo de edad
#
# Se parte de la tabla poblacional para asegurar que todas las
# unidades censales y grupos de edad con población estén presentes.
#
# Cuando no existen pacientes ATDOM en un estrato:
#   n_atdom = 0
#
# Se calcula:
#   tasa_atdom = pacientes ATDOM / población del estrato × 1000
#
# Interpretación:
# Número de pacientes ATDOM por 1.000 habitantes del mismo grupo
# de edad y unidad censal.
#
# Variables resultantes:
#   - Seccio_Censal
#   - edat_cat
#   - poblacion_total
#   - n_atdom
#   - tasa_atdom

res_format <- pob_estratificada_u_censal %>%
  left_join(
    Pacients_stratificat,
    by = c("Seccio_Censal", "edat_cat")
  ) %>%
  mutate(
    n_atdom = replace_na(n_atdom, 0),
    tasa_atdom = round(n_atdom / poblacion_total * 1000,2)
  )

# Resultado:
# Una tabla con UNA FILA POR UNIDAD CENSAL.
#
# Las tasas específicas por edad pasan a columnas separadas:
#   - tasa_atdom_15_64
#   - tasa_atdom_65_74
#   - tasa_atdom_75_84
#   - tasa_atdom_85_plus
#
# Además se calcula:
#   poblacion_total_uc
#
# que corresponde a la población total ≥18 años de la unidad
# censal, sumando todos los grupos de edad.
#
# Esta tabla es útil para:
#   - descripción de las unidades censales
#   - unión con cartografía sf
#   - representación de tasas específicas en mapas
#
# NOTA:
# Estas todavía son tasas ESPECÍFICAS por edad.
# No es todavía la tasa ATDOM estandarizada por edad.

res_format_wide <- res_format %>%
  select(
    Seccio_Censal,
    edat_cat,
    tasa_atdom
  ) %>%
  pivot_wider(
    id_cols = Seccio_Censal,
    names_from = edat_cat,
    values_from = tasa_atdom,
    names_prefix = "tasa_atdom_",
    values_fill = 0
  ) %>%
  left_join(
    pob_estratificada_u_censal %>%
      group_by(Seccio_Censal) %>%
      summarise(
        poblacion_total_uc = sum(poblacion_total, na.rm = TRUE),
        .groups = "drop"
      ),
    by = "Seccio_Censal"
  )

# TASAS ESPECÍFICAS DE INCLUSIÓN EN ATDOM POR UNIDAD CENSAL
# Y GRUPO DE EDAD
#
# Objetivo:
# Obtener, para cada unidad censal, la tasa de pacientes ATDOM
# por 1.000 habitantes dentro de cuatro grupos de edad:
#   18-64, 65-74, 75-84 y ≥85 años.
#
# El resultado en formato largo servirá posteriormente para
# realizar la estandarización directa por edad.
# ============================================================


# ------------------------------------------------------------
# 1. DENOMINADORES: POBLACIÓN CENSAL POR UNIDAD Y GRUPO DE EDAD
#
# Resultado:
# Una tabla en formato largo con una fila por:
#
#   Seccio_Censal × edat_cat
#
# Para cada combinación se obtiene:
#   poblacion_total = número de habitantes del grupo de edad
#
# Estos valores serán los denominadores de las tasas específicas
# de inclusión en ATDOM.

pob_denominadores <- pob_estratificada_u_censal %>%
  select(
    Seccio_Censal,
    edat_cat,
    poblacion_total,
    n_censurados
  )

# 2. NUMERADORES: PACIENTES ATDOM POR UNIDAD CENSAL Y EDAD
#
# Resultado:
# Una tabla en formato largo con una fila por:
#
#   Seccio_Censal × edat_cat
#
# Para cada combinación se obtiene:
#   n_atdom = número de pacientes incluidos en ATDOM
#
# Importante:
# Si en una unidad censal no existe ningún paciente ATDOM de un
# determinado grupo de edad, esa combinación no aparecerá todavía
# En esta tabla. Se incorporará posteriormente al cruzarla con los
# denominadores poblacionales.
# ------------------------------------------------------------

atdom_numeradores <- patients_locations_sf %>%
  st_drop_geometry() %>%
  
  left_join(
    TB_Descrip %>%
      mutate(ID = as.integer(ID)),
    by = "ID"
  ) %>%
  
  mutate(
    # Asegurar el mismo tipo que en la tabla poblacional
    Seccio_Censal = as.character(Seccio_Censal),
    
    # Se utilizan exactamente los mismos grupos de edad
    edat_cat = case_when(
      Edat >= 15 & Edat < 65 ~ "15_64",
      Edat >= 65 & Edat < 75 ~ "65_74",
      Edat >= 75 & Edat < 85 ~ "75_84",
      Edat >= 85 ~ "85_plus",
      TRUE ~ NA_character_
    )
  ) %>%
  
  filter(!is.na(edat_cat)) %>%
  
  count(
    Seccio_Censal,
    edat_cat,
    name = "n_atdom"
  )


# 3. CÁLCULO DE TASAS ESPECÍFICAS DE ATDOM POR 1.000 HABITANTES
#
# Se parte de los DENOMINADORES poblacionales para garantizar que
# todas las unidades censales y todos los grupos de edad con
# población estén presentes, incluso cuando n_atdom = 0.
#
# Resultado:
# Una tabla larga con:
#
#   Seccio_Censal
#   edat_cat
#   poblacion_total
#   n_atdom
#   tasa_atdom_1000
#
# La tasa representa:
#
#   pacientes ATDOM del grupo /
#   habitantes del mismo grupo de edad × 1.000
#
# Si no hay pacientes ATDOM en un estrato:
#   n_atdom = 0
#   tasa = 0
#
# Si el denominador fuese 0:
#   tasa = NA
# ------------------------------------------------------------

tasas_cobertura_atdom_long <- pob_denominadores %>%
  inner_join(U_cens[,8],
             by="Seccio_Censal")%>%
  left_join(
    atdom_numeradores,
    by = c("Seccio_Censal", "edat_cat")
  ) %>%
  
  mutate(
    # Una combinación población/edad sin pacientes ATDOM
    # corresponde a 0 casos, no a missing
    n_atdom = replace_na(n_atdom, 0L),
    
    tasa_atdom_1000 = if_else(
      poblacion_total > 0,
      (n_atdom / poblacion_total) * 1000,
      NA_real_
    )
  )

saveRDS(tasas_cobertura_atdom_long,here("data","processed","Age_categorized_long_ATDOM_U_CENSAL"))

# ------------------------------------------------------------
# 4. TRANSFORMACIÓN A FORMATO ANCHO
#
# Resultado:
# Una fila por unidad censal, con una columna para cada tasa
# específica por edad:
#
#   tasa_atdom_15_64
#   tasa_atdom_65_74
#   tasa_atdom_75_84
#   tasa_atdom_85_plus
#
# Este formato es útil para:
#   - descripción de las unidades censales
#   - comprobación de resultados
#   - unión con cartografía
#   - mapas específicos por grupo de edad
#
# NOTA:
# Estas siguen siendo tasas ESPECÍFICAS por edad.
# Todavía no constituyen la tasa estandarizada por edad.
# ------------------------------------------------------------

tasas_cobertura_atdom_wide <- tasas_cobertura_atdom_long %>%
  inner_join(U_cens[,8],
             by="Seccio_Censal")%>%
  select(
    Seccio_Censal,
    edat_cat,
    tasa_atdom_1000
  ) %>%
  
  pivot_wider(
    id_cols = Seccio_Censal,
    names_from = edat_cat,
    values_from = tasa_atdom_1000,
    names_prefix = "tasa_atdom_",
    values_fill = 0
  )

saveRDS(tasas_cobertura_atdom_wide,here("data","processed","Age_categorized_wide_ATDOM_U_CENSAL"))

# 4. Estandarización Y PESOS POR GRUPO DE EDAD
#
# Se utiliza como población estándar la población conjunta
# de todas las unidades censales incluidas en el estudio.
#
# Resultado:
# Para cada grupo de edad se obtiene su peso relativo en la
# población total del estudio.
#
# Los mismos pesos se aplicarán posteriormente a todas las
# unidades censales.
# ------------------------------------------------------------

pesos_edad <- pob_denominadores %>%
  group_by(edat_cat) %>%
  summarise(
    poblacion_estandar = sum(poblacion_total, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    peso_edad = poblacion_estandar /
      sum(poblacion_estandar)
  )

# 5. ESTANDARIZACIÓN DIRECTA POR EDAD
#
# Para cada unidad censal:
#
#   tasa estandarizada =
#       suma(tasa específica por edad × peso estándar)
#
# Resultado:
# Una fila por unidad censal con una única tasa de inclusión
# ATDOM estandarizada por edad, expresada por 1.000 habitantes.

tasas_atdom_estandarizadas <- tasas_cobertura_atdom_long %>%
  inner_join(U_cens[,8],
             by="Seccio_Censal")%>%
  left_join(
    pesos_edad,
    by = "edat_cat"
  ) %>%
  mutate(
    contribucion_edad = tasa_atdom_1000 * peso_edad
  ) %>%
  group_by(Seccio_Censal) %>%
  summarise(
    tasa_atdom_std_1000 = sum(
      contribucion_edad,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

saveRDS(tasas_atdom_estandarizadas,here("data","processed","Age_standarized_ATDOM_U_CENSAL.rds"))

