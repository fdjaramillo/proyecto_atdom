library(here)
library(sf)
library(tidyverse)


# load --------------------------------------------------------------------

barrios_sel <- c("27", "08", "09", "20", "21", "19", "24", "25", "26", "17")

Pob_u_censal_age <- read_csv(
  here("data", "external", "2024_pad_mdbas_edat.csv"),
  show_col_types = FALSE
)


patients_locations_sf <- readRDS(
  here("data", "processed", "adreces_SF.rds")
)

TB_Descrip <- readRDS(here("data", "Tables_DB", "TB_pacientes.RDS"))
TB_Descrip <- TB_Descrip |> select(ID, EDAT)

# calculate ---------------------------------------------------------------


pob_estratificada <- Pob_u_censal_age |>
  mutate(
    Codi_Barri_txt = str_pad(as.character(Codi_Barri), width = 2, pad = "0"),
    Seccio_Censal = as.character(Seccio_Censal),
    Valor = as.numeric(replace(Valor, Valor == "..", "0")),
    EDAT_1 = as.numeric(EDAT_1)
  ) |>
  filter(
    EDAT_1 >= 18,
    Codi_Barri_txt %in% barrios_sel
    # Codi_Barri %in% as.integer(barrios_sel)
  ) |>
  mutate(
    franja_edad = case_when(
      EDAT_1 < 65 ~ "menos65",
      EDAT_1 >= 65 & EDAT_1 <= 75 ~ "65_75",
      EDAT_1 > 75 ~ "mas75"
    )
  ) |>
  group_by(Seccio_Censal, franja_edad) |>
  summarise(poblacion_total = sum(Valor, na.rm = TRUE), .groups = "drop")


atdom_estratificado <- patients_locations_sf |>
  st_drop_geometry() |>
  mutate(
    Seccio_Censal = paste0(
      as.integer(districte),
      str_pad(as.character(secc_cens), width = 3, pad = "0")
      # as.character(secc_cens)
    ),
    Seccio_Censal = as.character(as.integer(Seccio_Censal))
  ) |>
  left_join(
    TB_Descrip,
    by = "ID"
  ) |>
  mutate(
    franja_edad = case_when(
      EDAT < 65 ~ "menos65",
      EDAT >= 65 & EDAT <= 75 ~ "65_75",
      EDAT > 75 ~ "mas75"
    )
  ) |>
  group_by(Seccio_Censal, franja_edad) |>
  summarise(n_atdom = n(), .groups = "drop")

res_formato_ancho <- pob_estratificada |>
  left_join(atdom_estratificado, by = c("Seccio_Censal", "franja_edad")) |>
  mutate(
    n_atdom = replace_na(n_atdom, 0),
    densidad = n_atdom / poblacion_total
  ) |>
  pivot_wider(
    id_cols = Seccio_Censal,
    names_from = franja_edad,
    values_from = densidad,
    names_prefix = "densidad_"
  ) |>
  left_join(
    pob_estratificada |>
      group_by(Seccio_Censal) |>
      summarise(poblacion_total_uc = sum(poblacion_total), .groups = "drop"),
    by = "Seccio_Censal"
  )


# -------------------------------------------------------------------------
# tasa de inclusión o cobertura del programa ATDOM a nivel de unidad censal


# 1. Denominadores desde la población censal
pob_denominadores <- Pob_u_censal_age |>
  mutate(
    Seccio_Censal = as.character(Seccio_Censal),
    Valor = as.numeric(replace(Valor, Valor == "..", "0")),
  ) |>
  group_by(Seccio_Censal) |>
  summarise(
    pob_total = sum(Valor, na.rm = TRUE),
    pob_65mas = sum(Valor[EDAT_1 >= 65], na.rm = TRUE),
    pob_75mas = sum(Valor[EDAT_1 >= 75], na.rm = TRUE),
    pob_80mas = sum(Valor[EDAT_1 >= 80], na.rm = TRUE),
    .groups = "drop"
  )

# 2. Numeradores desde los pacientes ATDOM
atdom_numeradores <- patients_locations_sf |>
  st_drop_geometry() |>
  mutate(
    Seccio_Censal = paste0(
      as.integer(districte),
      str_pad(as.character(secc_cens), width = 3, pad = "0")
      # as.character(secc_cens)
    ),
    Seccio_Censal = as.character(as.integer(Seccio_Censal))
  ) |>
  left_join(
    TB_Descrip,
    by = "ID"
  ) |>
  group_by(Seccio_Censal) |>
  summarise(
    atdom_total = n(),
    atdom_65mas = sum(EDAT >= 65, na.rm = TRUE),
    atdom_75mas = sum(EDAT >= 75, na.rm = TRUE),
    atdom_80mas = sum(EDAT >= 80, na.rm = TRUE),
    .groups = "drop"
  )

# 3. Cruce y cálculo de las 4 tasas por 1.000 habitantes
tasas_cobertura_atdom <- atdom_numeradores |>
  inner_join(pob_denominadores, by = "Seccio_Censal") |>
  mutate(
    tasa_1000_hab      = (atdom_total / pob_total) * 1000,
    tasa_1000_pob65mas = if_else(pob_65mas > 0, (atdom_65mas / pob_65mas) * 1000, 0),
    tasa_1000_pob75mas = if_else(pob_75mas > 0, (atdom_75mas / pob_75mas) * 1000, 0),
    tasa_1000_pob80mas = if_else(pob_80mas > 0, (atdom_80mas / pob_80mas) * 1000, 0)
  )
