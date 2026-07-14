# ============================================================
# 060.1_Plot_patients_center_location_and_census_place.R
# ============================================================

source(here("scripts", "00_setup.R"))

# ============================================================
# 0. Parameters
# ============================================================

barrios_sel <- c("27", "08", "09", "20", "21", "19", "24", "25", "26", "17")
districts_sel <- c("05", "02", "04")

density_palette <- c(
  "white", "#fee0d2", "#fcbba1", "#fc9272",
  "#fb6a4a", "#ef3b2c", "#cb181d", "#99000d"
)

# ============================================================
# 1. Load spatial data
# ============================================================

nodes <- st_read(
  here("data", "external", "BCN_GrafVial_SHP", "BCN_GrafVial_Nodes_ETRS89_SHP.shp"),
  quiet = TRUE
)

trams <- st_read(
  here("data", "external", "BCN_GrafVial_SHP", "BCN_GrafVial_Trams_ETRS89_SHP.shp"),
  quiet = TRUE
)

U_cens <- st_read(
  here("data", "external", "BCN_UNITATS_ADM", "0301040100_SecCens_UNITATS_ADM.shp"),
  quiet = TRUE
)

centros_sf <- readRDS(
  here("data", "external", "Centres_estudi_adreces_sf.rds")
)

patients_locations_sf <- readRDS(
  here("data", "processed", "adreces_SF.rds")
)

# ============================================================
# 2. Prepare street network and centres
# ============================================================

trams_sel <- trams %>%
  filter(Distric_E %in% districts_sel)

nodes_sel <- nodes[
  st_intersects(nodes, trams_sel, sparse = FALSE) |> apply(1, any),
]

centros_sf <- centros_sf %>%
  st_transform(st_crs(trams_sel))

# Optional: patient points from raw longitude/latitude
patients_sf <- patients_locations_sf %>%
  st_as_sf(
    coords = c("lon_paciente", "lat_paciente"),
    crs = 4326,
    remove = FALSE
  ) %>%
  st_transform(st_crs(centros_sf))

# ============================================================
# 3. Load and prepare census population
# ============================================================

Pob_u_censal <- read_csv(
  here("data", "external", "2026_pad_mdbas_sexe.csv"),
  show_col_types = FALSE
)

Pob_u_censal_sel <- Pob_u_censal %>%
  mutate(
    SEXE = case_when(
      SEXE == 1 ~ "homes",
      SEXE == 2 ~ "dones",
      TRUE ~ NA_character_
    ),
    Codi_Barri_txt = str_pad(as.character(Codi_Barri), width = 2, pad = "0"),
    Seccio_Censal = as.character(Seccio_Censal)
  ) %>%
  filter(
    Codi_Barri_txt %in% barrios_sel,
    !is.na(SEXE)
  ) %>%
  pivot_wider(
    names_from = SEXE,
    values_from = Valor
  ) %>%
  mutate(
    homes = coalesce(homes, 0),
    dones = coalesce(dones, 0),
    Total_pob = as.numeric(homes + dones)
  )

# ============================================================
# 4. Aggregate patients by census section
# ============================================================

Patients_censal <- read_rds(
  here("data", "Tables_DB", "Adreces_SF_ID.rds")
) %>%
  st_drop_geometry() %>%
  mutate(
    Seccio_Censal = paste0(
      as.integer(districte),
      str_pad(as.character(secc_cens), width = 3, pad = "0")
    ),
    Seccio_Censal = as.character(as.integer(Seccio_Censal))
  ) %>%
  filter(!is.na(Seccio_Censal)) %>%
  group_by(Seccio_Censal) %>%
  summarise(
    Patients = n_distinct(ID),
    .groups = "drop"
  )

# ============================================================
# 5. Join population and patients
# ============================================================

Patients_adreces_cens <- Pob_u_censal_sel %>%
  mutate(
    Seccio_Censal = as.character(as.integer(Seccio_Censal))
  ) %>%
  left_join(
    Patients_censal,
    by = "Seccio_Censal"
  ) %>%
  mutate(
    Patients = coalesce(Patients, 0L),
    Densitat = round(Patients / Total_pob * 1000, 2)
  )

# ============================================================
# 6. Prepare census polygons
# ============================================================

U_cens_sel <- U_cens %>%
  st_transform(st_crs(trams)) %>%
  mutate(
    BARRI_txt = str_pad(as.character(BARRI), width = 2, pad = "0"),
    Seccio_Censal = paste0(
      as.integer(DISTRICTE),
      str_pad(as.character(SEC_CENS), width = 3, pad = "0")
    ),
    Seccio_Censal = as.character(as.integer(Seccio_Censal))
  ) %>%
  filter(BARRI_txt %in% barrios_sel)

# ============================================================
# 7. Create map dataset
# ============================================================

map_censal <- U_cens_sel %>%
  left_join(
    Patients_adreces_cens %>%
      select(
        Seccio_Censal,
        Codi_Districte,
        Nom_Districte,
        Codi_Barri,
        Nom_Barri,
        Total_pob,
        Patients,
        Densitat
      ),
    by = "Seccio_Censal"
  ) %>%
  mutate(
    Patients = coalesce(Patients, 0L),
    Densitat = coalesce(Densitat, 0)
  )

# ============================================================
# 8. Crop street network and define map limits
# ============================================================

trams_sel <- st_crop(trams_sel, st_bbox(map_censal))

bb <- st_bbox(map_censal)

x_margin <- (bb["xmax"] - bb["xmin"]) * 0.03
y_margin <- (bb["ymax"] - bb["ymin"]) * 0.03

xlim_map <- c(bb["xmin"] - x_margin, bb["xmax"] + x_margin)
ylim_map <- c(bb["ymin"] - y_margin, bb["ymax"] + y_margin)

# ============================================================
# 9. Quality checks
# ============================================================

map_censal %>%
  st_drop_geometry() %>%
  summarise(
    n_sections = n(),
    n_sections_with_population = sum(!is.na(Total_pob)),
    n_sections_with_patients = sum(Patients > 0, na.rm = TRUE),
    total_patients = sum(Patients, na.rm = TRUE),
    max_density = max(Densitat, na.rm = TRUE)
  )

# Optional: check unmatched sections
anti_join(
  Patients_censal,
  Pob_u_censal_sel %>%
    mutate(Seccio_Censal = as.character(as.integer(Seccio_Censal))),
  by = "Seccio_Censal"
)

# ============================================================
# 10. Plot census-section density map
# ============================================================

plot_censal <- ggplot() +
  geom_sf(
    data = map_censal,
    aes(fill = Densitat),
    color = "white",
    linewidth = 0.08
  ) +
  geom_sf(
    data = trams_sel,
    color = "grey15",
    linewidth = 0.07,
    alpha = 0.50
  ) +
  geom_sf(
    data = centros_sf,
    shape = 22,
    size = 3.4,
    fill = "grey90",
    color = "black",
    stroke = 0.8
  ) +
  scale_fill_gradientn(
    colours = density_palette,
    name = "Patients per 1,000 inhabitants",
    na.value = "grey95"
  ) +
  coord_sf(
    xlim = xlim_map,
    ylim = ylim_map,
    expand = FALSE
  ) +
  guides(
    fill = guide_colorbar(
      title.position = "top",
      title.hjust = 0.5,
      barwidth = unit(8, "cm"),
      barheight = unit(0.4, "cm")
    )
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9),
    
    plot.title = element_text(size = 13, face = "bold"),
    plot.subtitle = element_text(size = 10, color = "grey30"),
    plot.margin = margin(t = 5, r = 5, b = 2, l = 5)
  ) +
  labs(
    title = "Density of patients receiving primary home-based care by census section",
    subtitle = "Patients per 1,000 inhabitants by census unit",
    x = NULL,
    y = NULL
  )

plot_censal

# ============================================================
# 11. Save figure
# ============================================================

ggsave(
  filename = here("Output", "Figures", "patients_density_census_section.png"),
  plot = plot_censal,
  width = 11.69,
  height = 8.27,
  units = "in",
  dpi = 300
)
0