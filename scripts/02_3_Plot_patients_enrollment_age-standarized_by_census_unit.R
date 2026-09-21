# ============================================================
# 060.1_Plot_patients_center_location_and_census_place.R
# ============================================================

source(here("scripts", "00_0 setup.R"))

# 0. Parameters

barrios_sel <- c("27", "08", "09", "20", "21", "19", "24", "25", "26", "17")
districts_sel <- c("05", "02", "04")

# 1. Load spatial data

ABS_sel<-readRDS(here("data", "SF", "ABS_sel_SF.rds"))

nodes <- st_read(here("data", "external", "BCN_GrafVial_SHP", "BCN_GrafVial_Nodes_ETRS89_SHP.shp"),
                 quiet = TRUE)

trams <- st_read(
  here("data", "external", "BCN_GrafVial_SHP", "BCN_GrafVial_Trams_ETRS89_SHP.shp"),
  quiet = TRUE)

U_cens <- readRDS(
  here("data", "SF", "unitats_censals_estudi_sf.rds"))

centros_sf <- readRDS(
  here("data", "SF", "Centres_estudi_SF.rds")
)

tasas_cobertura_atdom<-readRDS(here("data","processed","tasas_atdom_estandarizadas.rds"))


# 2. Prepare street network and centres

trams_sel <- trams %>%
  filter(Distric_E %in% districts_sel)

nodes_sel <- nodes[lengths(st_intersects(nodes, trams_sel)) > 0, ]

# Prepare census unit polygons

U_cens_sel <- U_cens 

unitats_censals_plot <- st_intersection(
  U_cens_sel,
  st_union(ABS_sel)
)

# 7. Create map dataset

map_censal <- unitats_censals_plot %>%
  left_join(
    tasas_cobertura_atdom,
    by = "Seccio_Censal"
  ) %>%
  select(
    Seccio_Censal,
    nom_districte,
    nom_barri,
    CODABSa,
    NOMABS,
    tasa_atdom_std_1000
  )


# 8. PREPARAR OBJETOS ESPACIALES Y DEFINIR EXTENSIÓN DEL MAPA

# El encuadre se define a partir de las 7 áreas de atención primaria
bb <- st_bbox(ABS_sel)

# Margen adicional alrededor del área de estudio
x_margin <- as.numeric(bb["xmax"] - bb["xmin"]) * 0.04
y_margin <- as.numeric(bb["ymax"] - bb["ymin"]) * 0.04

# Bounding box final para el mapa
bbox_plot <- st_bbox(
  c(
    xmin = as.numeric(bb["xmin"]) - x_margin,
    xmax = as.numeric(bb["xmax"]) + x_margin,
    ymin = as.numeric(bb["ymin"]) - y_margin,
    ymax = as.numeric(bb["ymax"]) + y_margin
  ),
  crs = st_crs(ABS_sel)
)

# Límites para ggplot
xlim_map <- c(
  as.numeric(bbox_plot["xmin"]),
  as.numeric(bbox_plot["xmax"])
)

ylim_map <- c(
  as.numeric(bbox_plot["ymin"]),
  as.numeric(bbox_plot["ymax"])
)

# Convertir bbox a geometría para seleccionar polígonos
bbox_sf <- st_as_sfc(bbox_plot)

# ------------------------------------------------------------
# Census sections:
# seleccionar las que intersectan el área de representación,
# pero mantener sus geometrías completas
# ------------------------------------------------------------

map_censal_plot <- map_censal[
  lengths(st_intersects(map_censal, bbox_sf)) > 0,
]

# ------------------------------------------------------------
# Street network:
# aquí sí se puede recortar físicamente
# ------------------------------------------------------------

trams_plot <- suppressWarnings(
  st_crop(trams_sel, bbox_plot)
)

# ------------------------------------------------------------
# PHC centres:
# mantener geometrías completas
# ------------------------------------------------------------

centros_plot <- centros_sf

# 10. PALETA DE COLOR PARA LAS TASAS

density_palette <- c(
  "white",
  "#fee0d2",
  "#fcbba1",
  "#fc9272",
  "#fb6a4a",
  "#ef3b2c",
  "#cb181d",
  "#99000d",
  "#4d0000"
)

# 11. MAPA

plot_censal <- ggplot() +
  
  # Census sections
  geom_sf(
    data = map_censal_plot,
    aes(fill = tasa_atdom_std_1000),
    color = "white",
    linewidth = 0.08
  ) +
  
  # Street network
  geom_sf(
    data = trams_plot,
    color = "grey30",
    linewidth = 0.10,
    alpha = 0.7
  ) +
  
  # PHC centres
  geom_sf(
    data = centros_plot,
    shape = 21,
    size = 1.8,
    fill = "white",
    color = "black",
    stroke = 0.5
  ) +
  
  # PHC area boundaries
  geom_sf(
    data = ABS_sel,
    aes(color = NOMABS),
    linewidth = 1.1,
    fill = NA,
    show.legend = FALSE
  ) +
  
  # Colors for PHC area boundaries
  scale_color_manual(
    values = c(
      "#0057B8",
      "#00875A",
      "#7B2CBF",
      "#00A6D6",
      "#5A189A",
      "#008C95",
      "#3A3A3A"
    )
  ) +
  
  # Age-standardized ATDOM rate
  scale_fill_gradientn(
    colours = density_palette,
    name = "Age and sex standardized rate per 1,000 inhabitants",
    limits = c(0, 24),
    breaks = seq(0, 24, by = 3),
    oob = scales::squish,
    na.value = "grey95"
  ) +
  
  # Map limits
  coord_sf(
    xlim = xlim_map,
    ylim = ylim_map,
    expand = T,
    clip = "on"
  ) +
  
  # Legend
  guides(
    color = "none",
    fill = guide_colorbar(
      title.position = "top",
      title.hjust = 0.5,
      barwidth = unit(8, "cm"),
      barheight = unit(0.4, "cm")
    )
  ) +
  
  # Theme
  theme_minimal() +
  
  theme(
    panel.grid = element_blank(),
    
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_text(
      size = 9,
      color = "black"
    ),
    legend.text = element_text(
      size = 9
    ),
    legend.box.margin = margin(
      t = 1,
      r = 0,
      b = 0,
      l = 0
    ),
    legend.margin = margin(
      t = 1,
      r = 0,
      b = 0,
      l = 0
    ),
    legend.box.spacing = unit(2, "pt"),
    
    plot.caption = element_text(
      size = 8,
      color = "black",
      hjust = 0,
      face = "italic"
    ),
    
    plot.title = element_text(
      size = 14,
      face = "bold",
      color = "black"
    ),
    
    plot.subtitle = element_text(
      size = 11,
      color = "black"
    ),
    
    plot.margin = margin(
      t = 5,
      r = 5,
      b = 2,
      l = 5
    )
  ) +
  
  # Labels
  labs(
    title = "Age and sex standardized rate of enrolment in primary care home-based care by census section",
    subtitle = "Spatial distribution across seven primary care areas in Barcelona",
    caption = "Colored boundaries indicate the limits of the seven primary health care areas.",
    x = NULL,
    y = NULL
  )

# Mostrar mapa
plot_censal


# 11. Save figure

ggsave(
  here("Output","Figures","Enrrollment_age_and sex standarized_by_census_section.png"),
  plot_censal,
  width = 11.69,
  height = 8.27,
  units = "in",
  dpi = 300,
  bg = "white"
)
