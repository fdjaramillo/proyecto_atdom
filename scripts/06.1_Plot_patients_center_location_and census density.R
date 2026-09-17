# ============================================================
# 060.1_Plot_patients_center_location_and_census_place.R
# ============================================================


source(here("scripts", "00_0 setup.R"))

# 0. Parameters

barrios_sel <- c("27", "08", "09", "20", "21", "19", "24", "25", "26", "17")
districts_sel <- c("05", "02", "04")

density_palette <- c(
  "white", "#fee0d2", "#fcbba1", "#fc9272",
  "#fb6a4a", "#ef3b2c", "#cb181d", "#99000d","#4d0000"
)

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

tasas_cobertura_atdom<-readRDS(here("data","processed","Age_standarized_ATDOM_U_CENSAL.rds"))

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

# 8. Crop street network and define map limits

map_visible <- map_censal %>%
  filter(!is.na(tasa_atdom_std_1000) & tasa_atdom_std_1000 > 0)

# 1. Definir bbox solo del área que quieres mostrar
bb <- st_bbox(map_visible)

x_margin <- as.numeric(bb["xmax"] - bb["xmin"]) * 0.04
y_margin <- as.numeric(bb["ymax"] - bb["ymin"]) * 0.04

bbox_plot <- st_bbox(
  c(
    xmin = as.numeric(bb["xmin"]) - x_margin,
    xmax = as.numeric(bb["xmax"]) + x_margin,
    ymin = as.numeric(bb["ymin"]) - y_margin,
    ymax = as.numeric(bb["ymax"]) + y_margin
  ),
  crs = st_crs(map_censal)
)

# Recortar DE VERDAD todos los objetos que entran en el mapa

map_censal_plot <- suppressWarnings(st_crop(map_censal, bbox_plot))
trams_plot <- suppressWarnings(st_crop(trams_sel, bbox_plot))
centros_plot <- suppressWarnings(st_crop(centros_sf, bbox_plot))

# Calcular límites desde el bbox recortado

xlim_map <- c(
  as.numeric(bbox_plot["xmin"]),
  as.numeric(bbox_plot["xmax"])
)

ylim_map <- c(
  as.numeric(bbox_plot["ymin"]),
  as.numeric(bbox_plot["ymax"])
)

# 9. Quality checks
names(map_censal_plot)

# 10. Plot census-section density map
st_bbox(map_censal)
st_bbox(map_censal_plot)

plot_censal <- ggplot() +
  geom_sf(
    data = map_censal_plot,
    aes(fill = tasa_atdom_std_1000),
    color = "white",
    linewidth = 0.08
  ) +
  geom_sf(
    data = trams_plot ,
    color = "grey30",
    linewidth = 0.10,
    alpha = 0.7
  ) +
  geom_sf(
    data = centros_plot,
    shape = 21,
    size = 1.8,
    fill = "white",
    color = "black",
    stroke = 0.5
  ) +
  geom_sf(
    data = ABS_sel,
    aes(color = NOMABS),
    linewidth = 1.1,
    fill = NA,
    show.legend = FALSE
  ) +
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
  )+
  scale_fill_gradientn(
    colours = density_palette,
    name = "Age-standardized rate per 1,000 inhabitants",
    limits = c(0, 32),
    breaks = seq(0, 32, by = 4),
    oob = squish,
    na.value = "grey95"
  ) +
  coord_sf(
    xlim = xlim_map,
    ylim = ylim_map,
    expand = FALSE,
    clip = "on"
  ) +
  guides(
    color = "none",
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
    legend.title = element_text(size = 9,color = "black"),
    legend.text = element_text(size = 9),
    legend.box.margin = margin(t = 1, r = 0, b = 0, l = 0),
    legend.margin = margin(t = 1, r = 0, b = 0, l = 0),
    legend.box.spacing = unit(2, "pt"),
    plot.caption = element_text(size = 8,color = "black",hjust = 0, face="italic"),
    
    plot.title = element_text(size = 13, face = "bold"),
    plot.subtitle = element_text(size = 10, color = "grey30"),
    plot.margin = margin(t = 5, r = 5, b = 2, l = 5)
  ) +
  labs(
    title = "Age-standardized ATDOM enrolment rate by census section",
    subtitle = "Spatial distribution across primary care catchment areas",
    caption = "Colored boundaries indicate the limits of the seven primary health care areas.",
    x = NULL,
    y = NULL
  )

plot_censal

map_censal %>%
  st_drop_geometry() %>%
  filter(tasa_atdom_std_1000 >= 28) %>%
  select(
    Seccio_Censal,
    NOMABS,
    tasa_atdom_std_1000
  ) %>%
  arrange(desc(tasa_atdom_std_1000))

tasas_cobertura_atdom_long<-readRDS(here("data","processed","Age_categorized_long_ATDOM_U_CENSAL"))
names(tasas_cobertura_atdom_long)

print(n=34,Pob_u_censal_age %>%
  filter(
    Seccio_Censal == "4049",
    EDAT_1 >= 85
  ) %>%
  arrange(EDAT_1) %>%
  select(
    Seccio_Censal,
    EDAT_1,
    Valor
  ))

tasas_cobertura_atdom_long %>%
  filter(Seccio_Censal == "4049") %>%
  left_join(
    pesos_edad,
    by = "edat_cat"
  ) %>%
  mutate(
    contribucion = tasa_atdom_1000 * peso_edad
  ) %>%
  select(
    edat_cat,
    poblacion_total,
    n_atdom,
    tasa_atdom_1000,
    peso_edad,
    contribucion
  )

sapply(
  list(
    map_censal_plot = map_censal_plot,
    trams_plot = trams_plot,
    ABS_sf = ABS_sf,
    centros_plot = centros_plot
  ),
  function(x) st_crs(x)$epsg
)

objetos <- list(
  map_censal_plot = map_censal_plot,
  trams_plot       = trams_plot,
  ABS_sf           = ABS_sf,
  centros_plot     = centros_plot
)

names(map_censal_plot)

# Ver CRS completo
map_censal_plot %>%
  st_drop_geometry() %>%
  select(
    nom_districte,
    NOMABS,
    Seccio_Censal
  ) %>%
  head(20)

# 11. Save figure

ggsave(
  here("Output","Figures","patients_density_census_section.png"),
  plot_censal,
  width = 11.69,
  height = 8.27,
  units = "in",
  dpi = 300,
  bg = "white"
)
