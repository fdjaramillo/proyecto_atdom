# ============================================================
# 06_Plot_patients_center_location.R
# ============================================================
source(c)


nodes <- st_read(
  c,
  quiet = TRUE
)

trams <- st_read(
  here("data", "external", "BCN_GrafVial_SHP", "BCN_GrafVial_Trams_ETRS89_SHP.shp"),
  quiet = TRUE
)


Patients_locations_sf<-readRDS(here("data", "processed", "adreces_SF.rds"))

centros_sf<-readRDS(here("data", "external", "Centres_estudi_adreces_sf.rds"))

patients_sf <- Patients_locations_sf %>%
  st_as_sf(
    coords = c("lon_paciente", "lat_paciente"),
    crs = 4326,
    remove = FALSE
  ) %>%
  st_transform(st_crs(centros_sf))

trams %>%
  count(NDistric_E, sort = TRUE)

trams_sel <- trams %>%
  filter(Distric_E %in% c("05","02","04"))

nodes_sel <- nodes[st_intersects(nodes, trams_sel, sparse = FALSE) |> apply(1, any), ]


#pacients

patients_sf <- patients_sf %>%
  st_transform(st_crs(trams_sel))

centros_sf <- centros_sf %>%
  st_transform(st_crs(trams_sel))

patients_plot <- Patients_locations_sf %>%
  st_as_sf(
    coords = c("lon_paciente", "lat_paciente"),
    crs = 4326,
    remove = FALSE
  ) %>%
  st_transform(st_crs(trams_sel))

#coordenadas X/Y para la densidad

patients_xy <- patients_sf %>%
  mutate(
    x = st_coordinates(.)[, 1],
    y = st_coordinates(.)[, 2]
  ) %>%
  st_drop_geometry()

##Ploting

labels = percent_format(accuracy = 1)

bbox <- st_bbox(trams_sel)

bbox_pat <- st_bbox(patients_sf)
bbox_cent <- st_bbox(centros_sf)

###Avoid outliers in the map
xlim_map <- quantile(
  patients_xy$x,
  probs = c(0.0, 1),
  na.rm = TRUE
)

ylim_map <- quantile(
  patients_xy$y,
  probs = c(0.0, 1),
  na.rm = TRUE
)

margen <- 300

xlim_map <- c(xlim_map[1] - margen, xlim_map[2] + margen)
ylim_map <- c(ylim_map[1] - margen, ylim_map[2] + margen)

plot_map<-ggplot() +
  geom_sf(
    data = trams_sel,
    color = "black",
    linewidth = 0.12
  ) +
  stat_density_2d(
    data = patients_xy,
    aes(
      x = x,
      y = y,
      fill = after_stat(nlevel)
    ),
    geom = "polygon",
    contour = TRUE,
    alpha = 0.45,
    bins = 7,
    h = c(400, 400),
  ) +
  scale_fill_gradientn(
    colours = c("#FDE0DD", "#FCAE91", "#FB6A4A", "#DE2D26", "#A50F15"),
    name = "Relative density",
    labels = percent_format(accuracy = 1),
    values = rescale(c(0, 0.25, 0.5, 0.75, 1))
  ) +
  geom_point(
    data = patients_xy,
    aes(x = x, y = y),
    color = "black",
    alpha = 0.9,          # puedes subir la opacidad para ver mejor cada punto
    size = 0.45,
    position = position_jitter(width = 0, height = 10)
  ) +
  geom_sf(
    data = centros_sf,
    shape = 22,
    size = 3.4,
    fill = "grey90",
    color = "black",
    stroke = 0.8
  ) +
  coord_sf(
    xlim = xlim_map,
    ylim = ylim_map
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
    legend.margin = margin(t = -12, r = 0, b = 0, l = 0),
    legend.box.margin = margin(t = -14, r = 0, b = 0, l = 0),
    
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9),
    
    plot.title = element_text(size = 13, face = "bold"),
    plot.subtitle = element_text(size = 10, color = "grey30"),
    plot.margin = margin(t = 5, r = 5, b = 2, l = 5)
  ) +
  labs(
    title = "Spatial distribution of patients receiving primary home-based care",
    subtitle = "Smoothed density of residential locations and primary care centres",
    x = NULL,
    y = NULL
  )

plot_map
library(plotly)

ggsave(
  filename = here("Output","Figures", "Patients_location_300dpi.png"),
  plot = plot_map,
  width = 11.69,
  height = 8.27,
  units = "in",
  dpi = 300,
  bg = "white"
)


library(plotly)

# En el geom_point, añade text = ID
plot_map2 <- ggplot() +
  geom_sf(data = trams_sel, color = "grey5", linewidth = 0.12) +
  stat_density_2d(
    data = patients_xy,
    aes(x = x, y = y, fill = after_stat(nlevel)),
    geom = "polygon",
    contour = TRUE,
    bins = 8,
    alpha = 0.45,
    h = c(500, 500)
  ) +
  scale_fill_gradientn(
    colours = c("#FDE0DD", "#FCAE91", "#FB6A4A", "#DE2D26", "#A50F15"),
    name = "Relative density",
    labels = percent_format(accuracy = 1),
    values = rescale(c(0, 0.25, 0.5, 0.75, 1))
  ) +
  geom_point(
    data = patients_xy,
    aes(x = x, y = y, text = ID),   # añadimos text = ID
    color = "grey15",
    alpha = 0.18,
    size = 0.22
  ) +
  geom_sf(
    data = centros_sf,
    shape = 22,
    size = 3.4,
    fill = "grey90",
    color = "black",
    stroke = 0.8
  ) +
  coord_sf(xlim = xlim_map, ylim = ylim_map) +
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
    legend.margin = margin(t = -12, r = 0, b = 0, l = 0),
    legend.box.margin = margin(t = -14, r = 0, b = 0, l = 0),
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9),
    plot.title = element_text(size = 13, face = "bold"),
    plot.subtitle = element_text(size = 10, color = "grey30"),
    plot.margin = margin(t = 5, r = 5, b = 2, l = 5)
  ) +
  labs(
    title = "Spatial distribution of patients receiving primary home-based care",
    subtitle = "Smoothed density of residential locations and primary care centres",
    x = NULL,
    y = NULL
  )

# Convertir a interactivo con plotly
ggplotly(plot_map2, tooltip = "text")
