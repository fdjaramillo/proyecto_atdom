ABS_sel<-readRDS(here("data", "SF", "ABS_sel_SF.rds"))%>%
  filter(NOMABS %in% c("Barcelona - 02C","Barcelona - 02E","Barcelona - 04C"))
)

nodes <- st_read(here("data", "external", "BCN_GrafVial_SHP", "BCN_GrafVial_Nodes_ETRS89_SHP.shp"),
                 quiet = TRUE)

trams <- st_read(
  here("data", "external", "BCN_GrafVial_SHP", "BCN_GrafVial_Trams_ETRS89_SHP.shp"),
  quiet = TRUE)

centros_sf<-readRDS(here("data", "external", "Centres_estudi_adreces_sf.rds"))%>%
filter(name %in% c("Centre d'Atenció Primària Comte Borrell",
                   "Centre d'Atenció Primària Ernest Lluch",
                   "Centre d'Atenció Primària Casanova"))

trams_sel <- trams %>%
  filter(Distric_E %in% c("05","02","04"))

nodes_sel <- nodes[st_intersects(nodes, trams_sel, sparse = FALSE) |> apply(1, any), ]


plot_map<-ggplot() +
  geom_sf(
    data = trams_sel,
    color = "black",
    linewidth = 0.10,
    alpha = 0.7
  ) +
  
  # PHC centres
  geom_sf(
    data = centros_sf,
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
    fill = c(
      "#0057B8",
      "#00875A",
      "#7B2CBF"
    ),
    alpha = 0.5,
    show.legend = FALSE
  ) +
  
  # Colors for PHC area boundaries
  scale_color_manual(
    values = c(
      "#0057B8",
      "#00875A",
      "#7B2CBF"
    )
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
    
    plot.title = element_text(size = 13, face = "bold"),
    plot.subtitle = element_text(size = 10, color = "grey30"),
    plot.margin = margin(t = 5, r = 5, b = 2, l = 5)
  ) +
  labs(
    title = "CAPSBE PHC centers",
    subtitle = NULL,
    x = NULL,
    y = NULL
  )

plot_map
