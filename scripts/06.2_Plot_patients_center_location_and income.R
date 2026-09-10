# ============================================================
# 060.2_Plot_Renta_media_u_censal.R
# ============================================================


source(here("scripts", "00_setup.R"))

# 0. Parameters

barrios_sel <- c("27", "08", "09", "20", "21", "19", "24", "25", "26", "17")
districts_sel <- c("05", "02", "04")

# 1. Load spatial data

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

ABS_sf<-st_read(
  here("data", "external", "cartografia_centres", "ABS.shp"),
  quiet = TRUE
)

# 2. Prepare street network and centres

trams_sel <- trams %>%
  filter(Distric_E %in% districts_sel)

nodes_sel <- nodes[
  st_intersects(nodes, trams_sel, sparse = FALSE) |> apply(1, any),]

ABS_sel <- ABS_sf %>%
  filter(NOMABS %in% c("Barcelona - 04A","Barcelona - 04B","Barcelona - 04C","Barcelona - 05B","Barcelona - 05A","Barcelona - 02C","Barcelona - 02E"))

centros_sf <- centros_sf %>%
  st_transform(st_crs(trams_sel))

Pob_u_censal <- read_csv(
  here("data", "external", "2024_pad_mdbas_sexe.csv"),
  show_col_types = FALSE
)

Pob_u_censal_selmap <- Pob_u_censal %>%
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
  )%>%
  select(Codi_Districte,Nom_Districte,Codi_Barri,Nom_Barri,AEB,Seccio_Censal,Total_pob)

## Renta media por hogar unidad censal data_frame

Renta_media<- read.csv2(here("data", "external", "renta_media_hogar.csv"),
                        stringsAsFactors = FALSE
)

Renta_media<-Renta_media%>%
  mutate(SEC_CENS = str_sub(as.character(Seccion_TOTAL), -4, -1)
)

Renta_media <- Renta_media %>%
  mutate(
    Seccio_Censal = paste0(
      as.integer(Distrito),
      str_pad(as.character(Seccion.Censal), width = 3, pad = "0")
    ),
    Seccio_Censal = as.character(as.integer(Seccio_Censal)),
    Media_renta_Hogar = as.numeric(Media_renta_Hogar)
  ) %>%
  select(Seccio_Censal, Media_renta_Hogar)

Patients_censal <- patients_locations_sf%>%
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

Patients_censal_renda <- Patients_censal %>%
  left_join(
    Renta_media,
    by = "Seccio_Censal")%>%
  select(Seccio_Censal,Patients,Media_renta_Hogar)

map_renta_sel <- U_cens %>%
  st_transform(st_crs(trams_sel)) %>%
  mutate(
    DISTRICTE = str_pad(as.character(DISTRICTE), width = 2, pad = "0"),
    SEC_CENS  = str_pad(as.character(SEC_CENS),  width = 3, pad = "0"),
    Seccio_Censal = paste0(as.integer(DISTRICTE), SEC_CENS),
    Seccio_Censal = as.character(as.integer(Seccio_Censal))
  ) %>%
  inner_join(Patients_censal_renda, by = "Seccio_Censal")
names(map_renta_sel)

# 6. Prepare census polygons

bb <- st_bbox(map_renta_sel)

x_margin <- as.numeric(bb["xmax"] - bb["xmin"]) * 0.03
y_margin <- as.numeric(bb["ymax"] - bb["ymin"]) * 0.03

xlim_map <- c(
  as.numeric(bb["xmin"]) - x_margin,
  as.numeric(bb["xmax"]) + x_margin
)

ylim_map <- c(
  as.numeric(bb["ymin"]) - y_margin,
  as.numeric(bb["ymax"]) + y_margin
)

bbox_plot <- st_bbox(
  c(
    xmin = xlim_map[1],
    xmax = xlim_map[2],
    ymin = ylim_map[1],
    ymax = ylim_map[2]
  ),
  crs = st_crs(map_renta_sel)
)

trams_plot <- suppressWarnings(st_crop(trams_sel, bbox_plot))
centros_plot <- suppressWarnings(st_crop(centros_sf, bbox_plot))

income_palette <- c(
  "white", "#edf8fb", "#ccece6", "#99d8c9",
  "#66c2a4", "#41ae76", "#238b45", "#005824"
)


plot_renta_sel <- ggplot() +
  geom_sf(
    data = map_renta_sel,
    aes(fill = Media_renta_Hogar),
    color = "white",
    linewidth = 0.08
  ) +
  geom_sf(
    data = trams_plot,
    color = "black",
    linewidth = 0.10,
    alpha = 0.7
  ) +
  geom_sf(
    data = ABS_sel,
    color = "black",
    linewidth = 1,
    fill = NA
  ) +
  geom_sf(
    data = centros_plot,
    shape = 22,
    size = 3.4,
    fill = "grey90",
    color = "black",
    stroke = 0.8
  ) +
  scale_fill_gradientn(
    colours = income_palette,
    name = "Mean household income (€)",
    na.value = "grey95",
    labels = scales::label_number(
      big.mark = ".",
      decimal.mark = ","
    )
  ) +
  coord_sf(
    xlim = xlim_map,
    ylim = ylim_map,
    expand = FALSE,
    clip = "on"
  ) +
  guides(
    fill = guide_colorbar(
      title.position = "bottom",
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
    legend.title = element_text(size = 9, color = "black"),
    legend.text = element_text(size = 9),
    legend.box.spacing = unit(0, "pt"),
    legend.margin = margin(0, 0, 0, 0),
    legend.box.margin = margin(0, 0, 0, 0),
    plot.title = element_text(size = 13, face = "bold"),
    plot.subtitle = element_text(size = 10, color = "grey30"),
    plot.margin = margin(t = 5, r = 5, b = 2, l = 5)
  ) +
  labs(
    title = "Mean household income by census section",
    subtitle = "Census sections with patients receiving home-based care",
    x = NULL,
    y = NULL
  )

plot_renta_sel

ggsave(
  here("Output","Figures","patients_income_census_section.png"),
  plot_renta_sel,
  width = 11.69,
  height = 8.27,
  units = "in",
  dpi = 300,
  bg = "white"
)

##Estudio relacion

Patients_censal_renda %>%
  summarise(
    n_sections = n(),
    min_patients = min(Patients, na.rm = TRUE),
    max_patients = max(Patients, na.rm = TRUE),
    mean_patients = mean(Patients, na.rm = TRUE),
    median_patients = median(Patients, na.rm = TRUE),
    min_income = min(Media_renta_Hogar, na.rm = TRUE),
    max_income = max(Media_renta_Hogar, na.rm = TRUE),
    mean_income = mean(Media_renta_Hogar, na.rm = TRUE),
    median_income = median(Media_renta_Hogar, na.rm = TRUE)
  )

income_patients<-ggplot(Patients_censal_renda, aes(x = Media_renta_Hogar, y = Patients)) +
  geom_point(size = 2.2, alpha = 0.75) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 0.8) +
  scale_x_continuous(
    labels = scales::label_number(big.mark = ".", decimal.mark = ",")
  ) +
  labs(
    title = "Relationship between household income and number of patients",
    subtitle = "Census sections included in the study area",
    x = "Mean household income (€)",
    y = "Number of patients"
  ) +
  theme_minimal()

ggsave(
  here("Output","Figures","corr_number_patients_income_census_section.png"),
  income_patients,
  width = 11.69,
  height = 8.27,
  units = "in",
  dpi = 300,
  bg = "white"
)

cor.test(
  Patients_censal_renda$Media_renta_Hogar,
  Patients_censal_renda$Patients,
  method = "spearman",
  exact = FALSE
)
cor.test(
  Patients_censal_renda$Media_renta_Hogar,
  Patients_censal_renda$Patients,
  method = "pearson"
)

model_lm <- lm(
  Patients ~ Media_renta_Hogar,
  data = Patients_censal_renda
)

summary(model_lm)
