# ============================================================
# 06_Plot_patients_center_location.R
# ============================================================
source(here("scripts", "00_setup.R"))


nodes <- st_read(
  here("data", "external", "BCN_GrafVial_SHP", "BCN_GrafVial_Nodes_ETRS89_SHP.shp"),
  quiet = TRUE
)

trams <- st_read(
  here("data", "external", "BCN_GrafVial_SHP", "BCN_GrafVial_Trams_ETRS89_SHP.shp"),
  quiet = TRUE
)

st_crs(nodes)
st_geometry_type(nodes)
plot(st_geometry(nodes))

trams <- st_read(
  here("data", "external", "BCN_GrafVial_SHP", "BCN_GrafVial_Trams_ETRS89_SHP.shp"),
  quiet = TRUE
)
plot(st_geometry(trams))
