  
# ============================================================
# 01.1 SF DATA PREP.R
# ============================================================ 

library(here)  
source(here("scripts", "00_0 setup.R"))

# Coordenades Centres

#URL  
centres<- "https://opendata-ajuntament.barcelona.cat/data/dataset/b959dce3-4862-4697-a158-63f8b15ed4f3/resource/9e135848-eb0a-4bc5-8e60-de558213b3ed/download"

#Download  
download.file(
    url = centres,
    destfile = "data/external/centres.csv",
    mode = "wb")

#1. CARGA DE LAS COORDENADAS DE LOS CENTROS 
Centres_adreces <- read.csv(
    "data/external/centres.csv",
    fileEncoding = "UTF-16LE"
  )

# 2. CONVERSIÓN A OBJETO ESPACIAL SF
Centres_adreces_sf <- st_as_sf(
    Centres_adreces,
    coords = c("geo_epgs_25831_x", "geo_epgs_25831_y"),
    crs = 25831,
    remove = FALSE
  )

# 3. SELECCIÓN DE LOS CENTROS INCLUIDOS EN EL ESTUDIO 
Centres_estudi_adreces_sf <- Centres_adreces_sf%>%
    filter(name %in% c("Centre d'Atenció Primària Comte Borrell",
                       "Centre d'Atenció Primària Ernest Lluch",
                       "Centre d'Atenció Primària Casanova",
                       "Centre d'Atenció Primària Montnegre",
                       "Centre d'Atenció Primària Adrià")
                      )
# 4. CORRECCIÓN DE LA LOCALIZACIÓN DEL CAP ERNEST LLUCH
Lluch_exacto <- st_sfc(st_point(c(2.1249939903309873, 41.383859217910526)), crs = 4326)
Lluch_exacto_utm <- st_transform(Lluch_exacto, crs = 25831)

# 5. IDENTIFICACIÓN DEL CENTRO ERNEST LLUCH
idx_lluch <- which(
  Centres_estudi_adreces_sf$name ==
    "Centre d'Atenció Primària Ernest Lluch"
)

# 6. ACTUALIZACIÓN DE LA GEOMETRÍA

st_geometry(Centres_estudi_adreces_sf)[idx_lluch] <-
  Lluch_exacto_utm

# 7. ACTUALIZACIÓN DE LAS COLUMNAS NUMÉRICAS DE COORDENADAS
  
coords_lluch <- st_coordinates(Lluch_exacto_utm)

Centres_estudi_adreces_sf$geo_epgs_25831_x[idx_lluch] <-
  coords_lluch[1, "X"]

Centres_estudi_adreces_sf$geo_epgs_25831_y[idx_lluch] <-
  coords_lluch[1, "Y"]

Centres_estudi_adreces_sf$geo_epgs_4326_lat[idx_lluch] <-
  41.383859217910526

Centres_estudi_adreces_sf$geo_epgs_4326_lon[idx_lluch] <-
  2.1249939903309873

Centres_estudi_adreces_sf <- Centres_estudi_adreces_sf %>%
  mutate(Home_based_PHC_org = case_when(
    register_id == "﻿94354121938" ~ "Equip_Atdom",
    register_id == "﻿99499400282464" ~ "Equip_Atdom",
    register_id == "﻿93056132443" ~ "UAB_consulta",
    register_id == "﻿92086002684" ~ "Equip_Inf",
    register_id == "﻿02931" ~ "UAB_consulta_reforc",
    TRUE ~ NA_character_
  ))

saveRDS(Centres_estudi_adreces_sf, here("data", "SF", "Centres_estudi_SF.rds"))

##Unitats censals inclonses

#URL  
Unitats_censals<- "https://opendata-ajuntament.barcelona.cat/data/dataset/808daafa-d9ce-48c0-925a-fa5afdb1ed41/resource/e16856a7-b3c0-4c32-a468-cc190cbbf7a9/download"

#Download  
download.file(
  url = Unitats_censals,
  destfile = "data/external/unitats_censals.csv",
  mode = "wb")

#Carrega

unitats_censals<- read_csv("data/external/unitats_censals.csv")%>%
  select(-geometria_wgs84)%>%
  mutate(
    Seccio_Censal = paste0(
      as.integer(codi_districte),
      str_pad(codi_seccio_censal, width = 3, pad = "0")
    ),
    Seccio_Censal = as.character(as.integer(Seccio_Censal))
  )

# CONVERSIÓN A OBJETO ESPACIAL SF
unitats_censals_sf <- unitats_censals %>%
  st_as_sf(
    wkt = "geometria_etrs89",
    crs = 25831
  )

ABS_sf<-readRDS(here("data", "SF", "ABS_sel_SF.rds")
)

# Selección: cualquier UC que intersecte algún ABS

unitats_censals_estudi_sf <- unitats_censals_sf[
  lengths(st_intersects(unitats_censals_sf, ABS_sf)) > 0,
]


# Asignación del ABS: usando punto interior
uc_points <- st_point_on_surface(unitats_censals_estudi_sf)

uc_abs <- st_join(uc_points,
  ABS_sf %>% select(CODABSa, NOMABS),
  join = st_within,
  left = TRUE
)

uc_abs <- uc_abs %>%
  mutate(Home_based_PHC_org = case_when(
    NOMABS %in% c("Barcelona - 02C", "Barcelona - 04C") ~ "Equip_Atdom",
    NOMABS %in% c("Barcelona - 04A", "Barcelona - 04B") ~ "UAB_consulta",
    NOMABS == "Barcelona - 02E" ~ "Equip_Inf",
    NOMABS %in% c("Barcelona - 05A", "Barcelona - 05B") ~ "UAB_consulta_reforc",
    TRUE ~ NA_character_
  )
)%>%
  st_drop_geometry()

#Merge with included

unitats_censals_estudi_sf<-unitats_censals_estudi_sf%>%
  inner_join(uc_abs[,c(5,7,8,9,10)],
             by="Seccio_Censal")

saveRDS(unitats_censals_estudi_sf,
  here("data", "SF", "unitats_censals_estudi_sf.rds"))

### Pacients a centre amb dades SF per a routes. ###

Patients_locations<-readRDS(here("data", "SF", "BCN_adreces_users_SF_included_Data_table.rds"))%>%
  filter(!ID %in% c(483, 1312, 1919, 2241))

Patients_locations <- Patients_locations %>%
    transmute(
      ID = ID,
      nom_carrer = nom_carrer,
      numero_Carrer = numpost_i,
      secc_censal = Seccio_Censal,
      districte = districte,
      barri = barri,
      nom_barri = nom_barri,
      USUA_UAB_UP = NOMABS,
      geo_epgs_25831_x = as.numeric(x_etrs89),
      geo_epgs_25831_y = as.numeric(y_etrs89),
      geo_epgs_4326_lon = as.numeric(longitud_wgs84),
      geo_epgs_4326_lat = as.numeric(latitud_wgs84)
    )%>%
mutate(Centre_ID= case_when(USUA_UAB_UP=="Barcelona - 02C"~ "99400282464",
                            USUA_UAB_UP=="Barcelona - 02E"~ "92086002684",
                            USUA_UAB_UP=="Barcelona - 04A"~ "93056132443",
                            USUA_UAB_UP=="Barcelona - 04B"~ "93056132443",
                            USUA_UAB_UP=="Barcelona - 05A"~ "92086002931",
                            USUA_UAB_UP=="Barcelona - 05B"~ "92086002931",
                            USUA_UAB_UP=="Barcelona - 04C"~ "94354121938"))

Center_location <- readRDS(here("data", "SF", "Centres_estudi_SF.rds"))%>%
    st_drop_geometry() %>%
    transmute(
      Centre_ID = register_id,
      Nom_centre = name,
      codi_carrer = addresses_road_id,
      nom_carrer = addresses_road_name,
      numero_Carrer = addresses_start_street_number,
      districte_centre_id = addresses_district_id,
      districte = addresses_district_name,
      barri_centre_id = addresses_neighborhood_id,
      barri = addresses_neighborhood_name,
      CP_centre = addresses_zip_code,
      x_centro_etrs89 = as.numeric(geo_epgs_25831_x),
      y_centro_etrs89 = as.numeric(geo_epgs_25831_y),
      geo_epgs_centro_4326_lon = as.numeric(geo_epgs_4326_lon),
      geo_epgs_centro_4326_lat = as.numeric(geo_epgs_4326_lat)
    )
  
Center_location <- Center_location %>%
    mutate(
      Centre_ID = str_replace_all(Centre_ID, "[^0-9]", ""))


pacients_adreces_i_centre <- Patients_locations%>%
  left_join(Center_location[,c(1,11:14)],
            by = "Centre_ID")%>%
  rename(Seccio_Censal=secc_censal)%>%
  filter(!ID %in% c(483, 1312, 1919, 2241)) ###no se dispone de datos ATDOM desde el DF_pacientes_inicial.RDS


saveRDS(
  pacients_adreces_i_centre,
  here("data", "SF", "pacients_i_centre_sf_Data_Table.rds"))


