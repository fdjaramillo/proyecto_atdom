  
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

#Load  
Centres_adreces <- read.csv(
    "data/external/centres.csv",
    fileEncoding = "UTF-16LE"
  )

#Transform into SF
Centres_adreces_sf <- st_as_sf(
    Centres_adreces,
    coords = c("geo_epgs_25831_x", "geo_epgs_25831_y"),
    crs = 25831,
    remove = FALSE
  )

#Select centres estudi  
Centres_estudi_adreces_sf <- Centres_adreces_sf%>%
    filter(name %in% c("Centre d'Atenció Primària Comte Borrell",
                       "Centre d'Atenció Primària Ernest Lluch",
                       "Centre d'Atenció Primària Casanova",
                       "Centre d'Atenció Primària Montnegre",
                       "Centre d'Atenció Primària Adrià")
                      )
#Recode Lluch
Lluch_exacto <- st_sfc(st_point(c(2.1249939903309873, 41.383859217910526)), crs = 4326)
Lluch_exacto_utm <- st_transform(Lluch_exacto, crs = 25831)
nuevas_coords <- st_coordinates(Lluch_exacto_utm)
  
# Crear el nuevo punto en UTM
nuevo_punto <- st_transform(Lluch_exacto, crs = 25831)
Centres_estudi_adreces_sf[2, ] <- st_set_geometry(Centres_estudi_adreces_sf[2, ], nuevo_punto)
  
# Actualizar coordenadas
  coords <- st_coordinates(nuevo_punto)
  Centres_estudi_adreces_sf[2, "geo_epgs_25831_x"] <- coords[1, "X"]
  Centres_estudi_adreces_sf[2, "geo_epgs_25831_y"] <- coords[1, "Y"]
  Centres_estudi_adreces_sf[2, "geo_epgs_4326_lat"] <- 41.383859217910526
  Centres_estudi_adreces_sf[2, "geo_epgs_4326_lon"] <- 2.1249939903309873
  
saveRDS(
    Centres_estudi_adreces_sf,
    here("data", "SF", "Centres_estudi_adreces_sf.rds"))
  
#Pacients a centre amb dades SF per a routes.

Patients_locations<-readRDS(here("data", "SF", "BCN_adreces_users_SF_included_Data_table.rds"))

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

Center_location <- readRDS(here("data", "SF", "Centres_estudi_adreces_sf.rds"))%>%
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
            by = "Centre_ID")

saveRDS(
  pacients_adreces_i_centre,
  here("data", "SF", "pacients_i_centre_sf_Data_Table.rds"))



