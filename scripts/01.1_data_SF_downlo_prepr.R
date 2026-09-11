  
# ============================================================
# 01.1_SF DATA DOWLOAD AND PREPARATION.R
# ============================================================ 

library(here)  
source(here("scripts", "00_setup.R"))

# Coordenades Centres
  
  centres<- "https://opendata-ajuntament.barcelona.cat/data/dataset/b959dce3-4862-4697-a158-63f8b15ed4f3/resource/9e135848-eb0a-4bc5-8e60-de558213b3ed/download"
  
  download.file(
    url = centres,
    destfile = "data/external/centres.csv",
    mode = "wb"
  )
  
  Centres_adreces <- read.csv(
    "data/external/centres.csv",
    fileEncoding = "UTF-16LE"
  )
  
  Centres_adreces_sf <- st_as_sf(
    Centres_adreces,
    coords = c("geo_epgs_25831_x", "geo_epgs_25831_y"),
    crs = 25831,
    remove = FALSE
  )
  
  Centres_estudi_adreces_sf <- Centres_adreces_sf%>%
    filter(name %in% c("Centre d'Atenció Primària Comte Borrell",
                       "Centre d'Atenció Primària Ernest Lluch",
                       "Centre d'Atenció Primària Casanova",
                       "Centre d'Atenció Primària Montnegre",
                       "Centre d'Atenció Primària Adrià")
                      )
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
    here("data", "external", "Centres_estudi_adreces_sf.rds")
  )
  
  #Pacients a centre amb dades SF per a routes.

Patients_locations <- BCN_adreces_users_SF %>%
    st_drop_geometry() %>%
  filter(included == "included") 
    transmute(
      ID = ID,
      codi_carrer = codi_carrer,
      nom_carrer = nom_carrer,
      numero_Carrer = USUA_NUMERO,
      secc_censal = Seccio_Censal,
      districte = districte,
      barri = barri,
      nom_barri = nom_barri,
      lon_paciente = as.numeric(longitud_wgs84),
      lat_paciente = as.numeric(latitud_wgs84)
    )
  
  Center_location <- Centres_estudi_adreces_sf %>%
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
      lon_centro = as.numeric(geo_epgs_4326_lon),
      lat_centro = as.numeric(geo_epgs_4326_lat)
    )
  
  df_pacients<- readRDS( here("data","Tables_DB","TB_pacientes.RDS"))
  
  df_pacients<-df_pacients%>%
    select(ID,USUA_UAB_UP)%>%
    mutate(Centre_ID= case_when(USUA_UAB_UP=="00460"~ "99400282464",
                                USUA_UAB_UP=="00462"~ "92086002684",
                                USUA_UAB_UP=="00474"~ "93056132443",
                                USUA_UAB_UP=="00475"~ "93056132443",
                                USUA_UAB_UP=="00477"~ "92086002931",
                                USUA_UAB_UP=="00478"~ "92086002931",
                                USUA_UAB_UP=="01004"~ "94354121938"))
  
  Center_location <- Center_location %>%
    mutate(
      Centre_ID = str_replace_all(Centre_ID, "[^0-9]", ""))
  
  Patients_locations <- Patients_locations %>%
    left_join(
      df_pacients %>% select(ID, Centre_ID),
      by = "ID") %>%
    left_join(
      Center_location %>%
        select(Centre_ID, Nom_centre, lon_centro, lat_centro),
      by = "Centre_ID"
    )
  
  saveRDS(
    Patients_locations,
    here("data", "processed", "pacients_adreces_i_centre_sf.rds")
  )
  
