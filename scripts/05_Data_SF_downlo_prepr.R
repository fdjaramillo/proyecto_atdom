
# 05_SF DATA DOWLOAD AND PREPARATION.R


locations<-readRDS(here("data","processed","adreces_SF.rds"))

# Download Adreces ajuntament de Barcelons SF object



## Problema amb UTS sense coordanades (borro)


# Canvi a majuscules


# Merge amb pacients


## Renta media por hogar unidad censal data_frame


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

saveRDS(
  Centres_adreces_sf,
  here("data", "external", "Centres_adreces_sf.rds")
)


Centres_estudi_adreces_sf <- Centres_adreces_sf%>%
  filter(name %in% c("Centre d'Atenció Primària Comte Borrell",
                     "Centre d'Atenció Primària Ernest Lluch",
                     "Centre d'Atenció Primària Casanova",
                     "Centre d'Atenció Primària Montnegre",
                     "Centre d'Atenció Primària Adrià")
                    )

saveRDS(
  Centres_estudi_adreces_sf,
  here("data", "external", "Centres_estudi_adreces_sf.rds")
)

#Pacients a centre amb dades SF per a routes.

Patients_locations <- Patients_locations %>%
  st_drop_geometry() %>%
  transmute(
    ID = ID,
    codi_carrer = codi_carrer,
    nom_carrer = nom_carrer,
    numero_Carrer = numpost_i,
    secc_censal = secc_cens,
    districte = districte,
    barri = barri,
    nom_barri = nom_barri,
    lon_paciente = as.numeric(longitud_wgs84),
    lat_paciente = as.numeric(latitud_wgs84)
  )

Center_location <- Center_location %>%
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

df_pacients<- readRDS("data/processed/df_cleaned.rds")

df_pacients<-df_pacients%>%
  select(ID,USUA_UAB_UP)%>%
  mutate(Centre_ID= case_when(USUA_UAB_UP=="Borrell"~ "99400282464",
                              USUA_UAB_UP=="Casanova"~ "92086002684",
                              USUA_UAB_UP=="Montnegre_1"~ "93056132443",
                              USUA_UAB_UP=="Montnegre_2"~ "93056132443",
                              USUA_UAB_UP=="Marc_Aureli"~ "92086002931",
                              USUA_UAB_UP=="Sant_Elies"~ "92086002931",
                              USUA_UAB_UP=="Lluch"~ "94354121938"))


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

