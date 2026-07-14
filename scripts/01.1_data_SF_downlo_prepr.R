
# ============================================================
# 01.1_SF DATA DOWLOAD AND PREPARATION.R


source(here("scripts", "00_setup.R"))

# Download Adreces ajuntament de Barcelons SF object

Adreces<- "https://opendata-ajuntament.barcelona.cat/data/dataset/25752522-3528-4c14-b68d-5f09a3e393bd/resource/661fe190-67c8-423a-b8eb-8140f547fde2/download"

download.file(
  url = Adreces,
  destfile = "data/adreces.csv",
  mode = "wb"
)

BCN_adreces<- st_read("data/adreces.csv")

### Problema amb UTS sense coordanades (borro) i alguns canvis inicials de majuscules

# Limpieza inicial:
# - Filtre barris de referència
# - eliminar registros sin coordenadas
# - convertir coordenadas a numéricas
# - normalizar nombre de calle
# - crear USUA_NUMERO a partir de numpost_i

BCN_adreces <- BCN_adreces %>%
  st_drop_geometry() %>%
  filter(barri %in% c("27","08","09","20","21","19","24","25","26","17"))%>%
  mutate(
    # Coordenadas
    x_etrs89 = as.numeric(na_if(as.character(x_etrs89), "")),
    y_etrs89 = as.numeric(na_if(as.character(y_etrs89), "")),
    longitud_wgs84 = as.numeric(na_if(as.character(longitud_wgs84), "")),
    latitud_wgs84 = as.numeric(na_if(as.character(latitud_wgs84), "")),
    
    # Nombre de calle normalizado
    nom_carrer = normaliza_carrer(nom_carrer),
    
    # Rango de numeración del portal
    num_i = as.integer(str_remove(as.character(numpost_i), "^0+")),
    num_f = as.integer(str_remove(as.character(numpost_f), "^0+")),
    
    # Si numpost_f está vacío, asumimos portal exacto
    num_f = if_else(is.na(num_f), num_i, num_f)
  ) %>%
  filter(
    !is.na(nom_carrer),
    nom_carrer != "",
    !is.na(num_i),
    !is.na(num_f),
    !is.na(x_etrs89),
    !is.na(y_etrs89)
  ) %>%
  select(
    nom_carrer,
    num_i,
    num_f,
    codi_carrer,
    districte,
    nom_districte,
    barri,
    nom_barri,
    secc_est,
    secc_cens,
    x_etrs89,
    y_etrs89,
    longitud_wgs84,
    latitud_wgs84
  ) %>%
  as.data.table()

BCN_adreces <- BCN_adreces[
  ,
  .(
    codi_carrer = first(codi_carrer),
    districte = first(districte),
    nom_districte = first(nom_districte),
    barri = first(barri),
    nom_barri = first(nom_barri),
    secc_est = first(secc_est),
    secc_cens = first(secc_cens),
    x_etrs89 = x_etrs89,
    y_etrs89 = y_etrs89,
    longitud_wgs84 = longitud_wgs84,
    latitud_wgs84 = latitud_wgs84,
    n_adreces_originals = .N
  ),
  by = .(nom_carrer, num_i, num_f)
]

BCN_adreces<- st_as_sf(
  BCN_adreces,
  coords = c("x_etrs89", "y_etrs89"),
  crs = 25831,
  remove = FALSE
)

## Carrers pacients

User_adreces<- readRDS(here("data", "external", "id_adreces_ID.rds")
)

User_adreces <- User_adreces %>%
  mutate(
    nom_carrer = normaliza_carrer(USUA_CARRER),
    USUA_NUMERO = as.numeric(USUA_NUMERO)
  )

### Problemes de codificació dels carrers en DF EHR hacer igual que la denominación original.

equivalencias_carrers <- tribble(
  ~nom_carrer_user,                    ~nom_carrer_bcn,
  
  "CADIS",                             "CADIS",
  "AGUSTINA DE SARAGOSSA",             "AGUSTINA SARAGOSSA",
  "BERTRAN I ROZPIDE",                 "BELTRAN I ROZPIDE",
  "JOAN SEBASTIA BACH",                "JOHANN SEBASTIAN BACH",
  "DE CARLES III",                     "CARLES III",
  "DE LES CORTS",                      "CORTS",
  "SOR EULALIA ANZIZU",                "SOR EULALIA D ANZIZU",
  "PRESIDENT JOSEP IRLA I BOSCH",      "JOSEP IRLA I BOSCH",
  "RICARD CALVO",                      "RICARDO CALVO",
  "DELS SEGADORS",                     "SEGADORS",
  "SANT GENIS",                        "SANT GENIS A HORTA",
  "MESTRE ANTONI NICOLAU",             "MESTRE NICOLAU",
  "LA TORRE",                          "TORRE",
  "LA ALFAMBRA",                       "ALFAMBRA",
  "ALFONS I",                          "ALFONS XII",
  "CIUTAT BALAGUER",                   "CIUTAT DE BALAGUER",
  "COMTE SALVATIERRA",                 "COMTE DE SALVATIERRA",
  "DOMINGUEZ MIRALLES",                "DOMINGUEZ I MIRALLES",
  "D'EN PUJOL",                        "PUJOL",
  "DE GRACIA",                         "GRACIA",
  "DE MALLORCA",                       "MALLORCA",
  "DE BADAL",                          "BADAL",
  "DE NAVARRA",                        "NAVARRA",
  "DE XILE",                           "XILE",
  "EMPEDRAT",                          "PEDRALBES",
  "AGUILO",                            "PUIG AGUILAR",
  "JOAN FERNANDEZ",                    "JOAN FERRANDIZ",
  "DE PAU CASALS",                     "PAU CASALS",
  "DE JOSEP TARRADELLAS",              "JOSEP TARRADELLAS",
  "DE LA RIERA DE CASSOLES",           "RIERA DE CASSOLES",
  "DEL MESTRE ANTONI NICOLAU",         "MESTRE NICOLAU",
  "COMTE D URGELL",                    "COMTE D URGELL",
  "COMTES DE BELL LLOC",               "COMTES DE BELL LLOC",
  "SABINO DE ARANA",                   "SABINO ARANA",
  "VALL D HEBRON",                     "VALL D HEBRON",
  "LA LLACUNA",                        "LLACUNA",
  "MARQUES DE MONT ROIG",              "MONT ROIG",
  "COMES",                             "COMAS",
  "PARC",                              "PARC",
  "APEL LES MESTRES",                  "APEL LES MESTRES",
  "CARAVEL LA NINA",                   "CARAVEL LA LA NINA",
  "GAL LA PLACIDIA",                   "GAL LA PLACIDIA",
  "PARAL LEL",                         "PARAL LEL",
  "PUIG REIG",                         "PUIG REIG",
  "FRANCESC PEREZ CABRERO",            "FRANCESC PEREZ CABRERO",
  "MARIA CUBI I SOLER",                "MARIA CUBI",
  "RIERA BLANCA",                      "BLANCA",
  "DE BOSCH I GIMPERA",                "BOSCH I GIMPERA",
  "DE CAN MARCET",                     "CAN MARCET",
  "DEL PARE MARIANA",                  "PARE MARIANA",
  "DOCTOR IBANEZ",                     "DOCTOR IBANEZ",
  "GARBI",                             "GARBI",
  "ABAT OLIBA",                        "ABAT OLIBA",
  "ADVOCAT MANUEL BALLBE",             "MANUEL BALLBE"
)%>%
  mutate(
    nom_carrer_user = normaliza_carrer(nom_carrer_user),
    nom_carrer_bcn = normaliza_carrer(nom_carrer_bcn)
  ) %>%
  distinct(nom_carrer_user, .keep_all = TRUE)

User_adreces <- User_adreces %>%
  left_join(
    equivalencias_carrers,
    by = c("nom_carrer" = "nom_carrer_user")
  ) %>%
  mutate(
    nom_carrer_join = coalesce(nom_carrer_bcn, nom_carrer)
  )

BCN_adreces_users <- User_adreces %>%
  left_join(
    BCN_adreces,
    by = c(
      "nom_carrer_join" = "nom_carrer",
      "USUA_NUMERO" = "num_i"),
    relationship = "many-to-many"
  )%>%
  distinct(ID, .keep_all = TRUE)

BCN_adreces_users <- BCN_adreces_users %>%
  filter(!is.na(x_etrs89) | !is.na(y_etrs89)) %>%
  filter(!USUA_CARRER %in% c("CONSELL DE CENT","CORTS CATALANES","QUATRE CAMINS","POMARET")) %>%
  filter(!ID %in% c("947","2053","498"))%>%
  arrange(ID)

BCN_adreces_users_SF <- BCN_adreces_users %>%
  st_as_sf(
    coords = c("x_etrs89", "y_etrs89"),
    crs = 25831,
    remove = FALSE
  )

saveRDS(
  BCN_adreces_users_SF,
  here("data", "processed", "adreces_SF.rds")
)

saveRDS(
  BCN_adreces_users_SF,
  here("data", "Tables_DB", "Adreces_SF_ID.rds")
)

## Renta media por hogar unidad censal data_frame

Renta_media<- read.csv2(here("data", "external", "renta_media_hogar.csv"),
                        stringsAsFactors = FALSE
)

Renta_media <- Renta_media %>%
  mutate(
    Seccion.Censal = str_pad(
      as.character(Seccion.Censal),
      width = 3,
      side = "left",
      pad = "0"
    ),
    Distrito = str_pad(
      as.character(Distrito),
      width = 2,
      side = "left",
      pad = "0"
    )
  )  

BCN_adreces_users__renda_SF <- BCN_adreces_users_SF  %>%
  inner_join(
    Renta_media[,4:5],
    by = c(
      "secc_cens" = "Seccion.Censal"
    )
  )

BCN_adreces_users__renda_SF <- BCN_adreces_users_SF %>%
  left_join(
    Renta_media,
    by = c(
      "districte" = "Distrito",
      "secc_cens" = "Seccion.Censal"
    )
  )

saveRDS(
  BCN_adreces_users__renda_SF,
  here("data", "processed", "adreces_SF_Renda.rds")
)

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
  transmute(
    ID = ID,
    codi_carrer = codi_carrer,
    nom_carrer = nom_carrer_join,
    numero_Carrer = USUA_NUMERO,
    secc_censal = secc_cens,
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

