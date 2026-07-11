
# ============================================================
# 05_SF DATA DOWLOAD AND PREPARATION.R
# ============================================================

source(here("scripts", "00_setup.R"))

locations<-readRDS(here("data","processed","adreces_SF.rds"))

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
# - eliminar registros sin coordenadas
# - convertir coordenadas a numéricas
# - normalizar nombre de calle
# - crear USUA_NUMERO a partir de numpost_i

BCN_adreces <- BCN_adreces %>%
  st_drop_geometry() %>%
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
    x_etrs89 = mean(x_etrs89, na.rm = TRUE),
    y_etrs89 = mean(y_etrs89, na.rm = TRUE),
    longitud_wgs84 = mean(longitud_wgs84, na.rm = TRUE),
    latitud_wgs84 = mean(latitud_wgs84, na.rm = TRUE),
    n_adreces_originals = .N
  ),
  by = .(nom_carrer, num_i, num_f)
]

BCN_adreces<- st_as_sf(
  BCN_adreces,
  coords = c("x_etrs89", "y_etrs89"),
  crs = crs_bcn,
  remove = FALSE
)

## Carrers pacients

User_adreces<- read.csv2(here("data", "external", "USER_adreces_original.csv"),
                         stringsAsFactors = FALSE
)

User_adreces <- User_adreces %>%
  mutate(
    nom_carrer = normaliza_carrer(USUA_CARRER),
    USUA_NUMERO = as.numeric(USUA_NUMERO)
  )

### Problemes de codificació dels carrers en DF EHR hacer igual que la denominación original.

equivalencias_carrers <- tribble(
  ~nom_carrer_user,                    ~nom_carrer_bcn,
  
  "CADIS","CADIS",
  "AGUSTINA DE SARAGOSSA",              "AGUSTINA SARAGOSSA",
  "BERTRAN I ROZPIDE",  "BELTRAN I ROZPIDE",
  "JOAN SEBASTIA BACH" , "JOHANN SEBASTIAN BACH",
  "DE CARLES III" , "CARLES III",
  "DE LES CORTS" , "CORTS",
  "SOR EULALIA ANZIZU" , "SOR EULALIA D ANZIZU",
  "PRESIDENT JOSEP IRLA I BOSCH" , "JOSEP IRLA I BOSCH",
  "RICARD CALVO" , "RICARDO CALVO",
  "DELS SEGADORS" , "SEGADORS",
  "SANT GENIS" , "SANT GENIS A HORTA",
  "DE GRACIA" , "GRACIA",
  "MESTRE ANTONI NICOLAU" , "MESTRE NICOLAU",
  "LA TORRE" , "TORRE",
  "LA ALFAMBRA" , "ALFAMBRA",
  "ALFONS I",   "ALFONS XII",
  "CIUTAT BALAGUER", "CIUTAT DE BALAGUER",
  "COMTE SALVATIERRA" , "COMTE DE SALVATIERRA",
  "DOMINGUEZ MIRALLES" , "DOMINGUEZ I MIRALLES",
  "RIERA BLANCA", "BLANCA",
  "D'EN PUJOL" , "PUJOL",
  "DE GRACIA",                          "PASSEIG DE GRACIA",
  "DE MALLORCA",                        "MALLORCA",
  "DE BADAL",                           "BADAL",
  "DE NAVARRA",                         "NAVARRA",
  "DE XILE",                            "XILE",
  "EMPEDRAT",                           "PEDRALBES",
  "AGUILO",                             "PUIG AGUILAR",
  "JOAN FERNANDEZ",	                    "JOAN FERRANDIZ",
  "DE PAU CASALS",                      "PAU CASALS",
  "DE JOSEP TARRADELLAS",               "JOSEP TARRADELLAS",
  "DE LA RIERA DE CASSOLES",            "RIERA DE CASSOLES",
  "DE CARLES III",                      "GRAN VIA DE CARLES III",
  "DE LES CORTS",                       "TRAVESSERA DE LES CORTS",
  "DEL MESTRE ANTONI NICOLAU",          "MESTRE NICOLAU",
  "DELS SEGADORS",                      "SEGADORS",
  "COMTE D URGELL",                     "COMTE D URGELL",
  "COMTES DE BELL LLOC",                "COMTES DE BELL LLOC",
  "SABINO DE ARANA",                    "SABINO ARANA",
  "VALL D HEBRON",                      "VALL D HEBRON",
  "LA LLACUNA",                         "LLACUNA",
  "MARQUES DE MONT ROIG",               "MONT ROIG",
  "COMES" ,                             "COMAS",
  "PARC",                               "PARC",
  "APEL LES MESTRES",                   "APEL LES MESTRES",
  "CARAVEL LA NINA",                    "CARAVEL LA LA NINA",
  "GAL LA PLACIDIA",                    "GAL LA PLACIDIA",
  "PARAL LEL",                          "PARAL LEL",
  "MARQUES DE MONT ROIG",               "MARQUES DE MONT ROIG",
  "PUIG REIG",                          "PUIG REIG",
  "FRANCESC PEREZ CABRERO",             "FRANCESC PEREZ CABRERO",
  "MARIA CUBI I SOLER",                 "MARIA CUBI",
  "JOAN SEBASTIA BACH",                 "JOAN SEBASTIA BACH",
  "SOR EULALIA ANZIZU",                 "SOR EULALIA ANZIZU",
  "RICARD CALVO",                       "RICARD CALVO",
  "RIERA BLANCA",                       "RIERA BLANCA",
  "SANT GENIS",                         "SANT GENIS",
  "D EN PUJOL",                         "EN PUJOL",
  "DE BOSCH I GIMPERA",                 "BOSCH I GIMPERA",
  "DE CAN MARCET",                      "CAN MARCET",
  "DEL PARE MARIANA",                   "PARE MARIANA",
  "DOCTOR IBANEZ",                      "DOCTOR IBANEZ",
  "DOMINGUEZ MIRALLES",                 "DOMINGUEZ MIRALLES",
  "GARBI",                              "GARBI",
  "ABAT OLIBA",                         "ABAT OLIBA",
  "ADVOCAT MANUEL BALLBE",              "MANUEL BALLBE",
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

User_adreces$USUA_NUMERO[User_adreces$ID=="1085"]<-25 
User_adreces$USUA_NUMERO[User_adreces$ID=="2024"]<-1
User_adreces$USUA_NUMERO[User_adreces$ID=="1085"]<-17
User_adreces$USUA_NUMERO[User_adreces$ID=="770"]<-4
User_adreces$USUA_NUMERO[User_adreces$ID=="340"]<-17
User_adreces$USUA_NUMERO[User_adreces$ID=="402"]<-25
User_adreces$USUA_NUMERO[User_adreces$ID=="1307"]<-2
User_adreces$USUA_NUMERO[User_adreces$ID=="504"]<-2

BCN_adreces_users <- User_adreces %>%
  left_join(
    BCN_adreces,
    by = c(
      "nom_carrer_join" = "nom_carrer",
      "USUA_NUMERO" = "num_i"
    )
  )

BCN_adreces_users <- BCN_adreces_users %>%
  filter(!is.na(x_etrs89) | !is.na(y_etrs89)) %>%
  filter(barri %in% c("27","08","09","20","21","19","24","25","26","17"),
         !ID %in% c("1009","1553","35","1054","1361","1737","1857",
                    "1575","1624","1868","304","1987","544","798","1393","1418","1841","922","1130","2024","2053","770","1740","504","1307")) %>%
  distinct(ID, USUA_CARRER, USUA_NUMERO, nom_carrer, nom_carrer_bcn, nom_carrer_join, .keep_all = TRUE) %>%
  arrange(nom_carrer_join, USUA_NUMERO)

BCN_adreces_users_SF <- BCN_adreces_users %>%
  filter(!is.na(x_etrs89), !is.na(y_etrs89)) %>%
  st_as_sf(
    coords = c("x_etrs89", "y_etrs89"),
    crs = 25831,
    remove = FALSE
  )

id<-BCN_adreces_users_SF[,1]

saveRDS(
  BCN_adreces_users_SF,
  here("data", "processed", "adreces_SF.rds")
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

Center_location <- Centres_adreces_sf %>%
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

