  
# ============================================================
# 01.1_SF DATA DOWLOAD AND PREPARATION.R
# ============================================================ 

library(here)  
source(here("scripts", "00_setup.R"))
  
# Download Adreces ajuntament de Barcelona SF object
  
Adreces<- "https://opendata-ajuntament.barcelona.cat/data/dataset/25752522-3528-4c14-b68d-5f09a3e393bd/resource/661fe190-67c8-423a-b8eb-8140f547fde2/download"
  
  download.file(
    url = Adreces,
    destfile = "data/adreces.csv",
    mode = "wb"
  )

BCN_adreces<- st_read("data/adreces.csv")

# Download codis carrer de Barcelona

Codis_carrerer<- "https://opendata-ajuntament.barcelona.cat/data/dataset/d7802fd1-cdfb-4562-9148-d18722d7e2d8/resource/2b010e59-6952-4b27-9c4e-47fcaf64c916/download"
  
download.file(
    url = Codis_carrerer,
    destfile = "data/carrerer.csv",
    mode = "wb"
  )

Codis_carrerer<- st_read("data/carrerer.csv")
############ Merge carrerer amb graf edificis pero obtenir codi de tipus de vial #########

BCN_adreces_rc<-BCN_adreces%>%
  
  left_join(Codis_carrerer[,c(1,3,4,5)],
            by=c("codi_carrer"="codi_via"))%>%
 
  mutate(Seccio_Censal = paste0(
                                as.integer(districte),
                                str_pad(as.character(secc_cens), width = 3, pad = "0")),
            
          llepost_i=trimws(llepost_i),
          llepost_i = na_if(llepost_i,""),
          codi_parc=trimws(codi_parc),
          codi_parc = na_if(codi_parc,""))%>%
  
  filter(
    #barri %in% c("27", "07","08", "09", "20", "21", "19", "24", "25", "26", "17"),
    #     districte %in% c("05", "02", "04"),
         !is.na(codi_parc)) %>%
  
  group_by(tipus_via, nom_carrer, numpost_i) %>%
  arrange(is.na(llepost_i), .by_group = TRUE) %>%
  filter(
    n() == 1 |                    # única fila → conservar
      !is.na(llepost_i) |           # varias → solo con letra
      row_number() == 1             # todas vacías → la primera
  ) %>%
  ungroup()

BCN_adreces_rc <- BCN_adreces_rc %>%
  distinct(tipus_via, nom_carrer, numpost_i, .keep_all = TRUE)


### Problema amb UTS sense coordanades (borro) i alguns canvis inicials de majuscules
  
  # Limpieza inicial:
  # - eliminar registros sin coordenadas
  # - normalizar nombre de calle
  # - crear USUA_NUMERO a partir de numpost_i

BCN_adreces_rc <- BCN_adreces_rc %>%
     mutate(
       
  # Nombre de calle normalizado
      nom_carrer = normaliza_carrer(nom_carrer),
      numpost_i  = as.integer(numpost_i),
  
  # Rango de numeración del portal
      numpost_i = as.integer(str_remove(as.character(numpost_i), "^0+")),
      numpost_f = as.integer(str_remove(as.character(numpost_f), "^0+")),
  
  #Clave
     Clave = paste(tipus_via,nom_carrer,numpost_i,sep = "|"))%>%
  
    filter(
      !is.na(nom_carrer),
      nom_carrer != "",
      !is.na(numpost_i),
      !is.na(x_etrs89),
      !is.na(y_etrs89)
      ) %>%
    select(
      codi_carrer,
      tipus_via,
      Clave,
      codi_parc,
      nom_carrer,
      nom_curt,
      nom_oficial,
      llepost_i,
      llepost_f,
      numpost_i,
      numpost_f,
      dist_post,
      districte,
      nom_districte,
      barri,
      nom_barri,
      Seccio_Censal,
      x_etrs89,
      y_etrs89,
      longitud_wgs84,
      latitud_wgs84
    ) 

# Resumen no hay duplicados por calle y numero 
BCN_adreces_rc %>%
  count(Clave, sort = TRUE) %>%
  filter(n > 1)

BCN_adreces_rc %>%
  count(longitud_wgs84, sort = TRUE) %>%
  filter(is.na(n))

####### Carrers pacients #########
  
User_adreces<- readRDS(here("data", "Starting", "ADRECES_FINAL_inicial.rds"))

##### Transformació tipus de vial ######

User_adreces <- User_adreces %>%
  filter(Residencia=="NO")%>%
    mutate(
      nom_carrer = normaliza_carrer(nom_carrer),
      numpost_i = as.integer(numpost_i),
      tipus_via = case_when(
        USUA_TIPUS_DE_VIAL == "CR" ~ "C",
        USUA_TIPUS_DE_VIAL == "AV" ~ "Av",
        USUA_TIPUS_DE_VIAL == "TS" ~ "Trav",
        USUA_TIPUS_DE_VIAL == "PL" ~ "Pl",
        USUA_TIPUS_DE_VIAL == "RI" ~ "Rier",
        USUA_TIPUS_DE_VIAL == "PG" ~ "Ptge",
        USUA_TIPUS_DE_VIAL == "BX" ~ "Bda",
        USUA_TIPUS_DE_VIAL == "PS" ~ "Pg",
        USUA_TIPUS_DE_VIAL == "VI" ~ "Via",
        USUA_TIPUS_DE_VIAL == "RD" ~ "Rda",
        USUA_TIPUS_DE_VIAL == "GV" ~ "G.V.",
        USUA_TIPUS_DE_VIAL == "RB" ~ "Rbla",
      )
    )

######## Problemes de codificació dels carrers en DF EHR hacer igual que la denominación original. #######

User_adreces <- User_adreces %>%
  mutate(
    # Normalizar el nombre original
    nom_carrer_norm = normaliza_carrer(nom_carrer),
    
    # Aplicar equivalencias
    nom_carrer_join = case_when(
      # Quitar "DE" o artículos
      nom_carrer_norm == normaliza_carrer("AGUSTINA DE SARAGOSSA") ~ "AGUSTINA SARAGOSSA",
      nom_carrer_norm == normaliza_carrer("DE CARLES III") ~ "CARLES III",
      nom_carrer_norm == normaliza_carrer("DE LES CORTS") ~ "CORTS",
      nom_carrer_norm == normaliza_carrer("DE GRACIA") ~ "GRACIA",
      nom_carrer_norm == normaliza_carrer("DE MALLORCA") ~ "MALLORCA",
      nom_carrer_norm == normaliza_carrer("DE XILE") ~ "XILE",
      nom_carrer_norm == normaliza_carrer("DE PAU CASALS") ~ "PAU CASALS",
      nom_carrer_norm == normaliza_carrer("DE JOSEP TARRADELLAS") ~ "JOSEP TARRADELLAS",
      nom_carrer_norm == normaliza_carrer("DE LA RIERA DE CASSOLES") ~ "RIERA DE CASSOLES",
      nom_carrer_norm == normaliza_carrer("DE BOSCH I GIMPERA") ~ "BOSCH I GIMPERA",
      nom_carrer_norm == normaliza_carrer("DE CAN MARCET") ~ "CAN MARCET",
      nom_carrer_norm == normaliza_carrer("DEL PARE MARIANA") ~ "PARE MARIANA",
      nom_carrer_norm == normaliza_carrer("DEL MESTRE ANTONI NICOLAU") ~ "MESTRE NICOLAU",
      nom_carrer_norm == normaliza_carrer("D'EN PUJOL") ~ "PUJOL",
      nom_carrer_norm == normaliza_carrer("SABINO DE ARANA") ~ "SABINO ARANA",
      nom_carrer_norm == normaliza_carrer("LA LLACUNA") ~ "LLACUNA",
      nom_carrer_norm == normaliza_carrer("LA TORRE") ~ "TORRE",
      nom_carrer_norm == normaliza_carrer("LA ALFAMBRA") ~ "ALFAMBRA",
      nom_carrer_norm == normaliza_carrer("RIERA BLANCA") ~ "BLANCA",
      nom_carrer_norm == normaliza_carrer("DELS SEGADORS") ~ "SEGADORS",
      nom_carrer_norm == normaliza_carrer("MARQUES DE MONT ROIG") ~ "MONT ROIG",
      nom_carrer_norm == normaliza_carrer("ADVOCAT MANUEL BALLBE") ~ "MANUEL BALLBE",
      
    # Correcciones ortográficas
      nom_carrer_norm == normaliza_carrer("BERTRAN I ROZPIDE") ~ "BELTRAN I ROZPIDE",
      nom_carrer_norm == normaliza_carrer("JOAN SEBASTIA BACH") ~ "JOHANN SEBASTIAN BACH",
      nom_carrer_norm == normaliza_carrer("SOR EULALIA ANZIZU") ~ "SOR EULALIA D ANZIZU",
      nom_carrer_norm == normaliza_carrer("PRESIDENT JOSEP IRLA I BOSCH") ~ "JOSEP IRLA I BOSCH",
      nom_carrer_norm == normaliza_carrer("RICARD CALVO") ~ "RICARDO CALVO",
      nom_carrer_norm == normaliza_carrer("COMES") ~ "COMAS",
      nom_carrer_norm == normaliza_carrer("SANT GENIS") ~ "SANT GENIS A HORTA",
      nom_carrer_norm == normaliza_carrer("MESTRE ANTONI NICOLAU") ~ "MESTRE NICOLAU",
      
    # Añadir palabras faltantes
      nom_carrer_norm == normaliza_carrer("CIUTAT BALAGUER") ~ "CIUTAT DE BALAGUER",
      nom_carrer_norm == normaliza_carrer("COMTE SALVATIERRA") ~ "COMTE DE SALVATIERRA",
      nom_carrer_norm == normaliza_carrer("DOMINGUEZ MIRALLES") ~ "DOMINGUEZ I MIRALLES",
      nom_carrer_norm == normaliza_carrer("CARAVEL LA NINA") ~ "CARAVEL LA LA NINA",
      nom_carrer_norm == normaliza_carrer("MARIA CUBI I SOLER") ~ "MARIA CUBI",
      
    # Si no coincide con nada, dejar el valor original
      TRUE ~ nom_carrer_norm
    ),
    tipus_via = case_when(
      nom_carrer_join == "MANUEL GIRONA" & tipus_via == "C"  ~  "Pg",
      TRUE ~ tipus_via),
    
    Clave = paste(
      tipus_via,nom_carrer_join,numpost_i,sep = "|")
  )%>%
  select(-nom_carrer_norm)   

# Join SF carrers de BCN amb adreces users. Identificacions postals.

BCN_adreces_users <- User_adreces %>%
  left_join(
    BCN_adreces_rc %>% select(-tipus_via, -nom_carrer, -numpost_i),
    by = "Clave"
  ) %>%
  distinct(USUA_CIP_RCA, .keep_all = TRUE) %>%
  filter(!is.na(x_etrs89) | !is.na(y_etrs89))

BCN_adreces_users_SF <- st_as_sf(
  BCN_adreces_users,
  coords = c("x_etrs89", "y_etrs89"),
  crs    = 25831,
  remove = FALSE)

BCN_adreces_users_SF<-st_make_valid(BCN_adreces_users_SF)

#Perímetre de les ABS dels centres participants.
  
ABS_sf<-st_read(
    here("data", "external", "cartografia_centres", "ABS.shp"),
    quiet = TRUE
  )

ABS_sel <- ABS_sf %>%
  filter(NOMABS %in% c("Barcelona - 04A","Barcelona - 04B","Barcelona - 04C","Barcelona - 05B","Barcelona - 05A","Barcelona - 02C","Barcelona - 02E"))

ABS_sel <- st_make_valid(ABS_sel)

##Nova columna inclusion

BCN_adreces_users_SF <- BCN_adreces_users_SF %>%
  st_join(
    ABS_sel %>% select(CODABSa, NOMABS),
    join = st_intersects,
    left = TRUE
  ) %>%
  mutate(
    included = if_else(is.na(CODABSa), "no included", "included")
  ) %>%
  distinct(USUA_CIP_RCA, .keep_all = TRUE)

nodes <- st_read(here("data", "external", "BCN_GrafVial_SHP", "BCN_GrafVial_Nodes_ETRS89_SHP.shp"),
                 quiet = TRUE)


trams <- st_read(
  here("data", "external", "BCN_GrafVial_SHP", "BCN_GrafVial_Trams_ETRS89_SHP.shp"),
  quiet = TRUE)

trams_sel <- trams %>%
  filter(Distric_E %in% c("05","02","04"))

nodes_sel <- nodes[st_intersects(nodes, trams_sel, sparse = FALSE) |> apply(1, any), ]

# 1. Primero los polígonos (fondo), con transparencia

library("mapview")
library("leaflet")
library("htmlwidgets")

sep_metros <- 8 

BCN_adreces_users_SF <- BCN_adreces_users_SF %>%
  mutate(
    x = st_coordinates(.)[, 1],
    y = st_coordinates(.)[, 2]
  ) %>%
  group_by(x, y) %>%
  mutate(
    n_grupo = n(),
    idx     = row_number(),
    # Centro de la línea: los puntos se distribuyen simétricamente
    # respecto a la coordenada original
    offset  = (idx - (n_grupo + 1) / 2) * sep_metros,
    # Si el grupo tiene 1 solo punto, offset = 0 → queda en su sitio
    offset  = if_else(n_grupo == 1, 0, offset),
    # Desplazamiento horizontal
    x_vis   = x + offset,
    y_vis   = y
  ) %>%
  ungroup() %>%
  st_drop_geometry() %>%
  st_as_sf(coords = c("x_vis", "y_vis"), crs = 25831, remove = FALSE)

##primer mapa que se visualiza

mapview(ABS_sel, col.regions = "lightblue", alpha.regions = 0.3, 
        legend = FALSE, layer.name = "ABS") +
  mapview(trams, color = "black", lwd = 2, legend = FALSE, layer.name = "Trams") +
  mapview(BCN_adreces_users_SF, 
          zcol = "included",
          col.regions = c("included" = "red", "no included" = "black"),
          cex = 4,
          label = BCN_adreces_users_SF$USUA_CIP,
          layer.name = "Usuarios")

plot(st_geometry(ABS_sel),
     col = adjustcolor("lightblue", alpha.f = 0.3),
     border = "blue",
     main = "Puntos incluidos en ABS")

# 2. Luego las líneas
plot(st_geometry(trams),
     col = "black",
     add = TRUE)

# 3. Finalmente los puntos encima

plot(st_geometry(BCN_adreces_users_SF),
     col = ifelse(BCN_adreces_users_SF$included == "included", "red", "black"),
     pch = 20,
     cex = 0.7,
     add = TRUE)


BCN_adreces_users <- BCN_adreces_users %>%
    filter(!is.na(x_etrs89) | !is.na(y_etrs89)) %>%
    filter(!nom_carrer %in% c("CONSELL DE CENT","CORTS CATALANES","QUATRE CAMINS","POMARET"))
  
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

BCN_included_df <- BCN_adreces_users_SF %>%
  filter(included == "included") %>%
  st_drop_geometry() %>%
  select(
    ID, USUA_CIP, USUA_CIP_RCA,
    USUA_SITUACIO, SITUACIO_2026,
    USUA_TIPUS_DE_VIAL, nom_carrer, numpost_i,
    USUA_ESCALA, USUA_PORTAL, USUA_PORTA,
    USUA_CODI_POSTAL, USUA_NOM_LOCALITAT, USUA_UP_RESID,
    USUA_DATA_DEFUNCIO, Residencia,
    tipus_via, nom_carrer_join, Clave,
    codi_carrer, codi_parc, nom_curt, nom_oficial,
    llepost_i, llepost_f, numpost_f, dist_post,
    districte, nom_districte, barri, nom_barri,
    Seccio_Censal,
    x_etrs89, y_etrs89, longitud_wgs84, latitud_wgs84,
    CODABSa, NOMABS,
    included
  )

saveRDS(BCN_included_df,here("data", "Starting", "Pacients_en_zona_final.rds"))
  
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
  
